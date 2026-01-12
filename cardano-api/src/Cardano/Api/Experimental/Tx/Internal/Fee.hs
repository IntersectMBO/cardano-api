{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Experimental.Tx.Internal.Fee
  ( TxBodyErrorAutoBalance (..)
  , TxFeeEstimationError (..)
  , collectTxBodyScriptWitnesses
  , estimateBalancedTxBody
  , evaluateTransactionExecutionUnits
  , evaluateTransactionFee
  , indexWitnessedTxProposalProcedures
  , makeTransactionBodyAutoBalance
  -- Internal
  , toUnsigned
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Error
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.BodyContent.New
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.Type
import Cardano.Api.Key.Internal qualified as Api
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal
import Cardano.Api.Plutus.Internal.Script (fromAlonzoExUnits)
import Cardano.Api.Plutus.Internal.Script qualified as Old
import Cardano.Api.Plutus.Internal.ScriptData
import Cardano.Api.Pretty
import Cardano.Api.ProtocolParameters
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.Body
  ( CtxTx
  , ScriptWitnessIndex (..)
  , renderScriptWitnessIndex
  , toScriptIndex
  )
import Cardano.Api.Tx.Internal.Fee
  ( EvalTxExecutionUnitsLog
  , ResolvablePointers (..)
  , ScriptExecutionError (..)
  , extractScriptBytesAndLanguage
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxIn
import Cardano.Api.Value.Internal

import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.Credential as Ledger (Credential)
import Cardano.Ledger.Val qualified as L

import Control.Monad
import Data.Bifunctor
import Data.Function (on, (&))
import Data.List (sortBy)
import Data.List qualified as List
import Data.Map.Ordered ()
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OSet.Strict qualified as OSet
import Data.Ord (Down (Down), comparing)
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro ((.~), (^.))
import Prettyprinter (punctuate)

data TxBodyErrorAutoBalance era
  = -- | There is not enough ada and non-ada to cover both the outputs and the fees.
    -- The transaction should be changed to provide more input assets, or
    -- otherwise adjusted to need less (e.g. outputs, script etc).
    TxBodyErrorBalanceNegative L.Coin L.MultiAsset
  | -- | There is enough ada to cover both the outputs and the fees, but the
    -- resulting change is too small: it is under the minimum value for
    -- new UTXO entries. The transaction should be changed to provide more
    -- input ada.
    TxBodyErrorAdaBalanceTooSmall
      (TxOut CtxTx era)
      -- ^ Offending TxOut
      L.Coin
      -- ^ Minimum UTxO
      L.Coin
      -- ^ Tx balance
  | -- | The minimum spendable UTxO threshold has not been met.
    TxBodyErrorMinUTxONotMet
      (TxOut CtxTx era)
      -- ^ Offending TxOut
      L.Coin
      -- ^ Minimum UTXO
  | TxBodyErrorNonAdaAssetsUnbalanced Value
  | TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap
      ScriptWitnessIndex
      (Map ScriptWitnessIndex ExecutionUnits)
  | -- | One or more scripts failed to execute correctly.
    TxBodyScriptExecutionError [(ScriptWitnessIndex, ScriptExecutionError)]
  | -- | One or more scripts were expected to fail validation, but none did.
    TxBodyScriptBadScriptValidity
  | BalanceIsNegative
      L.Coin
      -- ^ Negative balance
      (UnsignedTx ConwayEra)
      -- ^ The transaction body
  | NotEnoughAdaInUTxO
      L.MaryValue
      -- ^ Total UTxO value
      L.Coin
      -- ^ Total deposits
      L.MaryValue
      -- ^ Balance

deriving instance Show (TxBodyErrorAutoBalance era)

instance Error (TxBodyErrorAutoBalance era) where
  prettyError = \case
    TxBodyScriptExecutionError failures ->
      mconcat
        [ "The following scripts have execution failures:\n"
        , vsep
            [ mconcat
                [ "the script for " <> pretty (renderScriptWitnessIndex index)
                , " failed with: " <> "\n" <> prettyError failure
                ]
            | (index, failure) <- failures
            ]
        ]
    TxBodyScriptBadScriptValidity ->
      "One or more of the scripts were expected to fail validation, but none did."
    TxBodyErrorBalanceNegative lovelace assets ->
      mconcat $
        [ "The transaction does not balance in its use of assets. The net balance "
        , "of the transaction is negative: "
        ]
          <> punctuate ", " ([pretty lovelace] <> [pretty assets | assets /= mempty])
          <> [ ". The usual solution is to provide more inputs, or inputs with more assets."
             ]
    TxBodyErrorAdaBalanceTooSmall changeOutput minUTxO balance ->
      mconcat
        [ "The transaction does balance in its use of ada, however the net "
        , "balance does not meet the minimum UTxO threshold. \n"
        , "Balance: " <> pretty balance <> "\n"
        , "Offending output (change output): " <> pretty (show changeOutput) <> "\n"
        , "Minimum UTxO threshold: " <> pretty minUTxO <> "\n"
        , "The usual solution is to provide more inputs, or inputs with more ada to "
        , "meet the minimum UTxO threshold."
        ]
    TxBodyErrorMinUTxONotMet txout minUTxO ->
      mconcat
        [ "Minimum UTxO threshold not met for tx output: " <> pretty (show txout) <> "\n"
        , "Minimum required UTxO: " <> pretty minUTxO
        ]
    TxBodyErrorNonAdaAssetsUnbalanced val ->
      "Non-Ada assets are unbalanced: " <> pretty (renderValue val)
    TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap sIndex eUnitsMap ->
      mconcat
        [ "ScriptWitnessIndex (redeemer pointer): " <> pshow sIndex <> " is missing from the execution "
        , "units (redeemer pointer) map: " <> pshow eUnitsMap
        ]
    BalanceIsNegative negBalance txbody ->
      mconcat
        [ "The transaction balance is negative: "
        , pretty negBalance
        , "\nTransaction body: "
        , pshow txbody
        ]
    NotEnoughAdaInUTxO totalUTxOValue totalDeposits balance ->
      mconcat
        [ "The total ada in the provided UTxO(s) is not enough to cover the deposits required by the "
        , "transaction.\nTotal UTxO value: "
        , pshow totalUTxOValue
        , "\nTotal deposits: "
        , pretty totalDeposits
        , "\nBalance (UTxO value - deposits): "
        , pshow balance
        ]

-- | Use when you do not have access to the UTxOs you intend to spend
estimateBalancedTxBody
  :: HasCallStack
  => Era era
  -> TxBodyContent (LedgerEra era)
  -> L.PParams (LedgerEra era)
  -> Set PoolId
  -- ^ The set of registered stake pools, being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ A map of all deposits for stake credentials that are being
  --   unregistered in this transaction.
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -- ^ A map of all deposits for DRep credentials that are being
  --   unregistered in this transaction.
  -> Map (Ledger.PlutusPurpose Ledger.AsIx (LedgerEra era)) ExecutionUnits
  -- ^ Plutus script execution units.
  -> Coin
  -- ^ Total potential collateral amount.
  -> Int
  -- ^ The number of key witnesses to be added to the transaction.
  -> Int
  -- ^ The number of Byron key witnesses to be added to the transaction.
  -> Int
  -- ^ The size of all reference scripts in bytes.
  -> AddressInEra era
  -- ^ Change address.
  -> L.Value (LedgerEra era)
  -- ^ Total value of UTXOs being spent.
  -> Either (TxFeeEstimationError era) (TxBodyContent (LedgerEra era))
estimateBalancedTxBody
  w
  txbodycontent
  pparams
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  exUnitsMap =
    obtainCommonConstraints w $
      estimateBalancedTxBody'
        txbodycontent
        pparams
        poolids
        stakeDelegDeposits
        drepDelegDeposits
        (Map.mapKeys (toScriptIndex (convert w)) exUnitsMap)

data TxFeeEstimationError era
  = TxFeeEstimationScriptExecutionError (TxBodyErrorAutoBalance (LedgerEra era))
  | TxFeeEstimationBalanceError (TxBodyErrorAutoBalance (LedgerEra era))
  deriving Show

instance Error (TxFeeEstimationError era) where
  prettyError = \case
    TxFeeEstimationScriptExecutionError e -> prettyError e
    TxFeeEstimationBalanceError e -> prettyError e

-- | Use when you do not have access to the UTxOs you intend to spend
estimateBalancedTxBody'
  :: forall era
   . HasCallStack
  => IsEra era
  => TxBodyContent (LedgerEra era)
  -> L.PParams (LedgerEra era)
  -> Set PoolId
  -- ^ The set of registered stake pools, being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ A map of all deposits for stake credentials that are being
  --   unregistered in this transaction.
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -- ^ A map of all deposits for DRep credentials that are being
  --   unregistered in this transaction.
  -> Map ScriptWitnessIndex ExecutionUnits
  -- ^ Plutus script execution units.
  -> Coin
  -- ^ Total potential collateral amount.
  -> Int
  -- ^ The number of key witnesses to be added to the transaction.
  -> Int
  -- ^ The number of Byron key witnesses to be added to the transaction.
  -> Int
  -- ^ The size of all reference scripts in bytes.
  -> AddressInEra era
  -- ^ Change address.
  -> L.MaryValue
  -- ^ Total value of UTXOs being spent.
  -> Either (TxFeeEstimationError era) (TxBodyContent (LedgerEra era))
estimateBalancedTxBody'
  txbodycontent
  pparams
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  exUnitsMap
  totalPotentialCollateral
  intendedKeyWits
  byronwits
  sizeOfAllReferenceScripts
  changeaddr
  totalUTxOValue = do
    -- Step 1. Substitute those execution units into the tx

    txbodycontent1 <-
      first TxFeeEstimationScriptExecutionError $
        substituteExecutionUnits exUnitsMap txbodycontent

    -- Step 2. We need to calculate the current balance of the tx. The user
    -- must at least provide the total value of the UTxOs they intend to spend
    -- for us to calulate the balance. NB: We must:
    --  1. Subtract certificate and proposal deposits
    -- from the total available Ada value!
    -- Page 24 Shelley ledger spec
    let certificates :: [L.TxCert (LedgerEra era)] =
          [ cert
          | (Exp.Certificate cert, _) <- toList . unTxCertificates $ txCertificates txbodycontent1
          ]

        proposalProcedures :: OSet.OSet (L.ProposalProcedure (LedgerEra era))
        proposalProcedures =
          convProposalProcedures $ txProposalProcedures txbodycontent1

        totalDeposits :: L.Coin
        totalDeposits =
          -- Because we do not have access to the ledger state and to reduce the complexity of this function's
          -- type signature, we assume the user is trying to register a stake pool that has not been
          -- registered before and has not included duplicate stake pool registration certificates.
          let assumeStakePoolHasNotBeenRegistered = const False
           in sum
                [ obtainCommonConstraints (useEra @era) $
                    L.getTotalDepositsTxCerts pparams assumeStakePoolHasNotBeenRegistered certificates
                , obtainCommonConstraints (useEra @era) $
                    mconcat $
                      map (^. L.pProcDepositL) $
                        toList proposalProcedures
                ]
        availableUTxOValue :: L.MaryValue
        availableUTxOValue = totalUTxOValue L.<-> L.inject totalDeposits

    when (L.coin availableUTxOValue < 0) $
      Left $
        TxFeeEstimationBalanceError $
          NotEnoughAdaInUTxO
            totalUTxOValue
            totalDeposits
            availableUTxOValue
    let
      partialChange =
        calculatePartialChangeValue availableUTxOValue txbodycontent1
      maxLovelaceChange = L.Coin (2 ^ (64 :: Integer)) - 1
      changeWithMaxLovelace = L.modifyCoin (const maxLovelaceChange) partialChange
      changeTxOut :: L.TxOut (LedgerEra era)
      changeTxOut =
        obtainCommonConstraints (useEra @era) $
          L.mkBasicTxOut (toShelleyAddr changeaddr) changeWithMaxLovelace

    let mDummyCollateral = maybeDummyTotalCollAndCollReturnOutput txbodycontent changeaddr

    -- Step 3. Create a tx body with out max lovelace fee. This is strictly for
    -- calculating our fee with evaluateTransactionFee.
    let maxLovelaceFee = L.Coin (2 ^ (32 :: Integer) - 1)
    let txbody1ForFeeEstimateOnly =
          makeUnsignedTx
            useEra
            txbodycontent1
              { txFee = maxLovelaceFee
              , txOuts =
                  obtainCommonConstraints (useEra @era) (TxOut changeTxOut Nothing)
                    : txOuts txbodycontent
              , txCollateral = mDummyCollateral
              }
    let fee =
          evaluateTransactionFee
            pparams
            txbody1ForFeeEstimateOnly
            (fromIntegral intendedKeyWits)
            (fromIntegral byronwits)
            sizeOfAllReferenceScripts

        -- Step 4. We use the fee to calculate the required collateral
        maybeTxCollateral =
          obtainCommonConstraints (useEra @era) $
            calcReturnAndTotalCollateral
              fee
              pparams
              (txInsCollateral txbodycontent)
              (txCollateral txbodycontent)
              changeaddr
              (L.inject totalPotentialCollateral)

    -- Step 5. Now we can calculate the balance of the tx. What matter here are:
    --  1. The original outputs
    --  2. Tx fee
    --  3. Return and total collateral
    let
      txbody2 =
        makeUnsignedTx
          useEra
          txbodycontent1
            { txFee = fee
            , txCollateral = maybeTxCollateral
            }

    let fakeUTxO = createFakeUTxO txbodycontent1 $ L.coin availableUTxOValue
        balance :: Ledger.Value (LedgerEra era) =
          evaluateTransactionBalance pparams poolids stakeDelegDeposits drepDelegDeposits fakeUTxO txbody2

        coinBalance :: L.Coin
        coinBalance = obtainCommonConstraints (useEra @era) $ L.coin balance

        balanceTxOut :: TxOut CtxTx (LedgerEra era)
        balanceTxOut =
          obtainCommonConstraints (useEra @era) $
            TxOut (L.mkBasicTxOut (toShelleyAddr changeaddr) balance) Nothing
    case useEra @era of
      DijkstraEra -> error "estimateBalancedTxBody: DijkstraEra is not supported for fee estimation"
      ConwayEra -> do
        when (coinBalance < 0) $
          Left $
            TxFeeEstimationBalanceError $
              BalanceIsNegative coinBalance txbody2

        -- Step 6. Check all txouts have the min required UTxO value
        -- TOOD: Fix me. You need a new error type to accomodate your new types
        first (TxFeeEstimationBalanceError . uncurry TxBodyErrorMinUTxONotMet)
          . mapM_ (checkMinUTxOValue pparams)
          $ txOuts txbodycontent1

        -- check if the balance is positive or negative
        -- in one case we can produce change, in the other the inputs are insufficient
        finalTxOuts <-
          first TxFeeEstimationBalanceError $
            checkAndIncludeChange pparams balanceTxOut (txOuts txbodycontent1)

        -- Step 7.

        -- Create the txbody with the final fee and change output. This should work
        -- provided that the fee and change are less than 2^32-1, and so will
        -- fit within the encoding size we picked above when calculating the fee.
        -- Yes this could be an over-estimate by a few bytes if the fee or change
        -- would fit within 2^16-1. That's a possible optimisation.
        let finalTxBodyContent =
              txbodycontent1
                { txFee = fee
                , txOuts = finalTxOuts
                , txCollateral = maybeTxCollateral
                }

        return finalTxBodyContent

data IsEmpty = Empty | NonEmpty
  deriving (Eq, Show)

checkNonNegative
  :: forall era
   . IsEra era
  => Ledger.PParams (LedgerEra era)
  -> TxOut CtxTx (LedgerEra era)
  -> Either (TxBodyErrorAutoBalance (LedgerEra era)) IsEmpty
  -- ^ result of check if txout is empty
checkNonNegative bpparams txout@(TxOut balance _) = do
  let outValue@(L.MaryValue coin multiAsset) = balance ^. obtainCommonConstraints (useEra @era) L.valueTxOutL
      isPositiveValue = L.pointwise (>) outValue mempty
  if
    | L.isZero outValue -> pure Empty -- empty TxOut - ok, it's removed at the end
    | L.isZero coin ->
        -- no ADA, just non-ADA assets: positive lovelace is required in such case
        Left $
          TxBodyErrorAdaBalanceTooSmall
            txout
            (calculateMinimumUTxO bpparams txout)
            coin
    | not isPositiveValue -> Left $ TxBodyErrorBalanceNegative coin multiAsset
    | otherwise -> pure NonEmpty

-- | In the event of spending the exact amount of lovelace and non-ada assets in
-- the specified input(s), this function excludes the change
-- output. Note that this does not save any fees because by default
-- the fee calculation includes a change address for simplicity and
-- we make no attempt to recalculate the tx fee without a change address.
checkAndIncludeChange
  :: forall era
   . IsEra era
  => Ledger.PParams (LedgerEra era)
  -> TxOut CtxTx (LedgerEra era)
  -> [TxOut CtxTx (LedgerEra era)]
  -> Either (TxBodyErrorAutoBalance (LedgerEra era)) [TxOut CtxTx (LedgerEra era)]
checkAndIncludeChange pp change@(TxOut changeOutput _) rest = do
  isChangeEmpty <- checkNonNegative pp change
  if isChangeEmpty == Empty
    then pure rest
    else do
      let coin = changeOutput ^. L.coinTxOutL
      first ((coin &) . uncurry TxBodyErrorAdaBalanceTooSmall) $
        checkMinUTxOValue pp change
      -- We append change at the end so a client can predict the indexes of the outputs.
      pure $ rest <> [change]

checkMinUTxOValue
  :: Ledger.PParams (LedgerEra era)
  -> TxOut CtxTx (LedgerEra era)
  -> Either (TxOut CtxTx (LedgerEra era), Coin) ()
  -- ^ @Left (offending txout, minimum required utxo)@ or @Right ()@ when txout is ok
checkMinUTxOValue bpp txout@(TxOut out _) = do
  let minUTxO = calculateMinimumUTxO bpp txout
  if out ^. L.coinTxOutL >= minUTxO
    then Right ()
    else Left (txout, minUTxO)

calculateMinimumUTxO
  :: HasCallStack
  => Ledger.PParams (LedgerEra era)
  -> TxOut CtxTx (LedgerEra era)
  -> L.Coin
calculateMinimumUTxO pp (TxOut txout _) =
  let txOutWithMinCoin = L.setMinCoinTxOut pp txout
   in txOutWithMinCoin ^. L.coinTxOutL

-- | Compute the total balance of the proposed transaction. Ultimately, a valid
-- transaction must be fully balanced, which means that it has a total value
-- of zero.
--
-- Finding the (non-zero) balance of a partially constructed transaction is
-- useful for adjusting a transaction to be fully balanced.
evaluateTransactionBalance
  :: forall era
   . IsEra era
  => Ledger.PParams (LedgerEra era)
  -> Set PoolId
  -> Map StakeCredential L.Coin
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -> L.UTxO (LedgerEra era)
  -> UnsignedTx era
  -> L.Value (LedgerEra era)
evaluateTransactionBalance pp poolids stakeDelegDeposits drepDelegDeposits utxo (UnsignedTx unsignedTx) =
  let txbody = unsignedTx ^. L.bodyTxL
   in obtainCommonConstraints (useEra @era) $
        L.evalBalanceTxBody
          pp
          lookupDelegDeposit
          lookupDRepDeposit
          isRegPool
          utxo
          txbody
 where
  isRegPool :: Ledger.KeyHash Ledger.StakePool -> Bool
  isRegPool kh = Api.StakePoolKeyHash kh `Set.member` poolids

  lookupDelegDeposit
    :: Ledger.Credential 'Ledger.Staking -> Maybe L.Coin
  lookupDelegDeposit stakeCred =
    Map.lookup (fromShelleyStakeCredential stakeCred) stakeDelegDeposits

  lookupDRepDeposit
    :: Ledger.Credential 'Ledger.DRepRole -> Maybe L.Coin
  lookupDRepDeposit drepCred =
    Map.lookup drepCred drepDelegDeposits

-- | This is used in the balance calculation in the event where
-- the user does not supply the UTxO(s) they intend to spend
-- but they must supply their total balance of ADA.
-- evaluateTransactionBalance calls evalBalanceTxBody which requires a UTxO value.
-- This eventually calls getConsumedMaryValue which retrieves the balance
-- from the transaction itself. This necessitated a function to create a "fake" UTxO
-- to still use evalBalanceTxBody however this will fail for transactions
-- containing multi-assets, refunds and withdrawals.
-- TODO: Include multiassets
createFakeUTxO :: TxBodyContent era -> Coin -> L.UTxO era
createFakeUTxO txbodycontent totalAdaInUTxO =
  let singleTxIn = maybe [] (return . toShelleyTxIn . fst) $ List.uncons [txin | (txin, _) <- txIns txbodycontent]
      singleTxOut =
        maybe [] (\(TxOut firstOut _, _rest) -> return $ firstOut & L.coinTxOutL .~ totalAdaInUTxO) $
          List.uncons $
            txOuts txbodycontent
   in -- Take one txin and one txout. Replace the out value with totalAdaInUTxO
      -- Return an empty UTxO if there are no txins or txouts
      L.UTxO $ fromList $ zip singleTxIn singleTxOut

-- Calculation taken from validateInsufficientCollateral:
-- https://github.com/input-output-hk/cardano-ledger/blob/389b266d6226dedf3d2aec7af640b3ca4984c5ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxo.hs#L335
-- TODO: Bug Jared to expose a function from the ledger that returns total and
-- return collateral.
calcReturnAndTotalCollateral
  :: forall era
   . Ledger.AlonzoEraPParams (LedgerEra era)
  => IsEra era
  => L.Coin
  -- ^ Fee
  -> Ledger.PParams (LedgerEra era)
  -> [TxIn]
  -- ^ Collateral inputs the initial TxBodyContent
  -> Maybe (TxCollateral (LedgerEra era))
  -- ^ From the initial TxBodyContent
  -> AddressInEra era
  -- ^ Change address
  -> L.MaryValue
  -- ^ Total available collateral (can include non-ada)
  -> Maybe (TxCollateral (LedgerEra era))
calcReturnAndTotalCollateral _ _ [] _ _ _ = Nothing
calcReturnAndTotalCollateral fee pp' _ mTxCollateral cAddr totalAvailableCollateral = do
  let colPerc = pp' ^. Ledger.ppCollateralPercentageL
      -- We must first figure out how much lovelace we have committed
      -- as collateral and we must determine if we have enough lovelace at our
      -- collateral tx inputs to cover the tx
      totalCollateralLovelace = obtainCommonConstraints (useEra @era) $ L.coin totalAvailableCollateral
      requiredCollateral@(L.Coin reqAmt) = fromIntegral colPerc * fee
      totalCollateral =
        L.rationalToCoinViaCeiling $
          reqAmt % 100
      -- Why * 100? requiredCollateral is the product of the collateral percentage and the tx fee
      -- We choose to multiply 100 rather than divide by 100 to make the calculation
      -- easier to manage. At the end of the calculation we then use % 100 to perform our division
      -- and round the returnCollateral down which has the effect of potentially slightly
      -- overestimating the required collateral.
      L.Coin returnCollateralAmount = totalCollateralLovelace * 100 - requiredCollateral
      returnAdaCollateral :: L.MaryValue = L.inject $ L.rationalToCoinViaFloor $ returnCollateralAmount % 100
      -- non-ada collateral is not used, so just return it as is in the return collateral output
      _nonAdaCollateral = L.modifyCoin (const mempty) totalAvailableCollateral
      returnCollateral = returnAdaCollateral
  -- Remove non ada in collateral output -- <> nonAdaCollateral
  case mTxCollateral of
    Just (TxCollateral{}) -> mTxCollateral
    Nothing
      | returnCollateralAmount < 0 ->
          Nothing
      | otherwise ->
          Just $
            TxCollateral
              { totalCollateral = totalCollateral
              , returnCollateral =
                  obtainCommonConstraints (useEra @era) $ L.mkBasicTxOut (toShelleyAddr cAddr) returnCollateral
              }

-- | Transaction fees can be computed for a proposed transaction based on the
-- expected number of key witnesses (i.e. signatures).
--
-- When possible, use 'calculateMinTxFee', as it provides a more accurate
-- estimate:
evaluateTransactionFee
  :: Ledger.PParams (LedgerEra era)
  -> UnsignedTx era
  -> Word
  -- ^ The number of Shelley key witnesses
  -> Word
  -- ^ The number of Byron key witnesses
  -> Int
  -- ^ Reference script size in bytes
  -> L.Coin
evaluateTransactionFee pp (UnsignedTx tx) keywitcount byronwitcount refScriptsSize =
  L.estimateMinFeeTx pp tx (fromIntegral keywitcount) (fromIntegral byronwitcount) refScriptsSize

-- Essentially we check for the existence of collateral inputs. If they exist we
-- create a fictitious collateral return output. Why? Because we need to put dummy values
-- to get a fee estimate (i.e we overestimate the fee). The required collateral depends
-- on the tx fee as per the Alonzo spec.
maybeDummyTotalCollAndCollReturnOutput
  :: forall era
   . IsEra era
  => TxBodyContent (LedgerEra era)
  -> AddressInEra era
  -> Maybe (TxCollateral (LedgerEra era))
maybeDummyTotalCollAndCollReturnOutput TxBodyContent{txInsCollateral, txCollateral} cAddr =
  if null txInsCollateral
    then Nothing
    else
      let dummyRetCol =
            obtainCommonConstraints (useEra @era) $
              L.mkBasicTxOut (toShelleyAddr cAddr) (L.inject $ L.Coin (2 ^ (64 :: Integer)) - 1)

          dummyTotCol = L.Coin (2 ^ (32 :: Integer) - 1)
       in case txCollateral of
            Just col -> Just col
            Nothing ->
              return $
                TxCollateral
                  { returnCollateral = dummyRetCol
                  , totalCollateral = dummyTotCol
                  }

-- | Calculate the partial change - this does not include certificates' deposits
calculatePartialChangeValue
  :: forall era
   . IsEra era
  => L.MaryValue
  -> TxBodyContent (LedgerEra era)
  -> L.MaryValue
calculatePartialChangeValue incoming txbodycontent = do
  let outgoing = newUtxoValue
      mintedValue =
        mconcat
          [ toMaryValue $ policyAssetsToValue pid pAssets
          | (pid, (pAssets, _)) <- Map.toList . unTxMintValue $ txMintValue txbodycontent
          ]
  incoming L.<+> mintedValue L.<+> L.invert outgoing
 where
  newUtxoValue =
    mconcat
      [out ^. obtainCommonConstraints (useEra @era) L.valueTxOutL | (TxOut out _) <- txOuts txbodycontent]

substituteExecutionUnits
  :: forall era
   . IsEra era
  => Map ScriptWitnessIndex ExecutionUnits
  -> TxBodyContent (LedgerEra era)
  -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (TxBodyContent (LedgerEra era))
substituteExecutionUnits
  exUnitsMap
  txbodycontent@( TxBodyContent
                    txIns
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    txWithdrawals
                    txCertificates
                    txMintValue
                    _
                    txProposalProcedures
                    txVotingProcedures
                    _
                    _
                  ) = do
    mappedTxIns <- mapScriptWitnessesTxIns txIns
    mappedWithdrawals <- mapScriptWitnessesWithdrawals txWithdrawals
    mappedMintedVals <- mapScriptWitnessesMinting txMintValue
    mappedTxCertificates <- mapScriptWitnessesCertificates txCertificates
    mappedVotes <- mapScriptWitnessesVotes txVotingProcedures
    mappedProposals <- mapScriptWitnessesProposals txProposalProcedures

    Right $
      txbodycontent
        & setTxIns mappedTxIns
        & setTxCertificates mappedTxCertificates
        & setTxWithdrawals mappedWithdrawals
        & setTxMintValue mappedMintedVals
        & setTxVotingProcedures mappedVotes
        & setTxProposalProcedures mappedProposals
   where
    substituteExecUnits
      :: ScriptWitnessIndex
      -> AnyWitness (LedgerEra era)
      -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (AnyWitness (LedgerEra era))
    substituteExecUnits _ w@AnyKeyWitnessPlaceholder = Right w
    substituteExecUnits _ w@AnySimpleScriptWitness{} = Right w
    substituteExecUnits idx (AnyPlutusScriptWitness psw) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits ->
          Right $
            AnyPlutusScriptWitness $
              updatePlutusScriptWitnessExecutionUnits exunits psw

    mapScriptWitnessesTxIns
      :: [(TxIn, AnyWitness (LedgerEra era))]
      -> Either (TxBodyErrorAutoBalance (LedgerEra era)) [(TxIn, AnyWitness (LedgerEra era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( TxIn
                 , Either (TxBodyErrorAutoBalance (LedgerEra era)) (AnyWitness (LedgerEra era))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, wit')
            | (ix, txin, wit) <- indexTxIns txins
            , let wit' = substituteExecUnits ix wit
            ]
       in traverse
            (\(txIn, eWitness) -> (txIn,) <$> eWitness)
            mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: TxWithdrawals (LedgerEra era)
      -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (TxWithdrawals (LedgerEra era))
    mapScriptWitnessesWithdrawals txWithdrawals'@(TxWithdrawals _) =
      let mappedWithdrawals
            :: [ ( StakeAddress
                 , L.Coin
                 , Either (TxBodyErrorAutoBalance (LedgerEra era)) (AnyWitness (LedgerEra era))
                 )
               ]
          mappedWithdrawals =
            [ (addr, withdrawal, mappedWitness)
            | (ix, addr, withdrawal, wit@AnyPlutusScriptWitness{}) <- indexTxWithdrawals txWithdrawals'
            , let mappedWitness = substituteExecUnits ix wit
            ]
       in TxWithdrawals
            <$> traverse
              (\(sAddr, ll, eWitness) -> (sAddr,ll,) <$> eWitness)
              mappedWithdrawals

    mapScriptWitnessesCertificates
      :: TxCertificates (LedgerEra era)
      -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (TxCertificates (LedgerEra era))
    mapScriptWitnessesCertificates txCertificates' = do
      let mappedScriptWitnesses
            :: [ ( Exp.Certificate (LedgerEra era)
                 , Either
                     (TxBodyErrorAutoBalance (LedgerEra era))
                     ( Maybe
                         ( StakeCredential
                         , AnyWitness (LedgerEra era)
                         )
                     )
                 )
               ]
          mappedScriptWitnesses =
            [ (cert, Just . (stakeCred,) <$> eWitness')
            | (ix, cert, stakeCred, wit) <- indexTxCertificates txCertificates'
            , let eWitness' = substituteExecUnits ix wit
            ]
      TxCertificates . fromList <$> traverseScriptWitnesses mappedScriptWitnesses

    mapScriptWitnessesMinting
      :: TxMintValue (LedgerEra era)
      -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (TxMintValue (LedgerEra era))
    mapScriptWitnessesMinting txMintValue' = do
      let mappedScriptWitnesses =
            [ (policyId, (assets,) <$> substitutedWitness)
            | (ix, policyId, assets, wit@(AnyPlutusScriptWitness{})) <- indexTxMintValue txMintValue'
            , let substitutedWitness = substituteExecUnits ix wit
            ]
          -- merge map values, wit1 == wit2 will always hold
          mergeValues (assets1, wit1) (assets2, _wit2) = (assets1 <> assets2, wit1)
      final <- Map.fromListWith mergeValues <$> traverseScriptWitnesses mappedScriptWitnesses
      pure $ TxMintValue final

    mapScriptWitnessesVotes
      :: Maybe (TxVotingProcedures (LedgerEra era))
      -> Either
           (TxBodyErrorAutoBalance (LedgerEra era))
           (TxVotingProcedures (LedgerEra era))
    mapScriptWitnessesVotes Nothing = return $ TxVotingProcedures (L.VotingProcedures mempty) mempty
    mapScriptWitnessesVotes (Just v@(TxVotingProcedures vProcedures _)) = do
      let eSubstitutedExecutionUnits =
            [ (vote, updatedWitness)
            | (ix, vote, wit@(AnyPlutusScriptWitness{})) <- indexTxVotingProcedures v
            , let updatedWitness = substituteExecUnits ix wit
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return
        (TxVotingProcedures vProcedures (fromList substitutedExecutionUnits))

    mapScriptWitnessesProposals
      :: Maybe (TxProposalProcedures (LedgerEra era))
      -> Either
           (TxBodyErrorAutoBalance (LedgerEra era))
           (TxProposalProcedures (LedgerEra era))
    mapScriptWitnessesProposals Nothing = return $ TxProposalProcedures OMap.empty
    mapScriptWitnessesProposals (Just proposals) = do
      let indexed = indexWitnessedTxProposalProcedures proposals
          eSubstitutedExecutionUnits =
            [ (p, updatedWit)
            | (p, (i, wit)) <- indexed
            , let updatedWit = substituteExecUnits i wit
            ]
      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits
      pure $
        mkTxProposalProcedures substitutedExecutionUnits

collectTxBodyScriptWitnesses
  :: forall era
   . IsEra era
  => TxBodyContent (LedgerEra era)
  -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
collectTxBodyScriptWitnesses
  TxBodyContent
    { txIns
    , txWithdrawals
    , txCertificates
    , txMintValue
    , txVotingProcedures
    , txProposalProcedures
    } =
    concat
      [ scriptWitnessesTxIns txIns
      , scriptWitnessesWithdrawals txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting txMintValue
      , maybe [] scriptWitnessesVoting txVotingProcedures
      , maybe [] scriptWitnessesProposing txProposalProcedures
      ]
   where
    scriptWitnessesTxIns
      :: [(TxIn, AnyWitness (LedgerEra era))]
      -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
    scriptWitnessesTxIns txIns' =
      List.nub
        [ (ix, wit)
        | (ix, _, Just wit@AnyScriptWitnessPlutus{}) <- fmap toAnyScriptWitness <$> indexTxIns txIns'
        ]

    scriptWitnessesWithdrawals
      :: TxWithdrawals (LedgerEra era)
      -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
    scriptWitnessesWithdrawals txw =
      List.nub
        [ (ix, wit)
        | (ix, _, _, Just wit@AnyScriptWitnessPlutus{}) <- fmap toAnyScriptWitness <$> indexTxWithdrawals txw
        ]
    -- TODO: If this works you need to change the rest!
    scriptWitnessesCertificates
      :: TxCertificates (LedgerEra era)
      -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
    scriptWitnessesCertificates txc =
      List.nub
        [ (ix, wit)
        | (ix, _, _, Just wit@AnyScriptWitnessPlutus{}) <-
            fmap toAnyScriptWitness <$> indexTxCertificates txc
        ]

    scriptWitnessesMinting
      :: TxMintValue (LedgerEra era)
      -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
    scriptWitnessesMinting txMintValue' =
      List.nub
        [ (ix, wit)
        | (ix, _, _, Just wit@AnyScriptWitnessPlutus{}) <-
            fmap toAnyScriptWitness <$> indexTxMintValue txMintValue'
        ]

    scriptWitnessesVoting
      :: TxVotingProcedures (LedgerEra era)
      -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
    scriptWitnessesVoting txv =
      List.nub
        [ (ix, wit)
        | (ix, _, Just wit@AnyScriptWitnessPlutus{}) <-
            fmap toAnyScriptWitness <$> indexTxVotingProcedures txv
        ]

    scriptWitnessesProposing
      :: TxProposalProcedures (LedgerEra era)
      -> [(ScriptWitnessIndex, Exp.AnyScriptWitness (LedgerEra era))]
    scriptWitnessesProposing txp =
      List.nub
        [ (ix, wit)
        | (_, (ix, Just wit@AnyScriptWitnessPlutus{})) <-
            (fmap . fmap) toAnyScriptWitness <$> indexWitnessedTxProposalProcedures txp
        ]

toAnyScriptWitness :: AnyWitness era -> Maybe (Exp.AnyScriptWitness era)
toAnyScriptWitness AnyKeyWitnessPlaceholder = Nothing
toAnyScriptWitness (AnySimpleScriptWitness ssw) = Just $ AnyScriptWitnessSimple ssw
toAnyScriptWitness (AnyPlutusScriptWitness psw) = Just $ AnyScriptWitnessPlutus psw

traverseScriptWitnesses
  :: [(a, Either (TxBodyErrorAutoBalance (LedgerEra era)) b)]
  -> Either (TxBodyErrorAutoBalance (LedgerEra era)) [(a, b)]
traverseScriptWitnesses =
  traverse (\(item, eRes) -> eRes >>= (\res -> Right (item, res)))

-- | Index transaction inputs ordered by TxIn
-- Please note that the result can contain also 'KeyWitness'es.
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxIns
  :: [(TxIn, AnyWitness (LedgerEra era))]
  -> [(ScriptWitnessIndex, TxIn, AnyWitness (LedgerEra era))]
indexTxIns txins =
  [ (ScriptWitnessIndexTxIn ix, txIn, witness)
  | (ix, (txIn, witness)) <- zip [0 ..] $ orderTxIns txins
  ]
 where
  -- This relies on the TxId Ord instance being consistent with the
  -- Ledger.TxId Ord instance via the toShelleyTxId conversion
  -- This is checked by prop_ord_distributive_TxId
  orderTxIns :: [(TxIn, v)] -> [(TxIn, v)]
  orderTxIns = sortBy (compare `on` fst)

-- | Index the withdrawals with witnesses in the order of stake addresses.
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxWithdrawals
  :: TxWithdrawals era
  -> [(ScriptWitnessIndex, StakeAddress, L.Coin, AnyWitness era)]
indexTxWithdrawals (TxWithdrawals withdrawals) =
  [ (ScriptWitnessIndexWithdrawal ix, addr, coin, witness)
  | (ix, (addr, coin, witness)) <- zip [0 ..] (orderStakeAddrs withdrawals)
  ]
 where
  -- This relies on the StakeAddress Ord instance being consistent with the
  -- Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion
  -- This is checked by prop_ord_distributive_StakeAddress
  orderStakeAddrs :: [(StakeAddress, x, v)] -> [(StakeAddress, x, v)]
  orderStakeAddrs = sortBy (compare `on` (\(k, _, _) -> k))

-- | Index certificates with witnesses by the order they appear in the list (in the transaction).
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxCertificates
  :: TxCertificates (LedgerEra era)
  -> [ ( ScriptWitnessIndex
       , Exp.Certificate (LedgerEra era)
       , StakeCredential
       , AnyWitness (LedgerEra era)
       )
     ]
indexTxCertificates (TxCertificates certsWits) =
  [ (ScriptWitnessIndexCertificate ix, cert, stakeCred, witness)
  | (ix, (cert, Just (stakeCred, witness))) <- zip [0 ..] $ toList certsWits
  ]

-- | Index the assets with witnesses in the order of policy ids.
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxMintValue
  :: TxMintValue era
  -> [ ( ScriptWitnessIndex
       , PolicyId
       , PolicyAssets
       , AnyWitness era
       )
     ]
indexTxMintValue (TxMintValue policiesWithAssets) =
  [ (ScriptWitnessIndexMint ix, policyId, assets, witness)
  | (ix, (policyId, (assets, witness))) <- zip [0 ..] $ toList policiesWithAssets
  ]

-- | Index voting procedures by the order of the votes ('Ord').
indexTxVotingProcedures
  :: TxVotingProcedures era
  -> [ ( ScriptWitnessIndex
       , L.Voter
       , AnyWitness era
       )
     ]
indexTxVotingProcedures (TxVotingProcedures vProcedures sWitMap) =
  [ (ScriptWitnessIndexVoting $ fromIntegral index, vote, scriptWitness)
  | let allVoteMap = L.unVotingProcedures vProcedures
  , (vote, scriptWitness) <- toList sWitMap
  , index <- maybeToList $ Map.lookupIndex vote allVoteMap
  ]

-- | Index proposal procedures by their order ('Ord').
indexWitnessedTxProposalProcedures
  :: forall era
   . IsEra era
  => TxProposalProcedures (LedgerEra era)
  -> [ ( L.ProposalProcedure (LedgerEra era)
       , (ScriptWitnessIndex, AnyWitness (LedgerEra era))
       )
     ]
indexWitnessedTxProposalProcedures (TxProposalProcedures proposals) = do
  let allProposalsList = zip [0 ..] $ obtainCommonConstraints (useEra @era) $ toList proposals
  [ (proposal, (ScriptWitnessIndexProposing ix, anyWitness))
    | (ix, (proposal, anyWitness)) <- allProposalsList
    ]

toUnsigned :: forall era. Era era -> L.Tx (LedgerEra era) -> UnsignedTx era
toUnsigned e tx =
  obtainCommonConstraints e $
    UnsignedTx tx

-- | Compute the 'ExecutionUnits' required for each script in the transaction.
--
-- This process involves executing all scripts and counting the actual execution units
-- consumed.
evaluateTransactionExecutionUnits
  :: forall era
   . IsEra era
  => SystemStart
  -> LedgerEpochInfo
  -> L.PParams (LedgerEra era)
  -> L.UTxO (LedgerEra era)
  -> L.Tx (LedgerEra era)
  -> Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
evaluateTransactionExecutionUnits systemstart epochInfo pp utxo tx =
  obtainCommonConstraints (useEra @era) $
    fromLedgerScriptExUnitsMap $
      L.evalTxExUnitsWithLogs pp tx utxo ledgerEpochInfo systemstart
 where
  LedgerEpochInfo ledgerEpochInfo = epochInfo

  fromLedgerScriptExUnitsMap
    :: L.AlonzoEraScript (LedgerEra era)
    => Map
         (L.PlutusPurpose L.AsIx (LedgerEra era))
         (Either (L.TransactionScriptFailure (LedgerEra era)) (EvalTxExecutionUnitsLog, L.ExUnits))
    -> Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
  fromLedgerScriptExUnitsMap exmap =
    fromList
      [ ( obtainCommonConstraints (useEra @era) $ toScriptIndex (convert useEra) rdmrptr
        , bimap fromAlonzoScriptExecutionError (second fromAlonzoExUnits) exunitsOrFailure
        )
      | (rdmrptr, exunitsOrFailure) <- toList exmap
      ]

  fromAlonzoScriptExecutionError
    :: L.AlonzoEraScript (LedgerEra era)
    => L.TransactionScriptFailure (LedgerEra era)
    -> ScriptExecutionError
  fromAlonzoScriptExecutionError =
    \case
      L.UnknownTxIn txin -> ScriptErrorMissingTxIn txin'
       where
        txin' = fromShelleyTxIn txin
      L.InvalidTxIn txin -> ScriptErrorTxInWithoutDatum txin'
       where
        txin' = fromShelleyTxIn txin
      L.MissingDatum dh -> ScriptErrorWrongDatum (ScriptDataHash dh)
      L.ValidationFailure execUnits evalErr logs scriptWithContext ->
        ScriptErrorEvaluationFailed $ DebugPlutusFailure evalErr scriptWithContext execUnits logs
      L.IncompatibleBudget _ -> ScriptErrorExecutionUnitsOverflow
      L.RedeemerPointsToUnknownScriptHash rdmrPtr ->
        ScriptErrorRedeemerPointsToUnknownScriptHash $
          obtainCommonConstraints (useEra @era) $
            toScriptIndex (convert useEra) rdmrPtr
      -- This should not occur while using cardano-cli because we zip together
      -- the Plutus script and the use site (txin, certificate etc). Therefore
      -- the redeemer pointer will always point to a Plutus script.
      L.MissingScript indexOfScriptWitnessedItem resolveable ->
        let scriptWitnessedItemIndex = obtainCommonConstraints (useEra @era) $ toScriptIndex (convert useEra) indexOfScriptWitnessedItem
         in ScriptErrorMissingScript
              scriptWitnessedItemIndex
              $ obtainCommonConstraints (useEra @era)
              $ ResolvablePointers (convert useEra)
              $ Map.map extractScriptBytesAndLanguage resolveable
      L.NoCostModelInLedgerState l -> ScriptErrorMissingCostModel l
      L.ContextError e ->
        obtainCommonConstraints (useEra @era) $ ScriptErrorTranslationError e

-- | This is similar to 'makeTransactionBody' but with greater automation to
-- calculate suitable values for several things.
--
-- In particular:
--
-- * It calculates the correct script 'ExecutionUnits' (ignoring the provided
--   values, which can thus be zero).
--
-- * It calculates the transaction fees based on the script 'ExecutionUnits',
--   the current 'ProtocolParameters', and an estimate of the number of
--   key witnesses (i.e. signatures). There is an override for the number of
--   key witnesses.
--
-- * It accepts a change address, calculates the balance of the transaction
--   and puts the excess change into the change output.
--
-- * It also checks that the balance is positive and the change is above the
--   minimum threshold.
--
-- To do this, it requires more information than 'makeTransactionBody', all of
-- which can be queried from a local node.
makeTransactionBodyAutoBalance
  :: forall era
   . ()
  => HasCallStack
  => IsEra era
  => SystemStart
  -> LedgerEpochInfo
  -> L.PParams (LedgerEra era)
  -> Set PoolId
  -- ^ The set of registered stake pools, being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ The map of all deposits for stake credentials that are being
  --   unregistered in this transaction
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -- ^ The map of all deposits for DRep credentials that are being
  --   unregistered in this transaction
  -> L.UTxO (LedgerEra era)
  -- ^ The transaction inputs (including reference and collateral ones), not the entire 'UTxO'.
  -> TxBodyContent (LedgerEra era)
  -> AddressInEra era
  -- ^ Change address
  -> Maybe Word
  -- ^ Override key witnesses
  -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (UnsignedTx era, TxBodyContent (LedgerEra era))
makeTransactionBodyAutoBalance
  systemstart
  history
  pp
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  utxo
  txbodycontent
  changeaddr
  mnkeys = do
    -- Our strategy is to:
    -- 1. evaluate all the scripts to get the exec units, update with ex units
    -- 2. figure out the overall min fees
    -- 3. update tx with fees
    -- 4. balance the transaction and update tx change output

    let txbodyForChange =
          makeUnsignedTx
            useEra
            txbodycontent

    let initialChangeTxOutValue :: Ledger.Value (LedgerEra era) =
          evaluateTransactionBalance pp poolids stakeDelegDeposits drepDelegDeposits utxo txbodyForChange
        initialChangeTxOut :: TxOut CtxTx (LedgerEra era) =
          obtainCommonConstraints (useEra @era) $
            TxOut (L.mkBasicTxOut (toShelleyAddr changeaddr) initialChangeTxOutValue) Nothing

    -- Initial change is only used for execution units evaluation, so we don't require minimum UTXO requirement
    -- to be satisfied at this point
    _ <- checkNonNegative pp initialChangeTxOut

    -- Tx body used only for evaluating execution units. Because txout exact
    -- values do not matter much here, we are using an initial change value,
    -- which is slightly overestimated, because it does not include fee or
    -- scripts execution costs.
    -- TODO: The txbody is made (leder tx) so this
    -- is where the execution units map is made
    let UnsignedTx txbody =
          makeUnsignedTx
            useEra
            ( txbodycontent
                & modTxOuts
                  (<> [initialChangeTxOut])
            )
    let exUnitsMapWithLogs =
          evaluateTransactionExecutionUnits
            systemstart
            history
            pp
            utxo
            txbody

    let exUnitsMap = Map.map (fmap snd) exUnitsMapWithLogs

    exUnitsMap' <-
      case Map.mapEither id exUnitsMap of
        (failures, exUnitsMap') ->
          handleExUnitsErrors
            (txScriptValidity txbodycontent)
            failures
            exUnitsMap'

    txbodycontent1 <-
      substituteExecutionUnits exUnitsMap' txbodycontent

    -- Make a txbody that we will use for calculating the fees. For the purpose
    -- of fees we just need to make a txbody of the right size in bytes. We
    -- do not need the right values for the fee. We use "big enough" value
    -- for the fee and set so that the CBOR encoding size of the tx will be
    -- big enough to cover the size of the final output and fee. Yes this
    -- means this current code will only work for final fee of less than
    -- around 4000 ada (2^32-1 lovelace).
    let maxLovelaceFee = L.Coin (2 ^ (32 :: Integer) - 1)
    -- Make a txbody that we will use for calculating the fees.
    let mDummyCollateral = maybeDummyTotalCollAndCollReturnOutput txbodycontent changeaddr
    let txbody1 =
          makeUnsignedTx
            useEra
            txbodycontent1
              { txFee = maxLovelaceFee
              , txCollateral = mDummyCollateral
              , txOuts =
                  txOuts txbodycontent
                    <> [initialChangeTxOut]
              }

    -- NB: This has the potential to over estimate the fees because estimateTransactionKeyWitnessCount
    -- makes the conservative assumption that all inputs are from distinct
    -- addresses.
    let nkeys =
          fromMaybe
            (estimateTransactionKeyWitnessCount txbodycontent1)
            mnkeys
        fee = calculateMinTxFee pp utxo txbody1 nkeys
        totalPotentialCollateral =
          mconcat
            [ L.coin (txOut ^. obtainCommonConstraints (useEra @era) L.valueTxOutL :: L.MaryValue)
            | collInputs <- pure $ txInsCollateral txbodycontent
            , collTxIn <- collInputs
            , Just txOut <- pure $ Map.lookup (toShelleyTxIn collTxIn) (L.unUTxO utxo)
            ]
        maybeTxCollateral =
          obtainCommonConstraints (useEra @era) $
            calcReturnAndTotalCollateral
              fee
              pp
              (txInsCollateral txbodycontent)
              (txCollateral txbodycontent)
              changeaddr
              (L.inject totalPotentialCollateral)

    -- Make a txbody for calculating the balance. For this the size of the tx
    -- does not matter, instead it's just the values of the fee and outputs.
    -- Here we do not want to start with any change output, since that's what
    -- we need to calculate.
    let txbody2 =
          makeUnsignedTx
            useEra
            txbodycontent1
              { txFee = fee
              , txCollateral = maybeTxCollateral
              }

    case useEra @era of
      DijkstraEra -> error "makeTransactionBodyAutoBalance: DijkstraEra not supported"
      ConwayEra -> do
        let balance :: L.MaryValue = evaluateTransactionBalance pp poolids stakeDelegDeposits drepDelegDeposits utxo txbody2
            adaBalance = getAda (useEra @era) balance
        when (adaBalance < 0) $
          Left $
            BalanceIsNegative adaBalance txbodyForChange
        -- Previously we reported txbody2 now
        -- I am reporting with txbodyForChange to
        -- see if there are any tx inputs!

        let balanceTxOut :: TxOut CtxTx (LedgerEra era) =
              obtainCommonConstraints (useEra @era) $
                TxOut (L.mkBasicTxOut (toShelleyAddr changeaddr) balance) Nothing
        first (uncurry TxBodyErrorMinUTxONotMet)
          . mapM_ (checkMinUTxOValue pp)
          $ txOuts txbodycontent1

        -- check if change meets txout criteria, and include if non-zero
        finalTxOuts <- checkAndIncludeChange pp balanceTxOut (txOuts txbodycontent1)

        -- TODO: we could add the extra fee for the CBOR encoding of the change,
        -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.
        -- The txbody with the final fee and change output. This should work
        -- provided that the fee and change are less than 2^32-1, and so will
        -- fit within the encoding size we picked above when calculating the fee.
        -- Yes this could be an over-estimate by a few bytes if the fee or change
        -- would fit within 2^16-1. That's a possible optimisation.
        let finalTxBodyContent =
              txbodycontent1
                { txFee = fee
                , txOuts = finalTxOuts
                , txCollateral = maybeTxCollateral
                }
        let txbody3 =
              makeUnsignedTx
                useEra
                finalTxBodyContent
        return
          (txbody3, finalTxBodyContent)

getAda :: Era era -> L.Value (LedgerEra era) -> L.Coin
getAda e val = case e of
  ConwayEra -> L.coin val
  DijkstraEra -> L.coin val

handleExUnitsErrors
  :: ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> Map ScriptWitnessIndex ScriptExecutionError
  -> Map ScriptWitnessIndex ExecutionUnits
  -> Either (TxBodyErrorAutoBalance (LedgerEra era)) (Map ScriptWitnessIndex ExecutionUnits)
handleExUnitsErrors ScriptValid failuresMap exUnitsMap =
  if null failures
    then Right exUnitsMap
    else Left (TxBodyScriptExecutionError failures)
 where
  failures :: [(ScriptWitnessIndex, ScriptExecutionError)]
  failures = toList failuresMap
handleExUnitsErrors ScriptInvalid failuresMap exUnitsMap
  | null failuresMap = Left TxBodyScriptBadScriptValidity
  | otherwise = Right $ Map.map (\_ -> ExecutionUnits 0 0) failuresMap <> exUnitsMap

-- | Provide and approximate count of the key witnesses (i.e. signatures)
-- required for a transaction.
--
-- This estimate is not exact and may overestimate the required number of witnesses.
-- The function makes conservative assumptions, including:
--
-- * Treating all inputs as originating from distinct addresses. In reality,
--   multiple inputs may share the same address, requiring only one witness per address.
--
-- * Assuming regular and collateral inputs are distinct, even though they may overlap.
--
-- TODO: Consider implementing a more precise calculation that leverages the UTXO set
-- to determine which inputs correspond to distinct addresses. Additionally, the
-- estimate can be refined by distinguishing between Shelley and Byron-style witnesses.
estimateTransactionKeyWitnessCount :: forall era. IsEra era => TxBodyContent (LedgerEra era) -> Word
estimateTransactionKeyWitnessCount
  TxBodyContent
    { txIns
    , txInsCollateral
    , txExtraKeyWits
    , txWithdrawals
    , txCertificates
    , txProposalProcedures
    } =
    fromIntegral $
      sum (map estimateTxInWitnesses txIns)
        + length txInsCollateral
        + case txExtraKeyWits of
          TxExtraKeyWitnesses khs ->
            length khs
        + case txWithdrawals of
          TxWithdrawals withdrawals ->
            length [() | (_, _, AnyKeyWitnessPlaceholder) <- withdrawals]
        + case txCertificates of
          TxCertificates credWits ->
            length
              [() | (_, Just (_, AnyKeyWitnessPlaceholder)) <- toList credWits]
        + case txProposalProcedures of
          Just (TxProposalProcedures m) ->
            OMap.size m
          Nothing -> 0
   where
    estimateTxInWitnesses :: (TxIn, AnyWitness (LedgerEra era)) -> Int
    estimateTxInWitnesses (_, AnyKeyWitnessPlaceholder) = 1
    estimateTxInWitnesses (_, AnySimpleScriptWitness (SScript (SimpleScript simpleScript))) =
      maxWitnessesInSimpleScript $
        obtainCommonConstraints (useEra @era) $
          Old.fromAllegraTimelock simpleScript
    estimateTxInWitnesses (_, AnySimpleScriptWitness (SReferenceScript _)) = 0
    estimateTxInWitnesses (_, AnyPlutusScriptWitness{}) = 0
    -- This is a rough conservative estimate of the maximum number of witnesses
    -- needed for a simple script to be satisfied. It is conservative because it
    -- assumes that each key hash only appears once, and it assumes the worst
    -- scenario. A more accurate estimate for the maximum could be computed by
    -- keeping track of the possible combinations of key hashes that have
    -- potentially already been counted, but that would increase complexity a lot,
    -- and it would still be a conservative estimate.
    maxWitnessesInSimpleScript :: Old.SimpleScript -> Int
    maxWitnessesInSimpleScript (Old.RequireSignature _) = 1
    maxWitnessesInSimpleScript (Old.RequireTimeBefore _) = 0
    maxWitnessesInSimpleScript (Old.RequireTimeAfter _) = 0
    maxWitnessesInSimpleScript (Old.RequireAllOf simpleScripts) = sum $ map maxWitnessesInSimpleScript simpleScripts
    maxWitnessesInSimpleScript (Old.RequireAnyOf simpleScripts) = maximum $ map maxWitnessesInSimpleScript simpleScripts
    maxWitnessesInSimpleScript (Old.RequireMOf n simpleScripts) = sum $ take n $ sortBy (comparing Down) (map maxWitnessesInSimpleScript simpleScripts)

-- | Estimate the minimum transaction fee by analyzing the transaction structure
-- and determining the required number and type of key witnesses.
--
-- It requires access to the relevant portion of the UTXO set to look up any
-- transaction inputs (txins) included in the transaction. However, it cannot
-- reliably determine the number of witnesses required for native scripts.
--
-- Therefore, the number of witnesses needed for native scripts must be provided
-- as an additional argument.
calculateMinTxFee
  :: forall era
   . IsEra era
  => Ledger.PParams (LedgerEra era)
  -> L.UTxO (LedgerEra era)
  -> UnsignedTx era
  -> Word
  -- ^ The number of Shelley key witnesses
  -> L.Coin
calculateMinTxFee pp utxo (UnsignedTx txbody) keywitcount =
  obtainCommonConstraints (useEra @era) $
    L.calcMinFeeTx utxo pp txbody (fromIntegral keywitcount)
