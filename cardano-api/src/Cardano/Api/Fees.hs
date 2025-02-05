{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Fee calculation
module Cardano.Api.Fees
  ( -- * Transaction fees
    evaluateTransactionFee
  , calculateMinTxFee
  , estimateTransactionKeyWitnessCount

    -- * Script execution units
  , evaluateTransactionExecutionUnits
  , evaluateTransactionExecutionUnitsShelley
  , ScriptExecutionError (..)
  , TransactionValidityError (..)

    -- * Transaction balance
  , evaluateTransactionBalance

    -- * Automated transaction building
  , estimateBalancedTxBody
  , estimateOrCalculateBalancedTxBody
  , makeTransactionBodyAutoBalance
  , calcReturnAndTotalCollateral
  , AutoBalanceError (..)
  , BalancedTxBody (..)
  , FeeEstimationMode (..)
  , RequiredShelleyKeyWitnesses (..)
  , RequiredByronKeyWitnesses (..)
  , TotalReferenceScriptsSize (..)
  , TxBodyErrorAutoBalance (..)
  , TxFeeEstimationError (..)

    -- * Minimum UTxO calculation
  , calculateMinimumUTxO

    -- * Internal helpers
  , ResolvablePointers (..)
  )
where

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.Convert
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Core
import           Cardano.Api.Error
import           Cardano.Api.Feature
import qualified Cardano.Api.Ledger.Lens as A
import           Cardano.Api.Plutus
import           Cardano.Api.Pretty
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Script
import           Cardano.Api.Tx.Body
import           Cardano.Api.Tx.Sign
import           Cardano.Api.Value

import qualified Cardano.Ledger.Alonzo.Core as Ledger
import qualified Cardano.Ledger.Alonzo.Plutus.Context as Plutus
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Core as L
import           Cardano.Ledger.Credential as Ledger (Credential)
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Plutus.Language as Plutus
import qualified Cardano.Ledger.Val as L
import qualified Ouroboros.Consensus.HardFork.History as Consensus

import           Control.Monad
import           Data.Bifunctor (bimap, first, second)
import           Data.ByteString.Short (ShortByteString)
import           Data.Function ((&))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.OSet.Strict as OSet
import           Data.Ratio
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro ((.~), (^.))

-- | Type synonym for logs returned by the ledger's @evalTxExUnitsWithLogs@ function.
-- for scripts in transactions.
type EvalTxExecutionUnitsLog = [Text]

data AutoBalanceError era
  = AutoBalanceEstimationError (TxFeeEstimationError era)
  | AutoBalanceCalculationError (TxBodyErrorAutoBalance era)
  deriving Show

instance Error (AutoBalanceError era) where
  prettyError = \case
    AutoBalanceEstimationError e -> prettyError e
    AutoBalanceCalculationError e -> prettyError e

estimateOrCalculateBalancedTxBody
  :: ShelleyBasedEra era
  -> FeeEstimationMode era
  -> L.PParams (ShelleyLedgerEra era)
  -> TxBodyContent BuildTx era
  -> Set PoolId
  -> Map StakeCredential L.Coin
  -> Map (Ledger.Credential Ledger.DRepRole Ledger.StandardCrypto) L.Coin
  -> AddressInEra era
  -> Either (AutoBalanceError era) (BalancedTxBody era)
estimateOrCalculateBalancedTxBody era feeEstMode pparams txBodyContent poolids stakeDelegDeposits drepDelegDeposits changeAddr =
  case feeEstMode of
    CalculateWithSpendableUTxO utxo systemstart ledgerEpochInfo mOverride ->
      first AutoBalanceCalculationError $
        makeTransactionBodyAutoBalance
          era
          systemstart
          ledgerEpochInfo
          (LedgerProtocolParameters pparams)
          poolids
          stakeDelegDeposits
          drepDelegDeposits
          utxo
          txBodyContent
          changeAddr
          mOverride
    EstimateWithoutSpendableUTxO
      totalPotentialCollateral
      totalUTxOValue
      exUnitsMap
      (RequiredShelleyKeyWitnesses numKeyWits)
      (RequiredByronKeyWitnesses numByronWits)
      (TotalReferenceScriptsSize totalRefScriptsSize) ->
        forShelleyBasedEraInEon
          era
          (Left $ AutoBalanceEstimationError TxFeeEstimationOnlyMaryOnwardsSupportedError)
          ( \w ->
              first AutoBalanceEstimationError $
                estimateBalancedTxBody
                  w
                  txBodyContent
                  pparams
                  poolids
                  stakeDelegDeposits
                  drepDelegDeposits
                  exUnitsMap
                  totalPotentialCollateral
                  numKeyWits
                  numByronWits
                  totalRefScriptsSize
                  changeAddr
                  totalUTxOValue
          )

data TxFeeEstimationError era
  = TxFeeEstimationTransactionTranslationError (TransactionValidityError era)
  | TxFeeEstimationScriptExecutionError (TxBodyErrorAutoBalance era)
  | TxFeeEstimationBalanceError (TxBodyErrorAutoBalance era)
  | TxFeeEstimationxBodyError TxBodyError
  | TxFeeEstimationFinalConstructionError TxBodyError
  | TxFeeEstimationOnlyMaryOnwardsSupportedError
  deriving Show

instance Error (TxFeeEstimationError era) where
  prettyError = \case
    TxFeeEstimationTransactionTranslationError e -> prettyError e
    TxFeeEstimationScriptExecutionError e -> prettyError e
    TxFeeEstimationBalanceError e -> prettyError e
    TxFeeEstimationxBodyError e -> prettyError e
    TxFeeEstimationFinalConstructionError e -> prettyError e
    TxFeeEstimationOnlyMaryOnwardsSupportedError ->
      "Only mary era onwards supported."

-- | Use when you do not have access to the UTxOs you intend to spend
estimateBalancedTxBody
  :: forall era
   . HasCallStack
  => MaryEraOnwards era
  -> TxBodyContent BuildTx era
  -> L.PParams (ShelleyLedgerEra era)
  -> Set PoolId
  -- ^ The set of registered stake pools, that are being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ Map of all deposits for stake credentials that are being
  --   unregistered in this transaction
  -> Map (Ledger.Credential Ledger.DRepRole Ledger.StandardCrypto) L.Coin
  -- ^ Map of all deposits for drep credentials that are being
  --   unregistered in this transaction
  -> Map ScriptWitnessIndex ExecutionUnits
  -- ^ Plutus script execution units
  -> Coin
  -- ^ Total potential collateral amount
  -> Int
  -- ^ The number of key witnesses still to be added to the transaction.
  -> Int
  -- ^ The number of Byron key witnesses still to be added to the transaction.
  -> Int
  -- ^ Size of all reference scripts in bytes
  -> AddressInEra era
  -- ^ Change address
  -> Value
  -- ^ Total value of UTxOs being spent
  -> Either (TxFeeEstimationError era) (BalancedTxBody era)
estimateBalancedTxBody
  w
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

    let sbe = convert w
    txbodycontent1 <-
      maryEraOnwardsConstraints w $
        first TxFeeEstimationScriptExecutionError $
          substituteExecutionUnits exUnitsMap txbodycontent

    -- Step 2. We need to calculate the current balance of the tx. The user
    -- must at least provide the total value of the UTxOs they intend to spend
    -- for us to calulate the balance. NB: We must:
    --  1. Subtract certificate and proposal deposits
    -- from the total available Ada value!
    -- Page 24 Shelley ledger spec
    let certificates = convCertificates sbe $ txCertificates txbodycontent1

        proposalProcedures :: OSet.OSet (L.ProposalProcedure (ShelleyLedgerEra era))
        proposalProcedures =
          maryEraOnwardsConstraints w $
            maybe mempty (convProposalProcedures . unFeatured) (txProposalProcedures txbodycontent1)

        totalDeposits :: L.Coin
        totalDeposits =
          -- Because we do not have access to the ledger state and to reduce the complexity of this function's
          -- type signature, we assume the user is trying to register a stake pool that has not been
          -- registered before and has not included duplicate stake pool registration certificates.
          let assumeStakePoolHasNotBeenRegistered = const False
           in sum
                [ maryEraOnwardsConstraints w $
                    L.getTotalDepositsTxCerts pparams assumeStakePoolHasNotBeenRegistered certificates
                , maryEraOnwardsConstraints w $
                    mconcat $
                      map (^. L.pProcDepositL) $
                        toList proposalProcedures
                ]

        availableUTxOValue =
          mconcat
            [ totalUTxOValue
            , negateValue (lovelaceToValue totalDeposits)
            ]

    let partialChange = toLedgerValue w $ calculatePartialChangeValue sbe availableUTxOValue txbodycontent1
        maxLovelaceChange = L.Coin (2 ^ (64 :: Integer)) - 1
        changeWithMaxLovelace = partialChange & A.adaAssetL sbe .~ maxLovelaceChange
        changeTxOut =
          forShelleyBasedEraInEon
            sbe
            (lovelaceToTxOutValue sbe maxLovelaceChange)
            (\w' -> maryEraOnwardsConstraints w' $ TxOutValueShelleyBased sbe changeWithMaxLovelace)

    let (dummyCollRet, dummyTotColl) = maybeDummyTotalCollAndCollReturnOutput sbe txbodycontent changeaddr

    -- Step 3. Create a tx body with out max lovelace fee. This is strictly for
    -- calculating our fee with evaluateTransactionFee.
    let maxLovelaceFee = L.Coin (2 ^ (32 :: Integer) - 1)
    txbody1ForFeeEstimateOnly <-
      first TxFeeEstimationxBodyError $ -- TODO: impossible to fail now
        createTransactionBody
          sbe
          txbodycontent1
            { txFee = TxFeeExplicit sbe maxLovelaceFee
            , txOuts =
                TxOut changeaddr changeTxOut TxOutDatumNone ReferenceScriptNone
                  : txOuts txbodycontent
            , txReturnCollateral = dummyCollRet
            , txTotalCollateral = dummyTotColl
            }
    let fee =
          evaluateTransactionFee
            sbe
            pparams
            txbody1ForFeeEstimateOnly
            (fromIntegral intendedKeyWits)
            (fromIntegral byronwits)
            sizeOfAllReferenceScripts

        -- Step 4. We use the fee to calculate the required collateral
        (retColl, reqCol) =
          caseShelleyToAlonzoOrBabbageEraOnwards
            (const (TxReturnCollateralNone, TxTotalCollateralNone))
            ( \w' ->
                calcReturnAndTotalCollateral
                  w'
                  fee
                  pparams
                  (txInsCollateral txbodycontent)
                  (txReturnCollateral txbodycontent)
                  (txTotalCollateral txbodycontent)
                  changeaddr
                  (A.mkAdaValue sbe totalPotentialCollateral)
            )
            sbe

    -- Step 5. Now we can calculate the balance of the tx. What matter here are:
    --  1. The original outputs
    --  2. Tx fee
    --  3. Return and total collateral
    txbody2 <-
      first TxFeeEstimationxBodyError $ -- TODO: impossible to fail now
        createTransactionBody
          sbe
          txbodycontent1
            { txFee = TxFeeExplicit sbe fee
            , txReturnCollateral = retColl
            , txTotalCollateral = reqCol
            }

    let fakeUTxO = createFakeUTxO sbe txbodycontent1 $ selectLovelace availableUTxOValue
        balance =
          evaluateTransactionBalance sbe pparams poolids stakeDelegDeposits drepDelegDeposits fakeUTxO txbody2
    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    first TxFeeEstimationBalanceError $ balanceCheck sbe pparams changeaddr balance

    -- Step 6. Check all txouts have the min required UTxO value
    forM_ (txOuts txbodycontent1) $
      \txout -> first TxFeeEstimationBalanceError $ checkMinUTxOValue sbe txout pparams

    -- Step 7.

    -- Create the txbody with the final fee and change output. This should work
    -- provided that the fee and change are less than 2^32-1, and so will
    -- fit within the encoding size we picked above when calculating the fee.
    -- Yes this could be an over-estimate by a few bytes if the fee or change
    -- would fit within 2^16-1. That's a possible optimisation.
    let finalTxBodyContent =
          txbodycontent1
            { txFee = TxFeeExplicit sbe fee
            , txOuts =
                accountForNoChange
                  (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone)
                  (txOuts txbodycontent)
            , txReturnCollateral = retColl
            , txTotalCollateral = reqCol
            }
    txbody3 <-
      first TxFeeEstimationFinalConstructionError $ -- TODO: impossible to fail now. We need to implement a function
      -- that simply creates a transaction body because we have already
      -- validated the transaction body earlier within makeTransactionBodyAutoBalance
        createTransactionBody sbe finalTxBodyContent
    return
      ( BalancedTxBody
          finalTxBodyContent
          txbody3
          (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone)
          fee
      )

--- ----------------------------------------------------------------------------
--- Transaction fees
---

-- | Compute the transaction fee for a proposed transaction, with the
-- assumption that there will be the given number of key witnesses (i.e.
-- signatures).
--
-- Use 'calculateMinTxFee' if possible as that function is more accurate.
evaluateTransactionFee
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxBody era
  -> Word
  -- ^ The number of Shelley key witnesses
  -> Word
  -- ^ The number of Byron key witnesses
  -> Int
  -- ^ Reference script size in bytes
  -> L.Coin
evaluateTransactionFee sbe pp txbody keywitcount byronwitcount refScriptsSize =
  shelleyBasedEraConstraints sbe $
    case makeSignedTransaction' (toCardanoEra sbe) [] txbody of
      ShelleyTx _ tx ->
        L.estimateMinFeeTx pp tx (fromIntegral keywitcount) (fromIntegral byronwitcount) refScriptsSize

-- | Estimate minimum transaction fee for a proposed transaction by looking
-- into the transaction and figuring out how many and what kind of key
-- witnesses this transaction needs.
--
-- It requires access to the portion of the `UTxO` that is relevant for this
-- transaction in order to lookup any txins included in the transaction.
--
-- The only type of witnesses that it cannot figure out reliably is the
-- witnesses needed for satisfying native scripts included in the transaction.
--
-- For this reason number of witnesses needed for native scripts must be
-- supplied as an extra argument.
calculateMinTxFee
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> UTxO era
  -> TxBody era
  -> Word
  -- ^ The number of Shelley key witnesses
  -> L.Coin
calculateMinTxFee sbe pp utxo txbody keywitcount =
  shelleyBasedEraConstraints sbe $
    case makeSignedTransaction' (toCardanoEra sbe) [] txbody of
      ShelleyTx _ tx ->
        L.calcMinFeeTx (toLedgerUTxO sbe utxo) pp tx (fromIntegral keywitcount)

-- | Give an approximate count of the number of key witnesses (i.e. signatures)
-- a transaction will need.
--
-- This is an estimate not a precise count in that it can over-estimate: it
-- makes conservative assumptions such as all inputs are from distinct
-- addresses, but in principle multiple inputs can use the same address and we
-- only need a witness per address.
--
-- Similarly there can be overlap between the regular and collateral inputs,
-- but we conservatively assume they are distinct.
--
-- TODO: it is worth us considering a more precise count that relies on the
-- UTxO to resolve which inputs are for distinct addresses, and also to count
-- the number of Shelley vs Byron style witnesses.
estimateTransactionKeyWitnessCount :: TxBodyContent BuildTx era -> Word
estimateTransactionKeyWitnessCount
  TxBodyContent
    { txIns
    , txInsCollateral
    , txExtraKeyWits
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    } =
    fromIntegral $
      length [() | (_txin, BuildTxWith KeyWitness{}) <- txIns]
        + case txInsCollateral of
          TxInsCollateral _ txins ->
            length txins
          _ -> 0
        + case txExtraKeyWits of
          TxExtraKeyWitnesses _ khs ->
            length khs
          _ -> 0
        + case txWithdrawals of
          TxWithdrawals _ withdrawals ->
            length [() | (_, _, BuildTxWith KeyWitness{}) <- withdrawals]
          _ -> 0
        + case txCertificates of
          TxCertificates _ credWits ->
            length
              [() | (_, BuildTxWith (Just (_, KeyWitness{}))) <- toList credWits]
          _ -> 0
        + case txUpdateProposal of
          TxUpdateProposal _ (UpdateProposal updatePerGenesisKey _) ->
            Map.size updatePerGenesisKey
          _ -> 0

-- ----------------------------------------------------------------------------
-- Script execution units
--

type PlutusScriptBytes = ShortByteString

data ResolvablePointers where
  ResolvablePointers
    :: ( Ledger.Era (ShelleyLedgerEra era)
       , Show (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
       , Show (L.PlutusPurpose L.AsItem (ShelleyLedgerEra era))
       , Show (Alonzo.PlutusScript (ShelleyLedgerEra era))
       )
    => ShelleyBasedEra era
    -> !( Map
            (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
            ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
            , Maybe (PlutusScriptBytes, Plutus.Language)
            , Ledger.ScriptHash Ledger.StandardCrypto
            )
        )
    -> ResolvablePointers

deriving instance Show ResolvablePointers

-- | The different possible reasons that executing a script can fail,
-- as reported by 'evaluateTransactionExecutionUnits'.
--
-- The first three of these are about failures before we even get to execute
-- the script, and two are the result of execution.
--
-- TODO: We should replace ScriptWitnessIndex with ledger's
-- PlutusPurpose AsIx ledgerera. This would necessitate the
-- parameterization of ScriptExecutionError.
data ScriptExecutionError
  = -- | The script depends on a 'TxIn' that has not been provided in the
    -- given 'UTxO' subset. The given 'UTxO' must cover all the inputs
    -- the transaction references.
    ScriptErrorMissingTxIn TxIn
  | -- | The 'TxIn' the script is spending does not have a 'ScriptDatum'.
    -- All inputs guarded by Plutus scripts need to have been created with
    -- a 'ScriptDatum'.
    ScriptErrorTxInWithoutDatum TxIn
  | -- | The 'ScriptDatum' provided does not match the one from the 'UTxO'.
    -- This means the wrong 'ScriptDatum' value has been provided.
    ScriptErrorWrongDatum (Hash ScriptData)
  | -- | The script evaluation failed. This usually means it evaluated to an
    -- error value. This is not a case of running out of execution units
    -- (which is not possible for 'evaluateTransactionExecutionUnits' since
    -- the whole point of it is to discover how many execution units are
    -- needed).
    ScriptErrorEvaluationFailed DebugPlutusFailure
  | -- | The execution units overflowed a 64bit word. Congratulations if
    -- you encounter this error. With the current style of cost model this
    -- would need a script to run for over 7 months, which is somewhat more
    -- than the expected maximum of a few milliseconds.
    ScriptErrorExecutionUnitsOverflow
  | -- | An attempt was made to spend a key witnessed tx input
    -- with a script witness.
    ScriptErrorNotPlutusWitnessedTxIn ScriptWitnessIndex ScriptHash
  | -- | The redeemer pointer points to a script hash that does not exist
    -- in the transaction nor in the UTxO as a reference script"
    ScriptErrorRedeemerPointsToUnknownScriptHash ScriptWitnessIndex
  | -- | A redeemer pointer points to a script that does not exist.
    ScriptErrorMissingScript
      ScriptWitnessIndex -- The invalid pointer
      ResolvablePointers -- A mapping a pointers that are possible to resolve
  | -- | A cost model was missing for a language which was used.
    ScriptErrorMissingCostModel Plutus.Language
  | forall era.
    ( Plutus.EraPlutusContext (ShelleyLedgerEra era)
    , Show (Plutus.ContextError (ShelleyLedgerEra era))
    ) =>
    ScriptErrorTranslationError (Plutus.ContextError (ShelleyLedgerEra era))

deriving instance Show ScriptExecutionError

instance Error ScriptExecutionError where
  prettyError = \case
    ScriptErrorMissingTxIn txin ->
      "The supplied UTxO is missing the txin " <> pretty (renderTxIn txin)
    ScriptErrorTxInWithoutDatum txin ->
      mconcat
        [ "The Plutus script witness for the txin does not have a script datum "
        , "(according to the UTxO). The txin in question is "
        , pretty (renderTxIn txin)
        ]
    ScriptErrorWrongDatum dh ->
      mconcat
        [ "The Plutus script witness has the wrong datum (according to the UTxO). "
        , "The expected datum value has hash " <> pshow dh
        ]
    ScriptErrorEvaluationFailed plutusDebugFailure ->
      pretty $ renderDebugPlutusFailure plutusDebugFailure
    ScriptErrorExecutionUnitsOverflow ->
      mconcat
        [ "The execution units required by this Plutus script overflows a 64bit "
        , "word. In a properly configured chain this should be practically "
        , "impossible. So this probably indicates a chain configuration problem, "
        , "perhaps with the values in the cost model."
        ]
    ScriptErrorNotPlutusWitnessedTxIn scriptWitness scriptHash ->
      mconcat
        [ pretty (renderScriptWitnessIndex scriptWitness)
        , " is not a Plutus script witnessed tx input and cannot be spent using a "
        , "Plutus script witness.The script hash is " <> pshow scriptHash <> "."
        ]
    ScriptErrorRedeemerPointsToUnknownScriptHash scriptWitness ->
      mconcat
        [ pretty (renderScriptWitnessIndex scriptWitness)
        , " points to a script hash that is not known."
        ]
    ScriptErrorMissingScript rdmrPtr resolveable ->
      mconcat
        [ "The redeemer pointer: " <> pshow rdmrPtr <> " points to a Plutus "
        , "script that does not exist.\n"
        , "The pointers that can be resolved are: " <> pshow resolveable
        ]
    ScriptErrorMissingCostModel language ->
      "No cost model was found for language " <> pshow language
    ScriptErrorTranslationError e ->
      "Error translating the transaction context: " <> pshow e

data TransactionValidityError era where
  -- | The transaction validity interval is too far into the future.
  --
  -- Transactions with Plutus scripts need to have a validity interval that is
  -- not so far in the future that we cannot reliably determine the UTC time
  -- corresponding to the validity interval expressed in slot numbers.
  --
  -- This is because the Plutus scripts get given the transaction validity
  -- interval in UTC time, so that they are not sensitive to slot lengths.
  --
  -- If either end of the validity interval is beyond the so called \"time
  -- horizon\" then the consensus algorithm is not able to reliably determine
  -- the relationship between slots and time. This is this situation in which
  -- this error is reported. For the Cardano mainnet the time horizon is 36
  -- hours beyond the current time. This effectively means we cannot submit
  -- check or submit transactions that use Plutus scripts that have the end
  -- of their validity interval more than 36 hours into the future.
  TransactionValidityIntervalError
    :: Consensus.PastHorizonException -> TransactionValidityError era
  TransactionValidityCostModelError
    :: (Map AnyPlutusScriptVersion CostModel) -> String -> TransactionValidityError era

deriving instance Show (TransactionValidityError era)

instance Error (TransactionValidityError era) where
  prettyError = \case
    TransactionValidityIntervalError pastTimeHorizon ->
      mconcat
        [ "The transaction validity interval is too far in the future. "
        , "For this network it must not be more than "
        , pretty (timeHorizonSlots pastTimeHorizon)
        , "slots ahead of the current time slot. "
        , "(Transactions with Plutus scripts must have validity intervals that "
        , "are close enough in the future that we can reliably turn the slot "
        , "numbers into UTC wall clock times.)"
        ]
     where
      timeHorizonSlots :: Consensus.PastHorizonException -> Word
      timeHorizonSlots Consensus.PastHorizon{Consensus.pastHorizonSummary}
        | eraSummaries@(_ : _) <- pastHorizonSummary
        , Consensus.StandardSafeZone slots <-
            (Consensus.eraSafeZone . Consensus.eraParams . last) eraSummaries =
            fromIntegral slots
        | otherwise =
            0 -- This should be impossible.
    TransactionValidityCostModelError cModels err ->
      mconcat
        [ "An error occurred while converting from the cardano-api cost"
        , " models to the cardano-ledger cost models. Error: " <> pretty err
        , " Cost models: " <> pshow cModels
        ]

-- | Compute the 'ExecutionUnits' needed for each script in the transaction.
--
-- This works by running all the scripts and counting how many execution units
-- are actually used.
evaluateTransactionExecutionUnits
  :: forall era
   . ()
  => CardanoEra era
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters era
  -> UTxO era
  -> TxBody era
  -> Either
       (TransactionValidityError era)
       (Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits)))
evaluateTransactionExecutionUnits era systemstart epochInfo pp utxo txbody =
  case makeSignedTransaction' era [] txbody of
    ShelleyTx sbe tx' -> evaluateTransactionExecutionUnitsShelley sbe systemstart epochInfo pp utxo tx'

evaluateTransactionExecutionUnitsShelley
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters era
  -> UTxO era
  -> L.Tx (ShelleyLedgerEra era)
  -> Either
       (TransactionValidityError era)
       (Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits)))
evaluateTransactionExecutionUnitsShelley sbe systemstart epochInfo (LedgerProtocolParameters pp) utxo tx =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const (Right Map.empty))
    ( \w ->
        pure . fromLedgerScriptExUnitsMap w $
          alonzoEraOnwardsConstraints w $
            L.evalTxExUnitsWithLogs pp tx (toLedgerUTxO sbe utxo) ledgerEpochInfo systemstart
    )
    sbe
 where
  LedgerEpochInfo ledgerEpochInfo = epochInfo

  fromLedgerScriptExUnitsMap
    :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
    => AlonzoEraOnwards era
    -> Map
         (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
         (Either (L.TransactionScriptFailure (ShelleyLedgerEra era)) (EvalTxExecutionUnitsLog, Alonzo.ExUnits))
    -> Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
  fromLedgerScriptExUnitsMap aOnwards exmap =
    fromList
      [ ( toScriptIndex aOnwards rdmrptr
        , bimap (fromAlonzoScriptExecutionError aOnwards) (second fromAlonzoExUnits) exunitsOrFailure
        )
      | (rdmrptr, exunitsOrFailure) <- toList exmap
      ]

  fromAlonzoScriptExecutionError
    :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
    => AlonzoEraOnwards era
    -> L.TransactionScriptFailure (ShelleyLedgerEra era)
    -> ScriptExecutionError
  fromAlonzoScriptExecutionError aOnwards =
    shelleyBasedEraConstraints sbe $ \case
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
        ScriptErrorRedeemerPointsToUnknownScriptHash $ toScriptIndex aOnwards rdmrPtr
      -- This should not occur while using cardano-cli because we zip together
      -- the Plutus script and the use site (txin, certificate etc). Therefore
      -- the redeemer pointer will always point to a Plutus script.
      L.MissingScript indexOfScriptWitnessedItem resolveable ->
        let scriptWitnessedItemIndex = toScriptIndex aOnwards indexOfScriptWitnessedItem
         in ScriptErrorMissingScript scriptWitnessedItemIndex $
              ResolvablePointers sbe $
                Map.map extractScriptBytesAndLanguage resolveable
      L.NoCostModelInLedgerState l -> ScriptErrorMissingCostModel l
      L.ContextError e ->
        alonzoEraOnwardsConstraints aOnwards $
          ScriptErrorTranslationError e

extractScriptBytesAndLanguage
  :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
  => ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
     , Maybe (Alonzo.PlutusScript (ShelleyLedgerEra era))
     , L.ScriptHash Ledger.StandardCrypto
     )
  -> ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
     , Maybe (PlutusScriptBytes, Plutus.Language)
     , Ledger.ScriptHash Ledger.StandardCrypto
     )
extractScriptBytesAndLanguage (purpose, mbScript, scriptHash) =
  (purpose, fmap extractPlutusScriptAndLanguage mbScript, scriptHash)

extractPlutusScriptAndLanguage
  :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
  => Alonzo.PlutusScript (ShelleyLedgerEra era)
  -> (PlutusScriptBytes, Plutus.Language)
extractPlutusScriptAndLanguage p =
  let bin = Plutus.unPlutusBinary $ Alonzo.plutusScriptBinary p
      l = Alonzo.plutusScriptLanguage p
   in (bin, l)

-- ----------------------------------------------------------------------------
-- Transaction balance
--

-- | Compute the total balance of the proposed transaction. Ultimately a valid
-- transaction must be fully balanced: that is have a total value of zero.
--
-- Finding the (non-zero) balance of partially constructed transaction is
-- useful for adjusting a transaction to be fully balanced.
evaluateTransactionBalance
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> Set PoolId
  -> Map StakeCredential L.Coin
  -> Map (Ledger.Credential Ledger.DRepRole Ledger.StandardCrypto) L.Coin
  -> UTxO era
  -> TxBody era
  -> TxOutValue era
evaluateTransactionBalance sbe pp poolids stakeDelegDeposits drepDelegDeposits utxo (ShelleyTxBody _ txbody _ _ _ _) =
  shelleyBasedEraConstraints sbe $
    TxOutValueShelleyBased sbe $
      L.evalBalanceTxBody
        pp
        lookupDelegDeposit
        lookupDRepDeposit
        isRegPool
        (toLedgerUTxO sbe utxo)
        txbody
 where
  isRegPool :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> Bool
  isRegPool kh = StakePoolKeyHash kh `Set.member` poolids

  lookupDelegDeposit
    :: Ledger.Credential 'Ledger.Staking L.StandardCrypto -> Maybe L.Coin
  lookupDelegDeposit stakeCred =
    Map.lookup (fromShelleyStakeCredential stakeCred) stakeDelegDeposits

  lookupDRepDeposit
    :: Ledger.Credential 'Ledger.DRepRole L.StandardCrypto -> Maybe L.Coin
  lookupDRepDeposit drepCred =
    Map.lookup drepCred drepDelegDeposits

-- ----------------------------------------------------------------------------
-- Automated transaction building
--

-- | The possible errors that can arise from 'makeTransactionBodyAutoBalance'.
data TxBodyErrorAutoBalance era
  = -- | The same errors that can arise from 'makeTransactionBody'.
    TxBodyError TxBodyError
  | -- | One or more of the scripts fails to execute correctly.
    TxBodyScriptExecutionError [(ScriptWitnessIndex, ScriptExecutionError)]
  | -- | One or more of the scripts were expected to fail validation, but none did.
    TxBodyScriptBadScriptValidity
  | -- | There is not enough ada to cover both the outputs and the fees.
    -- The transaction should be changed to provide more input ada, or
    -- otherwise adjusted to need less (e.g. outputs, script etc).
    TxBodyErrorAdaBalanceNegative L.Coin
  | -- | There is enough ada to cover both the outputs and the fees, but the
    -- resulting change is too small: it is under the minimum value for
    -- new UTxO entries. The transaction should be changed to provide more
    -- input ada.
    TxBodyErrorAdaBalanceTooSmall
      -- \^ Offending TxOut
      TxOutInAnyEra
      -- ^ Minimum UTxO
      L.Coin
      -- ^ Tx balance
      L.Coin
  | -- | 'makeTransactionBodyAutoBalance' does not yet support the Byron era.
    TxBodyErrorByronEraNotSupported
  | -- | The 'ProtocolParameters' must provide the value for the min utxo
    -- parameter, for eras that use this parameter.
    TxBodyErrorMissingParamMinUTxO
  | -- | The transaction validity interval is too far into the future.
    -- See 'TransactionValidityIntervalError' for details.
    TxBodyErrorValidityInterval (TransactionValidityError era)
  | -- | The minimum spendable UTxO threshold has not been met.
    TxBodyErrorMinUTxONotMet
      -- \^ Offending TxOut
      TxOutInAnyEra
      -- ^ Minimum UTxO
      L.Coin
  | TxBodyErrorNonAdaAssetsUnbalanced Value
  | TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap
      ScriptWitnessIndex
      (Map ScriptWitnessIndex ExecutionUnits)
  deriving Show

instance Error (TxBodyErrorAutoBalance era) where
  prettyError = \case
    TxBodyError err ->
      prettyError err
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
    TxBodyErrorAdaBalanceNegative lovelace ->
      mconcat
        [ "The transaction does not balance in its use of ada. The net balance "
        , "of the transaction is negative: " <> pretty lovelace <> ". "
        , "The usual solution is to provide more inputs, or inputs with more ada."
        ]
    TxBodyErrorAdaBalanceTooSmall changeOutput minUTxO balance ->
      mconcat
        [ "The transaction does balance in its use of ada, however the net "
        , "balance does not meet the minimum UTxO threshold. \n"
        , "Balance: " <> pretty balance <> "\n"
        , "Offending output (change output): " <> pretty (prettyRenderTxOut changeOutput) <> "\n"
        , "Minimum UTxO threshold: " <> pretty minUTxO <> "\n"
        , "The usual solution is to provide more inputs, or inputs with more ada to "
        , "meet the minimum UTxO threshold"
        ]
    TxBodyErrorByronEraNotSupported ->
      "The Byron era is not yet supported by makeTransactionBodyAutoBalance"
    TxBodyErrorMissingParamMinUTxO ->
      "The minUTxOValue protocol parameter is required but missing"
    TxBodyErrorValidityInterval err ->
      prettyError err
    TxBodyErrorMinUTxONotMet txout minUTxO ->
      mconcat
        [ "Minimum UTxO threshold not met for tx output: " <> pretty (prettyRenderTxOut txout) <> "\n"
        , "Minimum required UTxO: " <> pretty minUTxO
        ]
    TxBodyErrorNonAdaAssetsUnbalanced val ->
      "Non-Ada assets are unbalanced: " <> pretty (renderValue val)
    TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap sIndex eUnitsMap ->
      mconcat
        [ "ScriptWitnessIndex (redeemer pointer): " <> pshow sIndex <> " is missing from the execution "
        , "units (redeemer pointer) map: " <> pshow eUnitsMap
        ]

handleExUnitsErrors
  :: ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> Map ScriptWitnessIndex ScriptExecutionError
  -> Map ScriptWitnessIndex ExecutionUnits
  -> Either (TxBodyErrorAutoBalance era) (Map ScriptWitnessIndex ExecutionUnits)
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

data BalancedTxBody era
  = BalancedTxBody
      (TxBodyContent BuildTx era)
      (TxBody era)
      (TxOut CtxTx era)
      -- ^ Transaction balance (change output)
      L.Coin
      -- ^ Estimated transaction fee
  deriving Show

newtype RequiredShelleyKeyWitnesses
  = RequiredShelleyKeyWitnesses {unRequiredShelleyKeyWitnesses :: Int}
  deriving Show

newtype RequiredByronKeyWitnesses
  = RequiredByronKeyWitnesses {unRequiredByronKeyWitnesses :: Int}
  deriving Show

newtype TotalReferenceScriptsSize
  = TotalReferenceScriptsSize {unTotalReferenceScriptsSize :: Int}
  deriving Show

data FeeEstimationMode era
  = -- | Accurate fee calculation.
    CalculateWithSpendableUTxO
      (UTxO era)
      -- ^ Spendable UTxO
      SystemStart
      LedgerEpochInfo
      (Maybe Word)
      -- ^ Override number of key witnesses
  | -- | Less accurate fee estimation.
    EstimateWithoutSpendableUTxO
      Coin
      -- ^ Total potential collateral amount
      Value
      -- ^ Total value of UTxOs being spent
      (Map ScriptWitnessIndex ExecutionUnits)
      -- ^ Plutus script execution units
      RequiredShelleyKeyWitnesses
      -- ^ The number of key witnesses still to be added to the transaction.
      RequiredByronKeyWitnesses
      -- ^ The number of Byron key witnesses still to be added to the transaction.
      TotalReferenceScriptsSize
      -- ^ The total size in bytes of reference scripts

-- | This is much like 'makeTransactionBody' but with greater automation to
-- calculate suitable values for several things.
--
-- In particular:
--
-- * It calculates the correct script 'ExecutionUnits' (ignoring the provided
--   values, which can thus be zero).
--
-- * It calculates the transaction fees, based on the script 'ExecutionUnits',
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
-- To do this it needs more information than 'makeTransactionBody', all of
-- which can be queried from a local node.
makeTransactionBodyAutoBalance
  :: forall era
   . ()
  => HasCallStack
  => ShelleyBasedEra era
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters era
  -> Set PoolId
  -- ^ The set of registered stake pools, that are being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ Map of all deposits for stake credentials that are being
  --   unregistered in this transaction
  -> Map (Ledger.Credential Ledger.DRepRole Ledger.StandardCrypto) L.Coin
  -- ^ Map of all deposits for drep credentials that are being
  --   unregistered in this transaction
  -> UTxO era
  -- ^ Just the transaction inputs (including reference and collateral ones), not the entire 'UTxO'.
  -> TxBodyContent BuildTx era
  -> AddressInEra era
  -- ^ Change address
  -> Maybe Word
  -- ^ Override key witnesses
  -> Either (TxBodyErrorAutoBalance era) (BalancedTxBody era)
makeTransactionBodyAutoBalance
  sbe
  systemstart
  history
  lpp@(LedgerProtocolParameters pp)
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  utxo
  txbodycontent
  changeaddr
  mnkeys =
    shelleyBasedEraConstraints sbe $ do
      -- Our strategy is to:
      -- 1. evaluate all the scripts to get the exec units, update with ex units
      -- 2. figure out the overall min fees
      -- 3. update tx with fees
      -- 4. balance the transaction and update tx change output

      txbodyForChange <- first TxBodyError $ createTransactionBody sbe txbodycontent
      let initialChangeTxOut =
            evaluateTransactionBalance sbe pp poolids stakeDelegDeposits drepDelegDeposits utxo txbodyForChange

      -- Tx body used only for evaluating execution units. Because txout exact
      -- values do not matter much here, we are using an initial change value,
      -- which is slightly overestimated, because it does not include fee or
      -- scripts execution costs.
      txbody <-
        first TxBodyError
          $ createTransactionBody
            sbe
          $ txbodycontent
            & modTxOuts
              (<> [TxOut changeaddr initialChangeTxOut TxOutDatumNone ReferenceScriptNone])
      exUnitsMapWithLogs <-
        first TxBodyErrorValidityInterval $
          evaluateTransactionExecutionUnits
            era
            systemstart
            history
            lpp
            utxo
            txbody

      let exUnitsMap = Map.map (fmap snd) exUnitsMapWithLogs

      exUnitsMap' <-
        case Map.mapEither id exUnitsMap of
          (failures, exUnitsMap') ->
            handleExUnitsErrors
              (txScriptValidityToScriptValidity (txScriptValidity txbodycontent))
              failures
              exUnitsMap'

      txbodycontent1 <- substituteExecutionUnits exUnitsMap' txbodycontent

      -- Make a txbody that we will use for calculating the fees. For the purpose
      -- of fees we just need to make a txbody of the right size in bytes. We
      -- do not need the right values for the fee. We use "big enough" value
      -- for the fee and set so that the CBOR encoding size of the tx will be
      -- big enough to cover the size of the final output and fee. Yes this
      -- means this current code will only work for final fee of less than
      -- around 4000 ada (2^32-1 lovelace).
      let maxLovelaceFee = L.Coin (2 ^ (32 :: Integer) - 1)
      -- Make a txbody that we will use for calculating the fees.
      let (dummyCollRet, dummyTotColl) = maybeDummyTotalCollAndCollReturnOutput sbe txbodycontent changeaddr
      txbody1 <-
        first TxBodyError $ -- TODO: impossible to fail now
          createTransactionBody
            sbe
            txbodycontent1
              { txFee = TxFeeExplicit sbe maxLovelaceFee
              , txOuts =
                  txOuts txbodycontent
                    <> [TxOut changeaddr initialChangeTxOut TxOutDatumNone ReferenceScriptNone]
              , txReturnCollateral = dummyCollRet
              , txTotalCollateral = dummyTotColl
              }
      -- NB: This has the potential to over estimate the fees because estimateTransactionKeyWitnessCount
      -- makes the conservative assumption that all inputs are from distinct
      -- addresses.
      let nkeys =
            fromMaybe
              (estimateTransactionKeyWitnessCount txbodycontent1)
              mnkeys
          fee = calculateMinTxFee sbe pp utxo txbody1 nkeys
          (retColl, reqCol) =
            caseShelleyToAlonzoOrBabbageEraOnwards
              (const (TxReturnCollateralNone, TxTotalCollateralNone))
              ( \w -> do
                  let totalPotentialCollateral =
                        mconcat
                          [ txOutValue
                          | TxInsCollateral _ collInputs <- pure $ txInsCollateral txbodycontent
                          , collTxIn <- collInputs
                          , Just (TxOut _ (TxOutValueShelleyBased _ txOutValue) _ _) <- pure $ Map.lookup collTxIn (unUTxO utxo)
                          ]
                  calcReturnAndTotalCollateral
                    w
                    fee
                    pp
                    (txInsCollateral txbodycontent)
                    (txReturnCollateral txbodycontent)
                    (txTotalCollateral txbodycontent)
                    changeaddr
                    totalPotentialCollateral
              )
              sbe

      -- Make a txbody for calculating the balance. For this the size of the tx
      -- does not matter, instead it's just the values of the fee and outputs.
      -- Here we do not want to start with any change output, since that's what
      -- we need to calculate.
      txbody2 <-
        first TxBodyError $ -- TODO: impossible to fail now
          createTransactionBody
            sbe
            txbodycontent1
              { txFee = TxFeeExplicit sbe fee
              , txReturnCollateral = retColl
              , txTotalCollateral = reqCol
              }
      let balance = evaluateTransactionBalance sbe pp poolids stakeDelegDeposits drepDelegDeposits utxo txbody2
      forM_ (txOuts txbodycontent1) $ \txout -> checkMinUTxOValue sbe txout pp

      -- check if the balance is positive or negative
      -- in one case we can produce change, in the other the inputs are insufficient
      balanceCheck sbe pp changeaddr balance

      -- TODO: we could add the extra fee for the CBOR encoding of the change,
      -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.
      -- The txbody with the final fee and change output. This should work
      -- provided that the fee and change are less than 2^32-1, and so will
      -- fit within the encoding size we picked above when calculating the fee.
      -- Yes this could be an over-estimate by a few bytes if the fee or change
      -- would fit within 2^16-1. That's a possible optimisation.
      let finalTxBodyContent =
            txbodycontent1
              { txFee = TxFeeExplicit sbe fee
              , txOuts =
                  accountForNoChange
                    (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone)
                    (txOuts txbodycontent)
              , txReturnCollateral = retColl
              , txTotalCollateral = reqCol
              }
      txbody3 <-
        first TxBodyError $ -- TODO: impossible to fail now. We need to implement a function
        -- that simply creates a transaction body because we have already
        -- validated the transaction body earlier within makeTransactionBodyAutoBalance
          createTransactionBody sbe finalTxBodyContent
      return
        ( BalancedTxBody
            finalTxBodyContent
            txbody3
            (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone)
            fee
        )
   where
    era :: CardanoEra era
    era = toCardanoEra sbe

-- | In the event of spending the exact amount of lovelace in
-- the specified input(s), this function excludes the change
-- output. Note that this does not save any fees because by default
-- the fee calculation includes a change address for simplicity and
-- we make no attempt to recalculate the tx fee without a change address.
accountForNoChange :: TxOut CtxTx era -> [TxOut CtxTx era] -> [TxOut CtxTx era]
accountForNoChange change@(TxOut _ balance _ _) rest =
  case txOutValueToLovelace balance of
    L.Coin 0 -> rest
    -- We append change at the end so a client can predict the indexes
    -- of the outputs
    _ -> rest ++ [change]

checkMinUTxOValue
  :: ShelleyBasedEra era
  -> TxOut CtxTx era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> Either (TxBodyErrorAutoBalance era) ()
checkMinUTxOValue sbe txout@(TxOut _ v _ _) bpp = do
  let minUTxO = calculateMinimumUTxO sbe txout bpp
  if txOutValueToLovelace v >= minUTxO
    then Right ()
    else Left $ TxBodyErrorMinUTxONotMet (txOutInAnyEra (toCardanoEra sbe) txout) minUTxO

balanceCheck
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> AddressInEra era
  -> TxOutValue era
  -> Either (TxBodyErrorAutoBalance era) ()
balanceCheck sbe bpparams changeaddr balance
  | txOutValueToLovelace balance == 0 && onlyAda (txOutValueToValue balance) = return ()
  | txOutValueToLovelace balance < 0 =
      Left . TxBodyErrorAdaBalanceNegative $ txOutValueToLovelace balance
  | otherwise =
      case checkMinUTxOValue sbe (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone) bpparams of
        Left (TxBodyErrorMinUTxONotMet txOutAny minUTxO) ->
          Left $ TxBodyErrorAdaBalanceTooSmall txOutAny minUTxO (txOutValueToLovelace balance)
        Left err -> Left err
        Right _ -> Right ()

isNotAda :: AssetId -> Bool
isNotAda AdaAssetId = False
isNotAda _ = True

onlyAda :: Value -> Bool
onlyAda = null . toList . filterValue isNotAda

-- Calculation taken from validateInsufficientCollateral:
-- https://github.com/input-output-hk/cardano-ledger/blob/389b266d6226dedf3d2aec7af640b3ca4984c5ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxo.hs#L335
-- TODO: Bug Jared to expose a function from the ledger that returns total and
-- return collateral.
calcReturnAndTotalCollateral
  :: ()
  => Ledger.AlonzoEraPParams (ShelleyLedgerEra era)
  => BabbageEraOnwards era
  -> L.Coin
  -- ^ Fee
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxInsCollateral era
  -- ^ From the initial TxBodyContent
  -> TxReturnCollateral CtxTx era
  -- ^ From the initial TxBodyContent
  -> TxTotalCollateral era
  -- ^ From the initial TxBodyContent
  -> AddressInEra era
  -- ^ Change address
  -> L.Value (ShelleyLedgerEra era)
  -- ^ Total available collateral (can include non-ada)
  -> (TxReturnCollateral CtxTx era, TxTotalCollateral era)
calcReturnAndTotalCollateral _ _ _ TxInsCollateralNone _ _ _ _ = (TxReturnCollateralNone, TxTotalCollateralNone)
calcReturnAndTotalCollateral w fee pp' TxInsCollateral{} txReturnCollateral txTotalCollateral cAddr totalAvailableCollateral = babbageEraOnwardsConstraints w $ do
  let sbe = convert w
      colPerc = pp' ^. Ledger.ppCollateralPercentageL
      -- We must first figure out how much lovelace we have committed
      -- as collateral and we must determine if we have enough lovelace at our
      -- collateral tx inputs to cover the tx
      totalCollateralLovelace = totalAvailableCollateral ^. A.adaAssetL sbe
      requiredCollateral@(L.Coin reqAmt) = fromIntegral colPerc * fee
      totalCollateral =
        TxTotalCollateral w . L.rationalToCoinViaCeiling $
          reqAmt % 100
      -- Why * 100? requiredCollateral is the product of the collateral percentage and the tx fee
      -- We choose to multiply 100 rather than divide by 100 to make the calculation
      -- easier to manage. At the end of the calculation we then use % 100 to perform our division
      -- and round the returnCollateral down which has the effect of potentially slightly
      -- overestimating the required collateral.
      L.Coin returnCollateralAmount = totalCollateralLovelace * 100 - requiredCollateral
      returnAdaCollateral = A.mkAdaValue sbe $ L.rationalToCoinViaFloor $ returnCollateralAmount % 100
      -- non-ada collateral is not used, so just return it as is in the return collateral output
      nonAdaCollateral = L.modifyCoin (const mempty) totalAvailableCollateral
      returnCollateral = returnAdaCollateral <> nonAdaCollateral
  case (txReturnCollateral, txTotalCollateral) of
    (rc@TxReturnCollateral{}, tc@TxTotalCollateral{}) ->
      (rc, tc)
    (rc@TxReturnCollateral{}, TxTotalCollateralNone) ->
      (rc, TxTotalCollateralNone)
    (TxReturnCollateralNone, tc@TxTotalCollateral{}) ->
      (TxReturnCollateralNone, tc)
    (TxReturnCollateralNone, TxTotalCollateralNone)
      | returnCollateralAmount < 0 ->
          (TxReturnCollateralNone, TxTotalCollateralNone)
      | otherwise ->
          ( TxReturnCollateral
              w
              ( TxOut
                  cAddr
                  (TxOutValueShelleyBased sbe returnCollateral)
                  TxOutDatumNone
                  ReferenceScriptNone
              )
          , totalCollateral
          )

-- | Calculate the partial change - this does not include certificates' deposits
calculatePartialChangeValue
  :: ShelleyBasedEra era
  -> Value
  -> TxBodyContent build era
  -> Value
calculatePartialChangeValue sbe incoming txbodycontent = do
  let outgoing = newUtxoValue
      mintedValue = txMintValueToValue $ txMintValue txbodycontent
  mconcat [incoming, mintedValue, negateValue outgoing]
 where
  newUtxoValue =
    mconcat [fromLedgerValue sbe v | (TxOut _ (TxOutValueShelleyBased _ v) _ _) <- txOuts txbodycontent]

-- | This is used in the balance calculation in the event where
-- the user does not supply the UTxO(s) they intend to spend
-- but they must supply their total balance of ADA.
-- evaluateTransactionBalance calls evalBalanceTxBody which requires a UTxO value.
-- This eventually calls getConsumedMaryValue which retrieves the balance
-- from the transaction itself. This necessitated a function to create a "fake" UTxO
-- to still use evalBalanceTxBody however this will fail for transactions
-- containing multi-assets, refunds and withdrawals.
-- TODO: Include multiassets
createFakeUTxO :: ShelleyBasedEra era -> TxBodyContent BuildTx era -> Coin -> UTxO era
createFakeUTxO sbe txbodycontent totalAdaInUTxO =
  let singleTxIn = maybe [] (return . fst) $ List.uncons [txin | (txin, _) <- txIns txbodycontent]
      singleTxOut =
        maybe [] (return . updateTxOut sbe totalAdaInUTxO . toCtxUTxOTxOut . fst) $
          List.uncons $
            txOuts txbodycontent
   in -- Take one txin and one txout. Replace the out value with totalAdaInUTxO
      -- Return an empty UTxO if there are no txins or txouts
      UTxO $ fromList $ zip singleTxIn singleTxOut

updateTxOut :: ShelleyBasedEra era -> Coin -> TxOut CtxUTxO era -> TxOut CtxUTxO era
updateTxOut sbe updatedValue txout =
  let ledgerout = shelleyBasedEraConstraints sbe $ toShelleyTxOut sbe txout & L.coinTxOutL .~ updatedValue
   in fromShelleyTxOut sbe ledgerout

-- Essentially we check for the existence of collateral inputs. If they exist we
-- create a fictitious collateral return output. Why? Because we need to put dummy values
-- to get a fee estimate (i.e we overestimate the fee). The required collateral depends
-- on the tx fee as per the Alonzo spec.
maybeDummyTotalCollAndCollReturnOutput
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> AddressInEra era
  -> (TxReturnCollateral CtxTx era, TxTotalCollateral era)
maybeDummyTotalCollAndCollReturnOutput sbe TxBodyContent{txInsCollateral, txReturnCollateral, txTotalCollateral} cAddr =
  case txInsCollateral of
    TxInsCollateralNone -> (TxReturnCollateralNone, TxTotalCollateralNone)
    TxInsCollateral{} ->
      forShelleyBasedEraInEon
        sbe
        (TxReturnCollateralNone, TxTotalCollateralNone)
        ( \w ->
            let dummyRetCol =
                  TxReturnCollateral
                    w
                    ( TxOut
                        cAddr
                        (lovelaceToTxOutValue sbe $ L.Coin (2 ^ (64 :: Integer)) - 1)
                        TxOutDatumNone
                        ReferenceScriptNone
                    )
                dummyTotCol = TxTotalCollateral w (L.Coin (2 ^ (32 :: Integer) - 1))
             in case (txReturnCollateral, txTotalCollateral) of
                  (rc@TxReturnCollateral{}, tc@TxTotalCollateral{}) -> (rc, tc)
                  (rc@TxReturnCollateral{}, TxTotalCollateralNone) -> (rc, dummyTotCol)
                  (TxReturnCollateralNone, tc@TxTotalCollateral{}) -> (dummyRetCol, tc)
                  (TxReturnCollateralNone, TxTotalCollateralNone) -> (dummyRetCol, dummyTotCol)
        )

substituteExecutionUnits
  :: forall era
   . Map ScriptWitnessIndex ExecutionUnits
  -> TxBodyContent BuildTx era
  -> Either (TxBodyErrorAutoBalance era) (TxBodyContent BuildTx era)
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
                    _
                    txWithdrawals
                    txCertificates
                    _
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
        & setTxMintValue mappedMintedVals
        & setTxCertificates mappedTxCertificates
        & setTxWithdrawals mappedWithdrawals
        & setTxVotingProcedures mappedVotes
        & setTxProposalProcedures mappedProposals
   where
    substituteExecUnits
      :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> Either (TxBodyErrorAutoBalance era) (ScriptWitness witctx era)
    substituteExecUnits _ wit@SimpleScriptWitness{} = Right wit
    substituteExecUnits idx (PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits ->
          Right $
            PlutusScriptWitness
              langInEra
              version
              script
              datum
              redeemer
              exunits

    adjustScriptWitness
      :: (ScriptWitness witctx era -> Either (TxBodyErrorAutoBalance era) (ScriptWitness witctx era))
      -> Witness witctx era
      -> Either (TxBodyErrorAutoBalance era) (Witness witctx era)
    adjustScriptWitness _ (KeyWitness ctx) = Right $ KeyWitness ctx
    adjustScriptWitness g (ScriptWitness ctx witness') = ScriptWitness ctx <$> g witness'

    mapScriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> Either (TxBodyErrorAutoBalance era) [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( TxIn
                 , Either (TxBodyErrorAutoBalance era) (BuildTxWith BuildTx (Witness WitCtxTxIn era))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, BuildTxWith <$> wit')
            | (ix, txin, wit) <- indexTxIns txins
            , let wit' = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in traverse
            (\(txIn, eWitness) -> (txIn,) <$> eWitness)
            mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxWithdrawals BuildTx era)
    mapScriptWitnessesWithdrawals TxWithdrawalsNone = Right TxWithdrawalsNone
    mapScriptWitnessesWithdrawals txWithdrawals'@(TxWithdrawals supported _) =
      let mappedWithdrawals
            :: [ ( StakeAddress
                 , L.Coin
                 , Either (TxBodyErrorAutoBalance era) (BuildTxWith BuildTx (Witness WitCtxStake era))
                 )
               ]
          mappedWithdrawals =
            [ (addr, withdrawal, BuildTxWith <$> mappedWitness)
            | (ix, addr, withdrawal, wit) <- indexTxWithdrawals txWithdrawals'
            , let mappedWitness = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in TxWithdrawals supported
            <$> traverse
              (\(sAddr, ll, eWitness) -> (sAddr,ll,) <$> eWitness)
              mappedWithdrawals

    mapScriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxCertificates BuildTx era)
    mapScriptWitnessesCertificates TxCertificatesNone = Right TxCertificatesNone
    mapScriptWitnessesCertificates txCertificates'@(TxCertificates supported _) = do
      let mappedScriptWitnesses
            :: [ ( Certificate era
                 , Either
                     (TxBodyErrorAutoBalance era)
                     ( BuildTxWith
                         BuildTx
                         ( Maybe
                             ( StakeCredential
                             , Witness WitCtxStake era
                             )
                         )
                     )
                 )
               ]
          mappedScriptWitnesses =
            [ (cert, BuildTxWith . Just . (stakeCred,) <$> eWitness')
            | (ix, cert, stakeCred, witness) <- indexTxCertificates txCertificates'
            , let eWitness' = adjustScriptWitness (substituteExecUnits ix) witness
            ]
      TxCertificates supported . fromList <$> traverseScriptWitnesses mappedScriptWitnesses

    mapScriptWitnessesVotes
      :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
      -> Either
           (TxBodyErrorAutoBalance era)
           (Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era)))
    mapScriptWitnessesVotes Nothing = return Nothing
    mapScriptWitnessesVotes (Just (Featured _ TxVotingProceduresNone)) = return Nothing
    mapScriptWitnessesVotes (Just (Featured _ (TxVotingProcedures _ ViewTx))) = return Nothing
    mapScriptWitnessesVotes (Just (Featured era txVotingProcedures'@(TxVotingProcedures vProcedures (BuildTxWith _)))) = do
      let eSubstitutedExecutionUnits =
            [ (vote, updatedWitness)
            | (ix, vote, witness) <- indexTxVotingProcedures txVotingProcedures'
            , let updatedWitness = substituteExecUnits ix witness
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return $
        Just
          (Featured era (TxVotingProcedures vProcedures (BuildTxWith $ fromList substitutedExecutionUnits)))

    mapScriptWitnessesProposals
      :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era))
      -> Either
           (TxBodyErrorAutoBalance era)
           (Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era)))
    mapScriptWitnessesProposals Nothing = return Nothing
    mapScriptWitnessesProposals (Just (Featured _ TxProposalProceduresNone)) = return Nothing
    mapScriptWitnessesProposals (Just (Featured _ (TxProposalProcedures _ ViewTx))) = return Nothing
    mapScriptWitnessesProposals (Just (Featured era txpp@(TxProposalProcedures osetProposalProcedures (BuildTxWith _)))) = do
      let eSubstitutedExecutionUnits =
            [ (proposal, updatedWitness)
            | (ix, proposal, scriptWitness) <- indexTxProposalProcedures txpp
            , let updatedWitness = substituteExecUnits ix scriptWitness
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return $
        Just
          ( Featured
              era
              (TxProposalProcedures osetProposalProcedures (BuildTxWith $ fromList substitutedExecutionUnits))
          )

    mapScriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxMintValue BuildTx era)
    mapScriptWitnessesMinting TxMintNone = Right TxMintNone
    mapScriptWitnessesMinting txMintValue'@(TxMintValue w _) = do
      let mappedScriptWitnesses =
            [ (policyId, pure . (assetName',quantity,) <$> substitutedWitness)
            | (ix, policyId, assetName', quantity, BuildTxWith witness) <- indexTxMintValue txMintValue'
            , let substitutedWitness = BuildTxWith <$> substituteExecUnits ix witness
            ]
      final <- Map.fromListWith (<>) <$> traverseScriptWitnesses mappedScriptWitnesses
      pure $ TxMintValue w final

traverseScriptWitnesses
  :: [(a, Either (TxBodyErrorAutoBalance era) b)]
  -> Either (TxBodyErrorAutoBalance era) [(a, b)]
traverseScriptWitnesses =
  traverse (\(item, eRes) -> eRes >>= (\res -> Right (item, res)))

calculateMinimumUTxO
  :: HasCallStack
  => ShelleyBasedEra era
  -> TxOut CtxTx era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> L.Coin
calculateMinimumUTxO sbe txout pp =
  shelleyBasedEraConstraints sbe $
    let txOutWithMinCoin = L.setMinCoinTxOut pp (toShelleyTxOutAny sbe txout)
     in txOutWithMinCoin ^. L.coinTxOutL
