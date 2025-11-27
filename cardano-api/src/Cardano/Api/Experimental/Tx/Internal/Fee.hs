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

module Cardano.Api.Experimental.Tx.Internal.Fee
  ( estimateBalancedTxBody
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era.Internal.Case
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.MaryEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Feature
import Cardano.Api.Error
import Cardano.Api.Experimental.Era
-- import Cardano.Api.Plutus

import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.BodyContent.New
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Pretty
import Cardano.Api.ProtocolParameters
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.Body (ScriptWitnessIndex (..), toScriptIndex)
import Cardano.Api.Tx.Internal.Body.Lens qualified as A
import Cardano.Api.Tx.Internal.Fee (TxBodyErrorAutoBalance (..), TxFeeEstimationError (..))
import Cardano.Api.Tx.Internal.Fee qualified as Fee
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxIn
import Cardano.Api.UTxO (UTxO (..))
import Cardano.Api.Value.Internal

import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Alonzo.Plutus.Context qualified as Plutus
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.Credential as Ledger (Credential)
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Val qualified as L
import Ouroboros.Consensus.HardFork.History qualified as Consensus

import Data.Bifunctor (bimap, first, second)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Short (ShortByteString)
import Data.Function
import Data.Function ((&))
import Data.List (sortBy)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OSet.Strict qualified as OSet
import Data.Ord (Down (Down), comparing)
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro ((.~), (^.))
import Prettyprinter (punctuate)

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
  -> Value
  -- ^ Total value of UTXOs being spent.
  -> Either (Fee.TxFeeEstimationError era) (TxBodyContent (LedgerEra era))
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
  -> Value
  -- ^ Total value of UTXOs being spent.
  -> Either (Fee.TxFeeEstimationError era) (TxBodyContent (LedgerEra era)) -- (Fee.BalancedTxBody era)
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
        dummmyCollateral = TxCollateral{totalCollateral = dummyTotColl, returnCollateral = dummyCollRet}
    txbody1ForFeeEstimateOnly <-
      first TxFeeEstimationxBodyError $ -- TODO: impossible to fail now
        createTransactionBody
          sbe
          txbodycontent1
            { txFee = TxFeeExplicit sbe maxLovelaceFee
            , txOuts =
                TxOut changeaddr changeTxOut TxOutDatumNone ReferenceScriptNone
                  : txOuts txbodycontent
            , txCollateral = dummmyCollateral
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
            , txCollateral = retColl
            }

    let fakeUTxO = createFakeUTxO sbe txbodycontent1 $ selectLovelace availableUTxOValue
        balance =
          evaluateTransactionBalance sbe pparams poolids stakeDelegDeposits drepDelegDeposits fakeUTxO txbody2
        balanceTxOut = TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone

    -- Step 6. Check all txouts have the min required UTxO value
    first (TxFeeEstimationBalanceError . uncurry TxBodyErrorMinUTxONotMet)
      . mapM_ (checkMinUTxOValue sbe pparams)
      $ txOuts txbodycontent1

    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    finalTxOuts <-
      first TxFeeEstimationBalanceError $
        checkAndIncludeChange sbe pparams balanceTxOut (txOuts txbodycontent1)

    -- Step 7.

    -- Create the txbody with the final fee and change output. This should work
    -- provided that the fee and change are less than 2^32-1, and so will
    -- fit within the encoding size we picked above when calculating the fee.
    -- Yes this could be an over-estimate by a few bytes if the fee or change
    -- would fit within 2^16-1. That's a possible optimisation.
    let finalTxBodyContent =
          txbodycontent1
            { txFee = TxFeeExplicit sbe fee
            , txOuts = finalTxOuts
            , txCollateral = retColl
            }
    txbody3 <-
      first TxFeeEstimationFinalConstructionError $ -- TODO: impossible to fail now. We need to implement a function
      -- that simply creates a transaction body because we have already
      -- validated the transaction body earlier within makeTransactionBodyAutoBalance
        createTransactionBody sbe finalTxBodyContent
    return finalTxBodyContent

-- return
--  ( BalancedTxBody
--      finalTxBodyContent
--      txbody3
--      balanceTxOut
--      fee
--  )

substituteExecutionUnits
  :: forall era
   . Map ScriptWitnessIndex ExecutionUnits
  -> TxBodyContent (LedgerEra era)
  -> Either (TxBodyErrorAutoBalance era) (TxBodyContent (LedgerEra era))
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
    -- LEFT OFF HERE
    -- mappedWithdrawals <- mapScriptWitnessesWithdrawals txWithdrawals
    -- mappedMintedVals <- mapScriptWitnessesMinting txMintValue
    -- mappedTxCertificates <- mapScriptWitnessesCertificates txCertificates
    -- mappedVotes <- mapScriptWitnessesVotes txVotingProcedures
    -- mappedProposals <- mapScriptWitnessesProposals txProposalProcedures

    Right $
      txbodycontent
        & setTxIns mappedTxIns
   where
    --  & setTxMintValue mappedMintedVals
    --  & setTxCertificates mappedTxCertificates
    --  & setTxWithdrawals mappedWithdrawals
    --  & setTxVotingProcedures mappedVotes
    --  & setTxProposalProcedures mappedProposals

    substituteExecUnits
      :: ScriptWitnessIndex
      -> AnyWitness (LedgerEra era)
      -> Either (TxBodyErrorAutoBalance era) (AnyWitness (LedgerEra era))
    substituteExecUnits _ w@AnyKeyWitnessPlaceholder = Right w
    substituteExecUnits _ w@AnySimpleScriptWitness{} = Right w
    substituteExecUnits idx (AnyPlutusScriptWitness (PlutusScriptWitness lang script dat redeemer _)) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits ->
          Right $
            AnyPlutusScriptWitness $
              PlutusScriptWitness
                lang
                script
                dat
                redeemer
                exunits

    --
    mapScriptWitnessesTxIns
      :: [(TxIn, AnyWitness (LedgerEra era))]
      -> Either (TxBodyErrorAutoBalance era) [(TxIn, AnyWitness (LedgerEra era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( TxIn
                 , Either (TxBodyErrorAutoBalance era) (AnyWitness (LedgerEra era))
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

{-
    mapScriptWitnessesWithdrawals
      :: TxWithdrawals era
      -> Either (TxBodyErrorAutoBalance era) (TxWithdrawals era)
    mapScriptWitnessesWithdrawals TxWithdrawalsNone = Right TxWithdrawalsNone
    mapScriptWitnessesWithdrawals txWithdrawals'@(TxWithdrawals supported _) =
      let mappedWithdrawals
            :: [ ( StakeAddress
                 , L.Coin
                 , Either (TxBodyErrorAutoBalance era) (Witness WitCtxStake era)
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
      :: TxCertificates era
      -> Either (TxBodyErrorAutoBalance era) (TxCertificates era)
    mapScriptWitnessesCertificates TxCertificatesNone = Right TxCertificatesNone
    mapScriptWitnessesCertificates txCertificates'@(TxCertificates supported _) = do
      let mappedScriptWitnesses
            :: [ ( Exp.Certificate (ShelleyLedgerEra era)
                 , Either
                     (TxBodyErrorAutoBalance era)
                     ( Maybe
                         ( StakeCredential
                         , Witness WitCtxStake era
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
      :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures era))
      -> Either
           (TxBodyErrorAutoBalance era)
           (Maybe (Featured ConwayEraOnwards era (TxProposalProcedures era)))
    mapScriptWitnessesProposals Nothing = return Nothing
    mapScriptWitnessesProposals (Just (Featured era proposals)) = do
      substitutedExecutionUnits <-
        traverse
          (bitraverse pure $ traverse $ uncurry substituteExecUnits)
          $ indexWitnessedTxProposalProcedures proposals
      pure $
        Just $
          Featured era $
            conwayEraOnwardsConstraints era $
              mkTxProposalProcedures substitutedExecutionUnits

    mapScriptWitnessesMinting
      :: TxMintValue era
      -> Either (TxBodyErrorAutoBalance era) (TxMintValue era)
    mapScriptWitnessesMinting TxMintNone = Right TxMintNone
    mapScriptWitnessesMinting txMintValue'@(TxMintValue w _) = do
      let mappedScriptWitnesses =
            [ (policyId, (assets,) <$> substitutedWitness)
            | (ix, policyId, assets, witness) <- indexTxMintValue txMintValue'
            , let substitutedWitness = BuildTxWith <$> substituteExecUnits ix witness
            ]
          -- merge map values, wit1 == wit2 will always hold
          mergeValues (assets1, wit1) (assets2, _wit2) = (assets1 <> assets2, wit1)
      final <- Map.fromListWith mergeValues <$> traverseScriptWitnesses mappedScriptWitnesses
      pure $ TxMintValue w final
-}
traverseScriptWitnesses
  :: [(a, Either (TxBodyErrorAutoBalance era) b)]
  -> Either (TxBodyErrorAutoBalance era) [(a, b)]
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
