{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Experimental.Tx.Internal.BodyContent.Old
  ( extractAllIndexedPlutusScriptWitnesses
  , makeUnsignedTx
  )
where

import Cardano.Api.Era
  ( alonzoEraOnwardsConstraints
  , caseShelleyToBabbageOrConwayEraOnwards
  , unFeatured
  )
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
import Cardano.Api.Experimental.Tx.Internal.Type
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..), maybeToStrictMaybe)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Tx.Internal.Body

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as Ledger

import GHC.Exts (IsList (..))
import Lens.Micro

extractAllIndexedPlutusScriptWitnesses
  :: forall era
   . Era era
  -> TxBodyContent BuildTx era
  -> Either
       CBOR.DecoderError
       [AnyIndexedPlutusScriptWitness (LedgerEra era)]
extractAllIndexedPlutusScriptWitnesses era b = obtainCommonConstraints era $ do
  let sbe = convert era
      aeon = convert era
      legacyTxInWits = extractWitnessableTxIns aeon $ txIns b
      legacyCertWits = extractWitnessableCertificates aeon $ txCertificates b
      legacyMintWits = extractWitnessableMints aeon $ txMintValue b
      proposalWits
        :: [(Witnessable ProposalItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))] =
          caseShelleyToBabbageOrConwayEraOnwards
            (const [])
            (`extractWitnessableProposals` txProposalProcedures b)
            sbe
      legacyWithdrawalWits = extractWitnessableWithdrawals aeon $ txWithdrawals b
      legacyVoteWits
        :: [(Witnessable VoterItem (ShelleyLedgerEra era), BuildTxWith BuildTx (Witness WitCtxStake era))] =
          caseShelleyToBabbageOrConwayEraOnwards
            (const [])
            (`extractWitnessableVotes` txVotingProcedures b)
            sbe

  txInWits <- legacyWitnessConversion aeon legacyTxInWits
  let indexedScriptTxInWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses txInWits

  certWits <- legacyWitnessConversion aeon legacyCertWits
  let indexedCertScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses certWits

  mintWits <- legacyWitnessConversion aeon legacyMintWits
  let indexedMintScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses mintWits

  withdrawalWits <- legacyWitnessConversion aeon legacyWithdrawalWits
  let indexedWithdrawalScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses withdrawalWits

  proposalScriptWits <- legacyWitnessConversion aeon proposalWits
  let indexedProposalScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses proposalScriptWits

  voteWits <- legacyWitnessConversion aeon legacyVoteWits
  let indexedVoteScriptWits = alonzoEraOnwardsConstraints aeon $ createIndexedPlutusScriptWitnesses voteWits
  return $
    mconcat
      [ indexedScriptTxInWits
      , indexedMintScriptWits
      , indexedCertScriptWits
      , indexedWithdrawalScriptWits
      , indexedProposalScriptWits
      , indexedVoteScriptWits
      ]

makeUnsignedTx
  :: Era era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (UnsignedTx era)
makeUnsignedTx DijkstraEra _ = error "makeUnsignedTx: Dijkstra era not supported yet"
makeUnsignedTx era@ConwayEra bc = obtainCommonConstraints era $ do
  let sbe = convert era
      aeon = convert era
  TxScriptWitnessRequirements languages scripts datums redeemers <-
    shelleyBasedEraConstraints sbe $
      collectTxBodyScriptWitnessRequirements (convert era) bc

  -- cardano-api types
  let apiTxOuts = txOuts bc
      apiScriptValidity = txScriptValidity bc
      apiMintValue = txMintValue bc
      apiProtocolParameters = txProtocolParams bc
      apiCollateralTxIns = txInsCollateral bc
      apiReferenceInputs = txInsReference bc
      apiExtraKeyWitnesses = txExtraKeyWits bc
      apiReturnCollateral = txReturnCollateral bc
      apiTotalCollateral = txTotalCollateral bc

      -- Ledger types
      txins = convTxIns $ txIns bc
      collTxIns = convCollateralTxIns apiCollateralTxIns
      refTxIns = convReferenceInputs apiReferenceInputs
      outs = convTxOuts sbe apiTxOuts
      fee = convTransactionFee sbe $ txFee bc
      withdrawals = convWithdrawals $ txWithdrawals bc
      returnCollateral = convReturnCollateral sbe apiReturnCollateral
      totalCollateral = convTotalCollateral apiTotalCollateral
      certs = convCertificates sbe $ txCertificates bc
      txAuxData = toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc)
      scriptIntegrityHash =
        convPParamsToScriptIntegrityHash
          aeon
          apiProtocolParameters
          redeemers
          datums
          languages

  let setMint = convMintValue apiMintValue
      setReqSignerHashes = convExtraKeyWitnesses apiExtraKeyWitnesses
      ledgerTxBody =
        L.mkBasicTxBody
          & L.inputsTxBodyL .~ txins
          & L.collateralInputsTxBodyL .~ collTxIns
          & L.referenceInputsTxBodyL .~ refTxIns
          & L.outputsTxBodyL .~ outs
          & L.totalCollateralTxBodyL .~ totalCollateral
          & L.collateralReturnTxBodyL .~ returnCollateral
          & L.feeTxBodyL .~ fee
          & L.vldtTxBodyL . L.invalidBeforeL .~ convValidityLowerBound (txValidityLowerBound bc)
          & L.vldtTxBodyL . L.invalidHereAfterL .~ convValidityUpperBound sbe (txValidityUpperBound bc)
          & L.reqSignerHashesTxBodyL .~ setReqSignerHashes
          & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
          & L.withdrawalsTxBodyL .~ withdrawals
          & L.certsTxBodyL .~ certs
          & L.mintTxBodyL .~ setMint
          & L.auxDataHashTxBodyL .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData

      scriptWitnesses =
        L.mkBasicTxWits
          & L.scriptTxWitsL
            .~ fromList
              [ (L.hashScript sw, sw)
              | sw <- scripts
              ]
          & L.datsTxWitsL .~ datums
          & L.rdmrsTxWitsL .~ redeemers

  let eraSpecificTxBody = eraSpecificLedgerTxBody era ledgerTxBody bc

  return . UnsignedTx $
    L.mkBasicTx eraSpecificTxBody
      & L.witsTxL .~ scriptWitnesses
      & L.auxDataTxL .~ maybeToStrictMaybe (toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc))
      & L.isValidTxL .~ txScriptValidityToIsValid apiScriptValidity

eraSpecificLedgerTxBody
  :: Era era
  -> Ledger.TxBody (LedgerEra era)
  -> TxBodyContent BuildTx era
  -> Ledger.TxBody (LedgerEra era)
eraSpecificLedgerTxBody era ledgerbody bc =
  body era
 where
  body e =
    let propProcedures = txProposalProcedures bc
        voteProcedures = txVotingProcedures bc
        treasuryDonation = txTreasuryDonation bc
        currentTresuryValue = txCurrentTreasuryValue bc
     in obtainCommonConstraints e $
          ledgerbody
            & L.proposalProceduresTxBodyL
              .~ convProposalProcedures (maybe TxProposalProceduresNone unFeatured propProcedures)
            & L.votingProceduresTxBodyL
              .~ convVotingProcedures (maybe TxVotingProceduresNone unFeatured voteProcedures)
            & L.treasuryDonationTxBodyL
              .~ maybe (L.Coin 0) unFeatured treasuryDonation
            & L.currentTreasuryValueTxBodyL
              .~ L.maybeToStrictMaybe (unFeatured =<< currentTresuryValue)
