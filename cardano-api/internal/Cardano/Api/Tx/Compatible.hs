{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides a way to construct a simple transaction over all eras.
-- It is exposed for testing purposes only.
module Cardano.Api.Tx.Compatible
  ( AnyProtocolUpdate (..)
  , AnyVote (..)
  , createCompatibleSignedTx
  )
where

import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eras
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.Tx.Body
import           Cardano.Api.Tx.Sign
import           Cardano.Api.Value

import qualified Cardano.Ledger.Api as L

import           Control.Error (catMaybes)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict
import qualified Data.Sequence.Strict as Seq
import           Data.Set (fromList)
import           Lens.Micro

data AnyProtocolUpdate era where
  ProtocolUpdate
    :: ShelleyToBabbageEra era
    -> UpdateProposal
    -> AnyProtocolUpdate era
  ProposalProcedures
    :: ConwayEraOnwards era
    -> TxProposalProcedures BuildTx era
    -> AnyProtocolUpdate era
  NoPParamsUpdate
    :: ShelleyBasedEra era
    -> AnyProtocolUpdate era

data AnyVote era where
  VotingProcedures
    :: ConwayEraOnwards era
    -> TxVotingProcedures BuildTx era
    -> AnyVote era
  NoVotes :: AnyVote era

createCompatibleSignedTx
  :: forall era
   . ShelleyBasedEra era
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> [KeyWitness era]
  -> Lovelace
  -- ^ Fee
  -> AnyProtocolUpdate era
  -> AnyVote era
  -> TxCertificates BuildTx era
  -> Either ProtocolParametersConversionError (Tx era)
createCompatibleSignedTx sbe ins outs witnesses txFee' anyProtocolUpdate anyVote txCertificates' =
  shelleyBasedEraConstraints sbe $ do
    let setCerts =
          caseShelleyToMaryOrAlonzoEraOnwards
            (const id)
            (\w -> alonzoEraOnwardsConstraints w $ L.certsTxBodyL .~ convCertificates sbe txCertificates')
            sbe
        txbody =
          createCommonTxBody sbe ins outs txFee'
            & setCerts

    fTx <- case anyProtocolUpdate of
      ProtocolUpdate shelleyToBabbageEra updateProposal -> do
        ledgerPParamsUpdate <- toLedgerUpdate sbe updateProposal

        let bodyWithProtocolUpdate =
              shelleyToBabbageEraConstraints shelleyToBabbageEra $
                txbody & L.updateTxBodyL .~ SJust ledgerPParamsUpdate
        pure $
          L.mkBasicTx bodyWithProtocolUpdate
            & L.witsTxL .~ shelleyToBabbageEraConstraints shelleyToBabbageEra allShelleyToBabbageWitnesses
      NoPParamsUpdate _ -> do
        pure $
          L.mkBasicTx txbody
            & L.witsTxL .~ shelleyBasedEraConstraints sbe allShelleyToBabbageWitnesses
      ProposalProcedures conwayOnwards proposalProcedures -> do
        let proposals = convProposalProcedures proposalProcedures
            apiScriptWitnesses = scriptWitnessesProposing proposalProcedures
            ledgerScripts = convScripts apiScriptWitnesses
            referenceInputs =
              map toShelleyTxIn $
                catMaybes [getScriptWitnessReferenceInput sWit | (_, AnyScriptWitness sWit) <- apiScriptWitnesses]
            sData = convScriptData sbe outs apiScriptWitnesses
            updatedTxBody =
              conwayEraOnwardsConstraints conwayOnwards $
                txbody
                  & L.referenceInputsTxBodyL .~ fromList referenceInputs -- TODO add refinputs from certs here
                  & L.proposalProceduresTxBodyL .~ proposals

        pure $
          L.mkBasicTx updatedTxBody
            & L.witsTxL
              .~ conwayEraOnwardsConstraints conwayOnwards (allConwayEraOnwardsWitnesses sData ledgerScripts)

    case anyVote of
      NoVotes -> return $ ShelleyTx sbe fTx
      VotingProcedures conwayOnwards procedures -> do
        let ledgerVotingProcedures = convVotingProcedures procedures
            updatedTx =
              conwayEraOnwardsConstraints conwayOnwards $
                overwriteVotingProcedures fTx ledgerVotingProcedures
        return $ ShelleyTx sbe updatedTx
 where
  overwriteVotingProcedures
    :: L.ConwayEraTxBody ledgerera
    => L.EraTx ledgerera
    => L.Tx ledgerera -> L.VotingProcedures ledgerera -> L.Tx ledgerera
  overwriteVotingProcedures lTx vProcedures =
    lTx & (L.bodyTxL . L.votingProceduresTxBodyL) .~ vProcedures

  shelleyKeywitnesses =
    fromList [w | ShelleyKeyWitness _ w <- witnesses]

  shelleyBootstrapWitnesses =
    fromList [w | ShelleyBootstrapWitness _ w <- witnesses]

  allConwayEraOnwardsWitnesses
    :: L.AlonzoEraTxWits (ShelleyLedgerEra era)
    => L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
    => TxBodyScriptData era -> [L.Script (ShelleyLedgerEra era)] -> L.TxWits (ShelleyLedgerEra era)
  allConwayEraOnwardsWitnesses sData ledgerScripts =
    let (datums, redeemers) = case sData of
          TxBodyScriptData _ ds rs -> (ds, rs)
          TxBodyNoScriptData -> (mempty, L.Redeemers mempty)
     in L.mkBasicTxWits
          & L.addrTxWitsL
            .~ shelleyKeywitnesses
          & L.bootAddrTxWitsL
            .~ shelleyBootstrapWitnesses
          & L.datsTxWitsL .~ datums
          & L.rdmrsTxWitsL .~ redeemers
          & L.scriptTxWitsL
            .~ Map.fromList
              [ (L.hashScript sw, sw)
              | sw <- ledgerScripts
              ]

  allShelleyToBabbageWitnesses
    :: L.EraTxWits (ShelleyLedgerEra era)
    => L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
    => L.TxWits (ShelleyLedgerEra era)
  allShelleyToBabbageWitnesses =
    L.mkBasicTxWits
      & L.addrTxWitsL
        .~ shelleyKeywitnesses
      & L.bootAddrTxWitsL
        .~ shelleyBootstrapWitnesses

createCommonTxBody
  :: ShelleyBasedEra era
  -> [TxIn]
  -> [TxOut ctx era]
  -> Lovelace
  -> L.TxBody (ShelleyLedgerEra era)
createCommonTxBody era ins outs txFee' =
  let txIns' = map toShelleyTxIn ins
      txOuts' = map (toShelleyTxOutAny era) outs
   in shelleyBasedEraConstraints era $
        L.mkBasicTxBody
          & L.inputsTxBodyL
            .~ fromList txIns'
          & L.outputsTxBodyL
            .~ Seq.fromList txOuts'
          & L.feeTxBodyL
            .~ txFee'
