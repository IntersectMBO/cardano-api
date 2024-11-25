{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides a way to construct a simple transaction over all eras.
-- It is exposed for testing purposes only.
module Cardano.Api.Tx.Compatible
  ( AnyProtocolUpdate (..)
  , AnyVote (..)
  , createCompatibleSignedTx
  )
where

import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eras
import           Cardano.Api.Eras.Case
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.Tx.Body
import           Cardano.Api.Tx.Sign
import           Cardano.Api.Value

import qualified Cardano.Ledger.Api as L

import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Strict
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import           GHC.Exts (IsList (..))
import           Lens.Micro hiding (ix)

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
    let txbody =
          createCommonTxBody sbe ins outs txFee'
            & setCerts
            & setRefInputs

    fTx <- case anyProtocolUpdate of
      ProtocolUpdate shelleyToBabbageEra updateProposal -> do
        ledgerPParamsUpdate <- toLedgerUpdate sbe updateProposal

        let apiScriptWitnesses =
              [ (ix, AnyScriptWitness witness)
              | (ix, _, _, ScriptWitness _ witness) <- txCertificatesToIndexed txCertificates'
              ]
            ledgerScripts = convScripts apiScriptWitnesses
            sData = convScriptData sbe outs apiScriptWitnesses
        let bodyWithProtocolUpdate =
              shelleyToBabbageEraConstraints shelleyToBabbageEra $
                txbody & L.updateTxBodyL .~ SJust ledgerPParamsUpdate
        pure $
          L.mkBasicTx bodyWithProtocolUpdate
            & L.witsTxL .~ allWitnesses sData ledgerScripts allShelleyToBabbageWitnesses
      NoPParamsUpdate _ -> do
        let apiScriptWitnesses =
              [ (ix, AnyScriptWitness witness)
              | (ix, _, _, ScriptWitness _ witness) <- txCertificatesToIndexed txCertificates'
              ]
            ledgerScripts = convScripts apiScriptWitnesses
            referenceInputs =
              [ toShelleyTxIn txIn
              | (_, AnyScriptWitness sWit) <- apiScriptWitnesses
              , txIn <- maybeToList $ getScriptWitnessReferenceInput sWit
              ]
            sData = convScriptData sbe outs apiScriptWitnesses
            updatedBody =
              txbody
                & caseShelleyToAlonzoOrBabbageEraOnwards
                  (const id)
                  (const $ L.referenceInputsTxBodyL %~ (<> fromList referenceInputs))
                  sbe
        pure $
          L.mkBasicTx updatedBody
            & L.witsTxL .~ allWitnesses sData ledgerScripts allShelleyToBabbageWitnesses
      ProposalProcedures conwayOnwards proposalProcedures -> do
        let proposals = convProposalProcedures proposalProcedures
            apiScriptWitnesses =
              [ (ix, AnyScriptWitness witness)
              | (ix, _, witness) <- txProposalProceduresToIndexed proposalProcedures
              ]
                <> [ (ix, AnyScriptWitness witness)
                   | (ix, _, _, ScriptWitness _ witness) <- txCertificatesToIndexed txCertificates'
                   ]
            ledgerScripts = convScripts apiScriptWitnesses
            referenceInputs =
              [ toShelleyTxIn txIn
              | (_, AnyScriptWitness sWit) <- apiScriptWitnesses
              , txIn <- maybeToList $ getScriptWitnessReferenceInput sWit
              ]
            sData = convScriptData sbe outs apiScriptWitnesses
            updatedTxBody =
              conwayEraOnwardsConstraints conwayOnwards $
                txbody
                  & L.referenceInputsTxBodyL %~ (<> fromList referenceInputs)
                  & L.proposalProceduresTxBodyL .~ proposals

        pure $
          L.mkBasicTx updatedTxBody
            & L.witsTxL
              .~ allWitnesses sData ledgerScripts allShelleyToBabbageWitnesses

    case anyVote of
      NoVotes -> return $ ShelleyTx sbe fTx
      VotingProcedures conwayOnwards procedures -> do
        let ledgerVotingProcedures = convVotingProcedures procedures
            updatedTx =
              conwayEraOnwardsConstraints conwayOnwards $
                overwriteVotingProcedures fTx ledgerVotingProcedures
        return $ ShelleyTx sbe updatedTx
 where
  setCerts :: L.TxBody (ShelleyLedgerEra era) -> L.TxBody (ShelleyLedgerEra era)
  setCerts =
    shelleyBasedEraConstraints sbe $
      caseShelleyToMaryOrAlonzoEraOnwards
        (const id)
        (const $ L.certsTxBodyL .~ convCertificates sbe txCertificates')
        sbe

  setRefInputs :: L.TxBody (ShelleyLedgerEra era) -> L.TxBody (ShelleyLedgerEra era)
  setRefInputs = do
    let refInputs =
          [ toShelleyTxIn refInput
          | (_, _, _, ScriptWitness _ wit) <- txCertificatesToIndexed txCertificates'
          , refInput <- maybeToList $ getScriptWitnessReferenceInput wit
          ]

    caseShelleyToAlonzoOrBabbageEraOnwards
      (const id)
      (const $ L.referenceInputsTxBodyL .~ fromList refInputs)
      sbe

  overwriteVotingProcedures
    :: L.ConwayEraTxBody ledgerera
    => L.EraTx ledgerera
    => L.Tx ledgerera -> L.VotingProcedures ledgerera -> L.Tx ledgerera
  overwriteVotingProcedures lTx vProcedures =
    lTx & (L.bodyTxL . L.votingProceduresTxBodyL) .~ vProcedures

  shelleyKeywitnesses =
    fromList @(Set _) [w | ShelleyKeyWitness _ w <- witnesses]

  shelleyBootstrapWitnesses =
    fromList @(Set _) [w | ShelleyBootstrapWitness _ w <- witnesses]

  allWitnesses
    :: TxBodyScriptData era
    -> [L.Script (ShelleyLedgerEra era)]
    -> L.TxWits (ShelleyLedgerEra era)
    -> L.TxWits (ShelleyLedgerEra era)
  allWitnesses sData ledgerScripts txw = shelleyBasedEraConstraints sbe $ do
    let txw1 =
          caseShelleyToMaryOrAlonzoEraOnwards
            (const txw)
            ( const $ do
                let (datums, redeemers) = case sData of
                      TxBodyScriptData _ ds rs -> (ds, rs)
                      TxBodyNoScriptData -> (mempty, L.Redeemers mempty)
                txw
                  & L.datsTxWitsL .~ datums
                  & L.rdmrsTxWitsL %~ (<> redeemers)
            )
            sbe
        txw2 =
          caseShelleyEraOnlyOrAllegraEraOnwards
            (const txw1)
            ( const $
                txw1
                  & L.scriptTxWitsL
                    .~ Map.fromList
                      [ (L.hashScript sw, sw)
                      | sw <- ledgerScripts
                      ]
            )
            sbe
    txw2

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

-- allWitnessesToIndexed ::

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
