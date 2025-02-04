{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides a way to construct a simple transaction over all eras.
-- It is exposed for testing purposes only.
module Cardano.Api.Tx.Compatible
  ( AnyProtocolUpdate (..)
  , AnyVote (..)
  , createCompatibleSignedTx
  )
where

import           Cardano.Api.Address (StakeCredential)
import           Cardano.Api.Certificate (Certificate)
import           Cardano.Api.Eon.AllegraEraOnwards
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
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

import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.Monoid
import qualified Data.Sequence.Strict as Seq
import           GHC.Exts (IsList (..))
import           GHC.Stack
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
    (updateTxBody, extraScriptWitnesses) <-
      case anyProtocolUpdate of
        ProtocolUpdate shelleyToBabbageEra updateProposal -> do
          ledgerPParamsUpdate <- toLedgerUpdate sbe updateProposal
          let updateTxBody :: Endo (L.TxBody (ShelleyLedgerEra era)) =
                shelleyToBabbageEraConstraints shelleyToBabbageEra $
                  Endo $ \txb ->
                    txb & L.updateTxBodyL .~ SJust ledgerPParamsUpdate

          pure (updateTxBody, [])
        NoPParamsUpdate _ -> do
          pure (mempty, [])
        ProposalProcedures conwayOnwards proposalProcedures -> do
          let proposals = convProposalProcedures proposalProcedures
              proposalWitnesses =
                [ (ix, AnyScriptWitness witness)
                | (ix, _, witness) <- indexTxProposalProcedures proposalProcedures
                ]
              referenceInputs =
                [ toShelleyTxIn txIn
                | (_, AnyScriptWitness sWit) <- proposalWitnesses
                , txIn <- maybeToList $ getScriptWitnessReferenceInput sWit
                ]
              -- append proposal reference inputs & set proposal procedures
              updateTxBody :: Endo (L.TxBody (ShelleyLedgerEra era)) =
                conwayEraOnwardsConstraints conwayOnwards $
                  Endo $
                    (L.referenceInputsTxBodyL %~ (<> fromList referenceInputs))
                      . (L.proposalProceduresTxBodyL .~ proposals)

          pure (updateTxBody, proposalWitnesses)

    let txbody =
          createCommonTxBody sbe ins outs txFee'
            & appEndos [setCerts, setRefInputs, updateTxBody]

        updateVotingProcedures =
          case anyVote of
            NoVotes -> id
            VotingProcedures conwayOnwards procedures ->
              overwriteVotingProcedures conwayOnwards (convVotingProcedures procedures)

        apiScriptWitnesses =
          [ (ix, AnyScriptWitness witness)
          | (ix, _, _, ScriptWitness _ witness) <- indexedTxCerts
          ]

    pure
      . ShelleyTx sbe
      $ L.mkBasicTx txbody
        & L.witsTxL
          .~ allWitnesses (apiScriptWitnesses <> extraScriptWitnesses) allShelleyToBabbageWitnesses
        & updateVotingProcedures
 where
  era = toCardanoEra sbe
  appEndos = appEndo . mconcat

  setCerts :: Endo (L.TxBody (ShelleyLedgerEra era))
  setCerts =
    monoidForEraInEon era $ \aeo ->
      alonzoEraOnwardsConstraints aeo $
        Endo $
          L.certsTxBodyL .~ convCertificates sbe txCertificates'

  setRefInputs :: Endo (L.TxBody (ShelleyLedgerEra era))
  setRefInputs = do
    let refInputs =
          [ toShelleyTxIn refInput
          | (_, _, _, ScriptWitness _ wit) <- indexedTxCerts
          , refInput <- maybeToList $ getScriptWitnessReferenceInput wit
          ]

    monoidForEraInEon era $ \beo ->
      babbageEraOnwardsConstraints beo $
        Endo $
          L.referenceInputsTxBodyL .~ fromList refInputs

  overwriteVotingProcedures
    :: ConwayEraOnwards era
    -> L.VotingProcedures (ShelleyLedgerEra era)
    -> L.Tx (ShelleyLedgerEra era)
    -> L.Tx (ShelleyLedgerEra era)
  overwriteVotingProcedures conwayOnwards votingProcedures =
    conwayEraOnwardsConstraints conwayOnwards $
      (L.bodyTxL . L.votingProceduresTxBodyL) .~ votingProcedures

  indexedTxCerts
    :: [(ScriptWitnessIndex, Certificate era, StakeCredential, Witness WitCtxStake era)]
  indexedTxCerts = indexTxCertificates txCertificates'

  allWitnesses
    :: [(ScriptWitnessIndex, AnyScriptWitness era)]
    -> L.TxWits (ShelleyLedgerEra era)
    -> L.TxWits (ShelleyLedgerEra era)
  allWitnesses scriptWitnesses =
    appEndos
      [ monoidForEraInEon
          era
          ( \aeo -> alonzoEraOnwardsConstraints aeo $ Endo $ do
              let sData = convScriptData sbe outs scriptWitnesses
              let (datums, redeemers) = case sData of
                    TxBodyScriptData _ ds rs -> (ds, rs)
                    TxBodyNoScriptData -> (mempty, L.Redeemers mempty)
              (L.datsTxWitsL .~ datums) . (L.rdmrsTxWitsL %~ (<> redeemers))
          )
      , monoidForEraInEon
          era
          ( \aeo -> allegraEraOnwardsConstraints aeo $ Endo $ do
              let ledgerScripts = convScripts scriptWitnesses
              L.scriptTxWitsL
                .~ Map.fromList
                  [ (L.hashScript sw, sw)
                  | sw <- ledgerScripts
                  ]
          )
      ]

  allShelleyToBabbageWitnesses
    :: L.EraTxWits (ShelleyLedgerEra era)
    => L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
    => L.TxWits (ShelleyLedgerEra era)
  allShelleyToBabbageWitnesses = do
    let shelleyKeywitnesses =
          fromList [w | ShelleyKeyWitness _ w <- witnesses]
    let shelleyBootstrapWitnesses =
          fromList [w | ShelleyBootstrapWitness _ w <- witnesses]
    L.mkBasicTxWits
      & L.addrTxWitsL
        .~ shelleyKeywitnesses
      & L.bootAddrTxWitsL
        .~ shelleyBootstrapWitnesses

createCommonTxBody
  :: HasCallStack
  => ShelleyBasedEra era
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
