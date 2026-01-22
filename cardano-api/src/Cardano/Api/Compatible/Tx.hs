{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides a way to construct a simple transaction over all eras.
-- It is exposed for testing purposes only.
module Cardano.Api.Compatible.Tx
  ( AnyProtocolUpdate (..)
  , AnyVote (..)
  , createCompatibleTx
  , addWitnesses
  )
where

import Cardano.Api.Address (StakeCredential)
import Cardano.Api.Era
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.ProtocolParameters
import Cardano.Api.Tx.Internal.Body
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Value.Internal

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as L

import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Monoid
import Data.Sequence.Strict qualified as Seq
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro hiding (ix)

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

-- | Create a transaction in any shelley based era
createCompatibleTx
  :: forall era
   . ShelleyBasedEra era
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Lovelace
  -- ^ Fee
  -> AnyProtocolUpdate era
  -> AnyVote era
  -> TxCertificates BuildTx era
  -> Either ProtocolParametersConversionError (Tx era)
createCompatibleTx sbe ins outs txFee' anyProtocolUpdate anyVote txCertificates' =
  shelleyBasedEraConstraints sbe $ do
    (updateTxBody, extraScriptWitnesses) <-
      case anyProtocolUpdate of
        ProtocolUpdate shelleyToBabbageEra updateProposal -> do
          ledgerPParamsUpdate <- toLedgerUpdate sbe updateProposal
          let updateTxBody :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era)) =
                shelleyToBabbageEraConstraints shelleyToBabbageEra $
                  Endo $ \txb ->
                    txb & L.updateTxBodyL .~ SJust ledgerPParamsUpdate

          pure (updateTxBody, [])
        NoPParamsUpdate _ ->
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
              updateTxBody :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era)) =
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
          %~ setScriptWitnesses (apiScriptWitnesses <> extraScriptWitnesses)
        & updateVotingProcedures
 where
  era = toCardanoEra sbe
  appEndos = appEndo . mconcat

  setCerts :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setCerts =
    shelleyBasedEraConstraints sbe $
      Endo $
        L.certsTxBodyL .~ convCertificates sbe txCertificates'

  setRefInputs :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
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
    -> L.Tx L.TopTx (ShelleyLedgerEra era)
    -> L.Tx L.TopTx (ShelleyLedgerEra era)
  overwriteVotingProcedures conwayOnwards votingProcedures =
    conwayEraOnwardsConstraints conwayOnwards $
      (L.bodyTxL . L.votingProceduresTxBodyL) .~ votingProcedures

  indexedTxCerts
    :: [ ( ScriptWitnessIndex
         , Exp.Certificate (ShelleyLedgerEra era)
         , StakeCredential
         , Witness WitCtxStake era
         )
       ]
  indexedTxCerts = indexTxCertificates txCertificates'

  setScriptWitnesses
    :: [(ScriptWitnessIndex, AnyScriptWitness era)]
    -> L.TxWits (ShelleyLedgerEra era)
    -> L.TxWits (ShelleyLedgerEra era)
  setScriptWitnesses scriptWitnesses =
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

createCommonTxBody
  :: HasCallStack
  => ShelleyBasedEra era
  -> [TxIn]
  -> [TxOut ctx era]
  -> Lovelace
  -> L.TxBody L.TopTx (ShelleyLedgerEra era)
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

-- | Add provided witnesses to the transaction
addWitnesses
  :: forall era
   . [KeyWitness era]
  -> Tx era
  -> Tx era
  -- ^ a signed transaction
addWitnesses witnesses (ShelleyTx sbe tx) =
  shelleyBasedEraConstraints sbe $
    ShelleyTx sbe txCommon
 where
  txCommon
    :: forall ledgerera
     . ShelleyLedgerEra era ~ ledgerera
    => L.EraTx ledgerera
    => L.Tx L.TopTx ledgerera
  txCommon =
    tx
      & L.witsTxL
        %~ ( ( L.addrTxWitsL
                 %~ (<> fromList [w | ShelleyKeyWitness _ w <- witnesses])
             )
               . ( L.bootAddrTxWitsL
                     %~ (<> fromList [w | ShelleyBootstrapWitness _ w <- witnesses])
                 )
           )
