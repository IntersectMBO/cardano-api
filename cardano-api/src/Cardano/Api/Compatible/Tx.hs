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
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.AnyWitness qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.ProtocolParameters
import Cardano.Api.Tx.Internal.Body hiding
  ( convCertificates
  , indexTxCertificates
  , indexWitnessedTxProposalProcedures
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Value.Internal

import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Api qualified as L

import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Monoid
import Data.OSet.Strict (OSet)
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
    -> Exp.TxProposalProcedures (ShelleyLedgerEra era)
    -> AnyProtocolUpdate era
  NoPParamsUpdate
    :: ShelleyBasedEra era
    -> AnyProtocolUpdate era

data AnyVote era where
  VotingProcedures
    :: ConwayEraOnwards era
    -> Exp.TxVotingProcedures (ShelleyLedgerEra era)
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
  -> Exp.TxCertificates (ShelleyLedgerEra era)
  -> Either ProtocolParametersConversionError (Tx era)
createCompatibleTx sbe ins outs txFee' anyProtocolUpdate anyVote txCertificates' =
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
        NoPParamsUpdate _ ->
          pure (mempty, [])
        ProposalProcedures conwayOnwards proposalProcedures -> do
          let Exp.TxProposalProcedures propMap = proposalProcedures
              proposals :: OSet (L.ProposalProcedure (ShelleyLedgerEra era)) = fromList $ fst <$> shelleyBasedEraConstraints sbe (toList propMap)

              proposalWitnesses =
                [ (ix, witness)
                | (_, (ix, witness)) <-
                    indexWitnessedTxProposalProcedures conwayOnwards proposalProcedures
                ]
              referenceInputs =
                [ toShelleyTxIn txIn
                | (_, wit) <- proposalWitnesses
                , txIn <- maybeToList $ getAnyWitnessReferenceInput wit
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
            VotingProcedures conwayOnwards (Exp.TxVotingProcedures procedures _) ->
              overwriteVotingProcedures conwayOnwards procedures

        apiScriptWitnesses =
          [ (ix, witness)
          | (ix, _, _, witness) <- indexedTxCerts
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

  setCerts :: Endo (L.TxBody (ShelleyLedgerEra era))
  setCerts =
    shelleyBasedEraConstraints sbe $
      Endo $
        L.certsTxBodyL .~ convCertificates txCertificates'

  setRefInputs :: Endo (L.TxBody (ShelleyLedgerEra era))
  setRefInputs = do
    let refInputs =
          [ toShelleyTxIn refInput
          | (_, _, _, wit) <- indexedTxCerts
          , refInput <- maybeToList $ getAnyWitnessReferenceInput wit
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
    :: [ ( ScriptWitnessIndex
         , Exp.Certificate (ShelleyLedgerEra era)
         , StakeCredential
         , Exp.AnyWitness (ShelleyLedgerEra era)
         )
       ]
  indexedTxCerts = indexTxCertificates txCertificates'

  setScriptWitnesses
    :: [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
    -> L.TxWits (ShelleyLedgerEra era)
    -> L.TxWits (ShelleyLedgerEra era)
  setScriptWitnesses scriptWitnesses =
    appEndos
      [ monoidForEraInEon
          era
          ( \aeo -> alonzoEraOnwardsConstraints aeo $ Endo $ do
              let sData = convScriptData' sbe outs scriptWitnesses
              let (datums, redeemers) = case sData of
                    TxBodyScriptData _ ds rs -> (ds, rs)
                    TxBodyNoScriptData -> (mempty, L.Redeemers mempty)
              (L.datsTxWitsL .~ datums) . (L.rdmrsTxWitsL %~ (<> redeemers))
          )
      , monoidForEraInEon
          era
          ( \aeo -> allegraEraOnwardsConstraints aeo $ Endo $ do
              let ledgerScripts = convSimpleScripts sbe scriptWitnesses
              L.scriptTxWitsL
                .~ Map.fromList
                  [ (L.hashScript sw, sw)
                  | sw <- ledgerScripts
                  ]
          )
      ]

convSimpleScripts
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> [(ScriptWitnessIndex, Exp.AnyWitness (ShelleyLedgerEra era))]
  -> [L.Script ledgerera]
convSimpleScripts sbe scriptWitnesses =
  catMaybes
    [ shelleyBasedEraConstraints sbe $ Exp.getAnyWitnessSimpleScript anywit
    | (_, anywit) <- scriptWitnesses
    ]

convCertificates
  :: Exp.TxCertificates (ShelleyLedgerEra era)
  -> Seq.StrictSeq (L.TxCert (ShelleyLedgerEra era))
convCertificates (Exp.TxCertificates cs) =
  fromList . map (\(Exp.Certificate c, _) -> c) $ toList cs

convScriptData'
  :: ShelleyBasedEra era
  -> [TxOut CtxTx era]
  -> [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
  -> TxBodyScriptData era
convScriptData' sbe txOuts' scriptWitnesses =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxBodyNoScriptData)
    ( \w ->
        let redeemers = getAnyPlutusScriptWitnessRedeemerPointerMap w scriptWitnesses

            datums = mconcat [getAnyWitnessScriptData wit | (_, wit) <- scriptWitnesses]

            supplementalDatums =
              let ds = [d | TxOut _ _ (TxOutSupplementalDatum _ d) _ <- txOuts']
               in Alonzo.TxDats $
                    fromList
                      [ (L.hashData d', d')
                      | d <- ds
                      , let d' = toAlonzoData d
                      ]
         in TxBodyScriptData w (datums <> supplementalDatums) redeemers
    )
    sbe

getAnyPlutusScriptWitnessRedeemerPointerMap
  :: AlonzoEraOnwards era
  -> [(ScriptWitnessIndex, Exp.AnyWitness (ShelleyLedgerEra era))]
  -> L.Redeemers (ShelleyLedgerEra era)
getAnyPlutusScriptWitnessRedeemerPointerMap w wits =
  alonzoEraOnwardsConstraints w $
    Alonzo.Redeemers $
      fromList
        [ ( i
          ,
            ( toAlonzoData $ getAnyPlutusScriptWitnessRedeemer pswit
            , toAlonzoExUnits $ getAnyPlutusScriptWitnessExecutionUnits pswit
            )
          )
        | ( idx
            , AnyPlutusScriptWitness pswit
            ) <-
            wits
        , Just i <- [fromScriptWitnessIndex w idx]
        ]

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
    => L.Tx ledgerera
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

-- | Index proposal procedures by their order ('Ord').
indexWitnessedTxProposalProcedures
  :: forall era
   . ConwayEraOnwards era
  -> Exp.TxProposalProcedures (ShelleyLedgerEra era)
  -> [ ( L.ProposalProcedure (ShelleyLedgerEra era)
       , (ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))
       )
     ]
indexWitnessedTxProposalProcedures cOnwards (Exp.TxProposalProcedures proposals) = do
  let allProposalsList = zip [0 ..] $ conwayEraOnwardsConstraints cOnwards $ toList proposals
  [ (proposal, (ScriptWitnessIndexProposing ix, anyWitness))
    | (ix, (proposal, anyWitness)) <- allProposalsList
    ]

-- | Index certificates with witnesses by the order they appear in the list (in the transaction).
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxCertificates
  :: Exp.TxCertificates (ShelleyLedgerEra era)
  -> [ ( ScriptWitnessIndex
       , Exp.Certificate (ShelleyLedgerEra era)
       , StakeCredential
       , AnyWitness (ShelleyLedgerEra era)
       )
     ]
indexTxCertificates (Exp.TxCertificates certsWits) =
  [ (ScriptWitnessIndexCertificate ix, cert, stakeCred, witness)
  | (ix, (cert, Just (stakeCred, witness))) <- zip [0 ..] $ toList certsWits
  ]
