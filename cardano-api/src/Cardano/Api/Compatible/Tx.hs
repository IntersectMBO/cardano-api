{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides a transaction builder covering all Shelley-based
-- eras, for consumers that still need pre-Conway support (e.g.
-- tx-generator). The experimental API only covers the current and next
-- era.
module Cardano.Api.Compatible.Tx
  ( AnyProtocolUpdate (..)
  , AnyVote (..)
  , CompatibleTxError (..)
  , CompatibleTxExtraContent (..)
  , defaultCompatibleTxExtraContent
  , createCompatibleTx
  , addWitnesses
  )
where

import Cardano.Api.Address (StakeCredential)
import Cardano.Api.Era
import Cardano.Api.Error (Error (..))
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus (obtainAlonzoScriptPurposeConstraints)
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
import Cardano.Api.Tx.Internal.Body.Lens qualified as A
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Value.Internal

import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.TxIn qualified as L
import Cardano.Slotting.Slot (SlotNo)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Monoid
import Data.OSet.Strict (OSet)
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Lens.Micro hiding (ix)

data AnyProtocolUpdate era where
  ProtocolUpdate
    :: ShelleyToBabbageEra era
    -> UpdateProposal era
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

-- | Extra content for 'createCompatibleTx' that is orthogonal to
-- certificates, proposals and votes: collateral inputs, the protocol
-- parameters needed to compute the script integrity hash, transaction
-- metadata, and the validity interval's upper bound.
data CompatibleTxExtraContent era = CompatibleTxExtraContent
  { compatibleTxCollateralIns :: [TxIn]
  -- ^ Collateral inputs. Only meaningful from Alonzo onwards; ignored
  -- below that (callers should only supply a non-empty list once plutus
  -- spending witnesses are actually in use).
  , compatibleTxProtocolParams :: Maybe (L.PParams (ShelleyLedgerEra era))
  -- ^ Protocol parameters, required to compute the script integrity hash
  -- whenever plutus script witnesses are present (spending, certifying,
  -- proposing, ...). 'Nothing' is only safe when there are none;
  -- see 'CompatibleTxMissingScriptIntegrityPParams'.
  , compatibleTxMetadata :: TxMetadataInEra era
  -- ^ Transaction metadata to embed, or 'TxMetadataNone' for none.
  , compatibleTxValidityUpperBound :: Maybe SlotNo
  -- ^ The validity interval's upper bound (the slot after which the
  -- transaction can no longer be included in a block), or 'Nothing' to
  -- leave it unbounded.
  }

-- | The extra content with no collateral, no protocol parameters, no
-- metadata and no validity upper bound, i.e. the behaviour
-- 'createCompatibleTx' had before plutus spending support was added.
defaultCompatibleTxExtraContent :: CompatibleTxExtraContent era
defaultCompatibleTxExtraContent =
  CompatibleTxExtraContent
    { compatibleTxCollateralIns = []
    , compatibleTxProtocolParams = Nothing
    , compatibleTxMetadata = TxMetadataNone
    , compatibleTxValidityUpperBound = Nothing
    }

-- | Errors that can occur while assembling a 'Tx' with 'createCompatibleTx'.
data CompatibleTxError
  = -- | The supplied protocol parameters could not be converted to the
    -- ledger representation needed to compute the script integrity hash.
    CompatibleTxProtocolParametersConversionError ProtocolParametersConversionError
  | -- | Plutus script witnesses are present (as evidenced by non-empty
    -- redeemers, datums or plutus languages), but
    -- 'compatibleTxProtocolParams' is 'Nothing', so the script integrity
    -- hash required by the ledger cannot be computed.
    CompatibleTxMissingScriptIntegrityPParams
  deriving Show

instance Error CompatibleTxError where
  prettyError err =
    case err of
      CompatibleTxProtocolParametersConversionError e -> prettyError e
      CompatibleTxMissingScriptIntegrityPParams ->
        "Plutus script witnesses are present but no protocol parameters were supplied "
          <> "to compute the script integrity hash."

-- | Create a transaction in any shelley based era
createCompatibleTx
  :: forall era
   . ShelleyBasedEra era
  -> [(TxIn, Exp.AnyWitness (ShelleyLedgerEra era))]
  -- ^ Transaction inputs, together with their witnesses. Key-witnessed
  -- inputs must supply 'Exp.AnyKeyWitnessPlaceholder' so that spending
  -- redeemer pointers for the OTHER, script-witnessed inputs are indexed
  -- correctly; see 'Exp.AnyWitness'.
  -> [Exp.TxOut (ShelleyLedgerEra era)]
  -> Map L.DataHash (L.Data (ShelleyLedgerEra era))
  -- ^ Supplemental datums to include in the witness set. Use 'mempty' if
  -- none are required. The legacy 'TxOut CtxTx era' bundled supplemental
  -- datums inside outputs; 'Exp.TxOut' only carries the datum hash, so
  -- callers thread the full datum bodies in here explicitly. Datums carried
  -- by spending script witnesses are collected automatically and merged
  -- with these.
  -> Lovelace
  -- ^ Fee
  -> AnyProtocolUpdate era
  -> AnyVote era
  -> Exp.TxCertificates (ShelleyLedgerEra era)
  -> CompatibleTxExtraContent era
  -- ^ Collateral inputs, protocol parameters (needed for the script
  -- integrity hash), metadata and the validity interval's upper bound.
  -> Either CompatibleTxError (Tx era)
createCompatibleTx sbe ins outs extraDatums txFee' anyProtocolUpdate anyVote txCertificates' extraContent =
  shelleyBasedEraConstraints sbe $ do
    (updateTxBody, extraScriptWitnesses) <-
      case anyProtocolUpdate of
        ProtocolUpdate shelleyToBabbageEra updateProposal -> do
          let ledgerPParamsUpdate = toLedgerUpdate sbe updateProposal
              updateTxBody :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era)) =
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
              updateTxBody :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era)) =
                conwayEraOnwardsConstraints conwayOnwards $
                  Endo $
                    (L.referenceInputsTxBodyL %~ (<> fromList referenceInputs))
                      . (L.proposalProceduresTxBodyL .~ proposals)

          pure (updateTxBody, proposalWitnesses)

    let apiScriptWitnesses =
          [ (ix, witness)
          | (ix, _, Just (_, witness)) <- indexedTxCerts
          ]

        -- All script-witnessable things merged into a single list: the
        -- redeemer pointer map, witness datums and the script integrity
        -- hash are all derived from this one list so that certificates,
        -- proposals and now inputs are treated uniformly.
        allScriptWitnesses
          :: [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
        allScriptWitnesses = apiScriptWitnesses <> extraScriptWitnesses <> inputScriptWitnesses

        sData :: TxBodyScriptData era
        sData = convScriptData' sbe extraDatums allScriptWitnesses

        txAuxData :: Maybe (L.TxAuxData (ShelleyLedgerEra era))
        txAuxData = toAuxiliaryData sbe (compatibleTxMetadata extraContent) TxAuxScriptsNone

    integrityHashUpdate <- setScriptIntegrityHash sData allScriptWitnesses

    let txbody =
          createCommonTxBody sbe ledgerTxIns outs txFee'
            & appEndos
              [ setCerts
              , setRefInputs
              , updateTxBody
              , setCollateralIns
              , setValidityUpperBound
              , setMetadataHash txAuxData
              , integrityHashUpdate
              ]

        updateVotingProcedures =
          case anyVote of
            NoVotes -> id
            VotingProcedures conwayOnwards (Exp.TxVotingProcedures procedures _) ->
              overwriteVotingProcedures conwayOnwards procedures

    pure
      . ShelleyTx sbe
      $ L.mkBasicTx txbody
        & L.witsTxL
          %~ setScriptWitnesses sData allScriptWitnesses
        & updateVotingProcedures
        & L.auxDataTxL
          .~ maybeToStrictMaybe txAuxData
 where
  era = toCardanoEra sbe
  appEndos = appEndo . mconcat

  -- \| The final, sorted set of ledger inputs. Redeemer pointers for
  -- script-witnessed inputs ('inputScriptWitnesses' below) MUST be
  -- computed against this, and never against the order 'ins' was
  -- supplied in, since the ledger stores tx inputs in a 'Set'.
  ledgerTxIns :: Set L.TxIn
  ledgerTxIns = fromList $ map (toShelleyTxIn . fst) ins

  inputScriptWitnesses
    :: [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
  inputScriptWitnesses =
    [ (ScriptWitnessIndexTxIn . fromIntegral . Set.findIndex ledgerTxIn $ ledgerTxIns, witness)
    | (txIn, witness) <- ins
    , let ledgerTxIn = toShelleyTxIn txIn
    ]

  setCerts :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setCerts =
    shelleyBasedEraConstraints sbe $
      Endo $
        L.certsTxBodyL .~ convCertificates txCertificates'

  -- \| NB the reference inputs collected here come from BOTH witnessed
  -- certificates and witnessed spending inputs; this uses '%~ (<>)'
  -- rather than '.~' so it does not clobber reference inputs already
  -- appended by 'updateTxBody' (the 'ProposalProcedures' case above).
  setRefInputs :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setRefInputs = do
    let refInputs =
          [ toShelleyTxIn refInput
          | (_, _, Just (_, wit)) <- indexedTxCerts
          , refInput <- maybeToList $ getAnyWitnessReferenceInput wit
          ]
            ++ [ toShelleyTxIn refInput
               | (_, wit) <- ins
               , refInput <- maybeToList $ getAnyWitnessReferenceInput wit
               ]

    monoidForEraInEon era $ \beo ->
      babbageEraOnwardsConstraints beo $
        Endo $
          L.referenceInputsTxBodyL %~ (<> fromList refInputs)

  -- \| Alonzo onwards only; a no-op below that (the list is expected to be
  -- empty there anyway, since collateral only matters for plutus spending).
  setCollateralIns :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setCollateralIns =
    monoidForEraInEon era $ \aeo ->
      alonzoEraOnwardsConstraints aeo $
        Endo $
          L.collateralInputsTxBodyL
            .~ (fromList . map toShelleyTxIn $ compatibleTxCollateralIns extraContent)

  -- \| Compatibility lens over 'ttlTxBodyL' (Shelley) and 'vldtTxBodyL'
  -- (Allegra onwards), preserving the validity interval's lower bound.
  setValidityUpperBound :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setValidityUpperBound =
    Endo $ \txb ->
      A.unTxBody $
        A.LedgerTxBody txb
          & A.invalidHereAfterTxBodyL sbe
            .~ compatibleTxValidityUpperBound extraContent

  -- \| Only the body-side auxiliary data hash; the auxiliary data itself is
  -- set on the 'L.Tx' via 'L.auxDataTxL' in the main function body.
  setMetadataHash
    :: Maybe (L.TxAuxData (ShelleyLedgerEra era))
    -> Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setMetadataHash txAuxData =
    shelleyBasedEraConstraints sbe $
      Endo $
        L.auxDataHashTxBodyL
          .~ maybe SNothing (SJust . L.hashTxAuxData) txAuxData

  -- \| Following ledger's own script integrity hash computation (there is
  -- no reusable function for this in ledger, see e.g.
  -- 'Cardano.Api.Tx.Internal.Body.convPParamsToScriptIntegrityHash' for the
  -- legacy API's equivalent): the hash is only computed when there is
  -- something for it to cover, and it is an error to have something to
  -- cover without protocol parameters to compute it with.
  setScriptIntegrityHash
    :: TxBodyScriptData era
    -> [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
    -> Either CompatibleTxError (Endo (L.TxBody L.TopTx (ShelleyLedgerEra era)))
  setScriptIntegrityHash scriptData allWitnesses =
    case forEraMaybeEon era of
      Nothing -> Right mempty
      Just aeo ->
        alonzoEraOnwardsConstraints aeo $ do
          let
            datumsAndRedeemers
              :: (L.TxDats (ShelleyLedgerEra era), L.Redeemers (ShelleyLedgerEra era))
            datumsAndRedeemers = case scriptData of
              TxBodyScriptData _ ds rs -> (ds, rs)
              TxBodyNoScriptData -> (mempty, L.Redeemers mempty)

            (datums, redeemers) = datumsAndRedeemers

            languages :: Set L.Language
            languages = fromList $ mapMaybe (getAnyWitnessPlutusLanguage . snd) allWitnesses

            shouldCalculateHash =
              not $
                null (redeemers ^. L.unRedeemersL)
                  && null (datums ^. L.unTxDatsL)
                  && null languages

          if not shouldCalculateHash
            then Right mempty
            else case compatibleTxProtocolParams extraContent of
              Nothing -> Left CompatibleTxMissingScriptIntegrityPParams
              Just protocolParams ->
                Right $
                  Endo $
                    L.scriptIntegrityHashTxBodyL
                      .~ SJust
                        ( L.hashScriptIntegrity $
                            L.ScriptIntegrity
                              redeemers
                              datums
                              (Set.map (L.getLanguageView protocolParams) languages)
                        )

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
         , Maybe (StakeCredential, Exp.AnyWitness (ShelleyLedgerEra era))
         )
       ]
  indexedTxCerts = indexTxCertificates txCertificates'

  setScriptWitnesses
    :: TxBodyScriptData era
    -> [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
    -> L.TxWits (ShelleyLedgerEra era)
    -> L.TxWits (ShelleyLedgerEra era)
  setScriptWitnesses sData scriptWitnesses =
    appEndos
      [ monoidForEraInEon
          era
          ( \aeo ->
              alonzoEraOnwardsConstraints aeo $
                obtainAlonzoScriptPurposeConstraints aeo $
                  Endo $ do
                    let (datums, redeemers) = case sData of
                          TxBodyScriptData _ ds rs -> (ds, rs)
                          TxBodyNoScriptData -> (mempty, L.Redeemers mempty)
                    -- 'getAnyWitnessScript' covers both plutus AND simple
                    -- scripts; the simple ones it returns here overlap
                    -- harmlessly (same hash, same script) with the
                    -- allegra-onwards branch below, which is still needed
                    -- for simple scripts in the pre-Alonzo eras this
                    -- branch does not run in.
                    let plutusAndSimpleScripts =
                          mapMaybe (getAnyWitnessScript . snd) scriptWitnesses
                    (L.datsTxWitsL .~ datums)
                      . (L.rdmrsTxWitsL %~ (<> redeemers))
                      . ( L.scriptTxWitsL
                            %~ (<> Map.fromList [(L.hashScript sw, sw) | sw <- plutusAndSimpleScripts])
                        )
          )
      , monoidForEraInEon
          era
          ( \aeo -> allegraEraOnwardsConstraints aeo $ Endo $ do
              let ledgerScripts = convSimpleScripts sbe scriptWitnesses
              L.scriptTxWitsL
                %~ ( <>
                       Map.fromList
                         [ (L.hashScript sw, sw)
                         | sw <- ledgerScripts
                         ]
                   )
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
  -> Map L.DataHash (L.Data (ShelleyLedgerEra era))
  -> [(ScriptWitnessIndex, AnyWitness (ShelleyLedgerEra era))]
  -> TxBodyScriptData era
convScriptData' sbe extraDatums scriptWitnesses =
  forEraInEon
    (convert sbe)
    TxBodyNoScriptData
    ( \w ->
        alonzoEraOnwardsConstraints w $
          let redeemers = getAnyPlutusScriptWitnessRedeemerPointerMap w scriptWitnesses
              datums = mconcat [getAnyWitnessScriptData wit | (_, wit) <- scriptWitnesses]
              supplementalDatums = Alonzo.TxDats extraDatums
           in TxBodyScriptData w (datums <> supplementalDatums) redeemers
    )

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
  :: ShelleyBasedEra era
  -> Set L.TxIn
  -- ^ The final, sorted set of ledger inputs; see 'ledgerTxIns' at the
  -- 'createCompatibleTx' call site.
  -> [Exp.TxOut (ShelleyLedgerEra era)]
  -> Lovelace
  -> L.TxBody L.TopTx (ShelleyLedgerEra era)
createCommonTxBody era ledgerTxIns outs txFee' =
  shelleyBasedEraConstraints era $
    let txOuts' = map (\(Exp.TxOut o) -> o) outs
     in L.mkBasicTxBody
          & L.inputsTxBodyL
            .~ ledgerTxIns
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

-- | Index certificates by the order they appear in the transaction, including
-- both witnessed and unwitnessed certs. See 'indexCertificatesWith' for which
-- certificate types are unwitnessed.
--
-- See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
indexTxCertificates
  :: Exp.TxCertificates (ShelleyLedgerEra era)
  -> [ ( ScriptWitnessIndex
       , Exp.Certificate (ShelleyLedgerEra era)
       , Maybe (StakeCredential, AnyWitness (ShelleyLedgerEra era))
       )
     ]
indexTxCertificates (Exp.TxCertificates certsWits) =
  indexCertificatesWith $ toList certsWits
