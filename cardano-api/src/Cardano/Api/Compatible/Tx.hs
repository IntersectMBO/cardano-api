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
  , CompatibleTxBodyContent (..)
  , CompatibleTxError (..)
  , defaultCompatibleTxBodyContent
  , createCompatibleTx
  , addWitnesses
  )
where

import Cardano.Api.Era
import Cardano.Api.Error (Error (..))
import Cardano.Api.Experimental.Era (obtainCommonConstraints)
import Cardano.Api.Experimental.Era qualified as Exp
import Cardano.Api.Experimental.Plutus
  ( Witnessable (..)
  , WitnessableItem (..)
  , getAnyWitnessRedeemerPointerMap
  , obtainAlonzoScriptPurposeConstraints
  )
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Experimental.Tx.Internal.AnyWitness qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Monad.Error ((?!))
import Cardano.Api.ProtocolParameters
import Cardano.Api.Tx.Internal.Body hiding
  ( convCertificates
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

import Data.List qualified as L
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Monoid
import Data.OSet.Strict (OSet)
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts
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

-- | The content of a transaction 'createCompatibleTx' builds.
data CompatibleTxBodyContent era = CompatibleTxBodyContent
  { compatibleTxIns :: [(TxIn, Exp.AnyWitness (ShelleyLedgerEra era))]
  -- ^ Inputs with witnesses. Key-witnessed inputs must use 'Exp.AnyKeyWitnessPlaceholder', or redeemer pointers shift.
  , compatibleTxOuts :: [Exp.TxOut (ShelleyLedgerEra era)]
  -- ^ Transaction outputs.
  , compatibleTxSupplementalDatums :: Map L.DataHash (L.Data (ShelleyLedgerEra era))
  -- ^ Supplemental datums to include in the witness set.
  , compatibleTxFee :: Lovelace
  -- ^ Fee.
  , compatibleTxProtocolUpdate :: AnyProtocolUpdate era
  -- ^ Era-appropriate protocol update: Shelley-Babbage proposal, Conway-onwards proposal procedure, or none.
  , compatibleTxVotingProcedures :: AnyVote era
  -- ^ Governance votes, Conway onwards; 'NoVotes' otherwise.
  , compatibleTxCertificates :: Exp.TxCertificates (ShelleyLedgerEra era)
  -- ^ Certificates, witnessed or not.
  , compatibleTxInsCollateral :: [TxIn]
  -- ^ Collateral inputs. Meaningful only Alonzo onwards; supply non-empty only when using plutus spending witnesses.
  , compatibleTxProtocolParams :: Maybe (L.PParams (ShelleyLedgerEra era))
  -- ^ Needed to compute the script integrity hash when plutus witnesses are present.
  -- 'Nothing' is only safe when there are none; see 'CompatibleTxMissingScriptIntegrityPParams'.
  , compatibleTxMetadata :: TxMetadataInEra era
  -- ^ Transaction metadata to embed, or 'TxMetadataNone' for none.
  , compatibleTxValidityUpperBound :: Maybe SlotNo
  -- ^ Last slot the transaction can be included in, or 'Nothing' for unbounded.
  }

-- | 'CompatibleTxBodyContent' with everything empty: no inputs, outputs,
-- certificates, votes, protocol update, collateral, protocol parameters,
-- metadata or validity upper bound.
--
-- The 'ShelleyBasedEra' witness is only needed to build the default
-- 'NoPParamsUpdate'.
defaultCompatibleTxBodyContent :: ShelleyBasedEra era -> CompatibleTxBodyContent era
defaultCompatibleTxBodyContent sbe =
  CompatibleTxBodyContent
    { compatibleTxIns = []
    , compatibleTxOuts = []
    , compatibleTxSupplementalDatums = mempty
    , compatibleTxFee = 0
    , compatibleTxProtocolUpdate = NoPParamsUpdate sbe
    , compatibleTxVotingProcedures = NoVotes
    , compatibleTxCertificates = Exp.TxCertificates OMap.empty
    , compatibleTxInsCollateral = []
    , compatibleTxProtocolParams = Nothing
    , compatibleTxMetadata = TxMetadataNone
    , compatibleTxValidityUpperBound = Nothing
    }

-- | Errors that can occur while assembling a 'Tx' with 'createCompatibleTx'.
data CompatibleTxError
  = -- | Plutus script witnesses are present (non-empty redeemers, datums
    -- or plutus languages), but 'compatibleTxProtocolParams' is
    -- 'Nothing', so the ledger's required script integrity hash cannot
    -- be computed.
    CompatibleTxMissingScriptIntegrityPParams
  deriving Show

instance Error CompatibleTxError where
  prettyError err =
    case err of
      CompatibleTxMissingScriptIntegrityPParams ->
        "Plutus script witnesses are present but no protocol parameters were supplied "
          <> "to compute the script integrity hash."

-- | Create a transaction in any shelley based era
createCompatibleTx
  :: forall era
   . ShelleyBasedEra era
  -> CompatibleTxBodyContent era
  -> Either CompatibleTxError (Tx era)
createCompatibleTx sbe bodyContent =
  shelleyBasedEraConstraints sbe $ do
    integrityHashUpdate <- setScriptIntegrityHash sData allWitnesses

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
          %~ setScriptWitnesses sData allWitnesses
        & updateVotingProcedures
        & L.auxDataTxL
          .~ maybeToStrictMaybe txAuxData
 where
  era = toCardanoEra sbe
  appEndos = appEndo . mconcat

  -- Local synonyms for bodyContent's fields, used throughout below.
  ins = compatibleTxIns bodyContent
  outs = compatibleTxOuts bodyContent
  extraDatums = compatibleTxSupplementalDatums bodyContent
  txFee' = compatibleTxFee bodyContent
  anyProtocolUpdate = compatibleTxProtocolUpdate bodyContent
  anyVote = compatibleTxVotingProcedures bodyContent
  txCertificates' = compatibleTxCertificates bodyContent

  -- Order must stay OMap insertion order; the shared Witnessable-based
  -- indexing (via 'extractWitnessableProposals') preserves it.
  proposalWitnesses
    :: [(Witnessable ProposalItem (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  proposalWitnesses =
    case anyProtocolUpdate of
      ProtocolUpdate{} -> []
      NoPParamsUpdate{} -> []
      ProposalProcedures conwayOnwards proposalProcedures ->
        Exp.obtainCommonConstraints (convert conwayOnwards) $
          Exp.extractWitnessableProposals $
            Just proposalProcedures

  updateTxBody :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  updateTxBody =
    case anyProtocolUpdate of
      ProtocolUpdate shelleyToBabbageEra updateProposal ->
        let ledgerPParamsUpdate = toLedgerUpdate sbe updateProposal
         in shelleyToBabbageEraConstraints shelleyToBabbageEra $
              Endo $ \txb ->
                txb & L.updateTxBodyL .~ SJust ledgerPParamsUpdate
      NoPParamsUpdate _ -> mempty
      ProposalProcedures conwayOnwards proposalProcedures ->
        shelleyBasedEraConstraints sbe $
          let Exp.TxProposalProcedures propMap = proposalProcedures
              proposals :: OSet (L.ProposalProcedure (ShelleyLedgerEra era))
              proposals = fromList $ fst <$> toList propMap
              -- append proposal reference inputs & set proposal procedures
              referenceInputs =
                [ toShelleyTxIn txIn
                | (_, wit) <- proposalWitnesses
                , txIn <- maybeToList $ getAnyWitnessReferenceInput wit
                ]
           in obtainCommonConstraints (convert conwayOnwards) $
                Endo $
                  (L.referenceInputsTxBodyL %~ (<> fromList referenceInputs))
                    . (L.proposalProceduresTxBodyL .~ proposals)

  -- Flat witnesses from all four script-witnessable categories
  -- (certificates, proposals, votes, inputs), for
  -- 'setScriptIntegrityHash' and 'setScriptWitnesses' to collect
  -- datums, scripts and plutus languages from. Only the redeemer
  -- pointer map (built inside 'convScriptData'') needs per-category
  -- indexing.
  allWitnesses :: [AnyWitness (ShelleyLedgerEra era)]
  allWitnesses =
    witnessedCertWitnesses
      <> map snd proposalWitnesses
      <> map snd voteWitnesses
      <> map snd ins

  sData :: Maybe (L.TxDats (ShelleyLedgerEra era), L.Redeemers (ShelleyLedgerEra era))
  sData = convScriptData' sbe ins txCertificates' proposalWitnesses voteWitnesses allWitnesses extraDatums

  txAuxData :: Maybe (L.TxAuxData (ShelleyLedgerEra era))
  txAuxData = toAuxiliaryData sbe (compatibleTxMetadata bodyContent) TxAuxScriptsNone

  -- The final set of ledger inputs, in ascending 'Ord' order ('Data.Set').
  -- This is the order the ledger serialises tx inputs in, and the order
  -- it resolves 'Spending' redeemer pointers against.
  --
  -- Spending redeemer pointers are indexed against this same order:
  -- 'witnessableTxIns' nubs duplicate (TxIn, witness) pairs, and the
  -- shared Witnessable indexing machinery sorts 'WitTxIn' entries by
  -- 'TxIn' ('compareWitnesses') to match this 'Set' order. Never index
  -- against the order 'ins' was supplied in.
  ledgerTxIns :: Set L.TxIn
  ledgerTxIns = fromList $ map (toShelleyTxIn . fst) ins

  -- The witness of every witnessed certificate in 'txCertificates''.
  -- Unwitnessed certs contribute nothing here: no reference input, datum,
  -- script or language. They only matter for redeemer indexing, which
  -- 'convScriptData'' handles separately with a placeholder witness.
  witnessedCertWitnesses :: [AnyWitness (ShelleyLedgerEra era)]
  witnessedCertWitnesses =
    [wit | (_, Just wit) <- toList certsWits]
   where
    Exp.TxCertificates certsWits = txCertificates'

  -- The witness of every vote in 'anyVote', witnessed or not (unwitnessed
  -- votes get 'AnyKeyWitnessPlaceholder'). Mirrors 'proposalWitnesses'.
  voteWitnesses
    :: [(Witnessable VoterItem (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  voteWitnesses =
    case anyVote of
      NoVotes -> []
      VotingProcedures conwayOnwards votingProcedures ->
        Exp.obtainCommonConstraints (convert conwayOnwards) $
          Exp.extractWitnessableVotes $
            Just votingProcedures

  setCerts :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setCerts =
    shelleyBasedEraConstraints sbe $
      Endo $
        L.certsTxBodyL .~ convCertificates txCertificates'

  -- Uses '%~ (<>)', not '.~', so it does not clobber reference inputs
  -- already appended by 'updateTxBody' (the 'ProposalProcedures' case
  -- above). Reference inputs collected here come from witnessed
  -- certificates, witnessed votes and witnessed spending inputs.
  setRefInputs :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setRefInputs = do
    let refInputs =
          [ toShelleyTxIn refInput
          | wit <- witnessedCertWitnesses <> map snd voteWitnesses <> map snd ins
          , refInput <- maybeToList $ getAnyWitnessReferenceInput wit
          ]

    monoidForEraInEon era $ \beo ->
      babbageEraOnwardsConstraints beo $
        Endo $
          L.referenceInputsTxBodyL %~ (<> fromList refInputs)

  -- Alonzo onwards only; a no-op below that. The list is expected to be
  -- empty pre-Alonzo anyway, since collateral only matters for plutus
  -- spending.
  setCollateralIns :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setCollateralIns =
    monoidForEraInEon era $ \aeo ->
      alonzoEraOnwardsConstraints aeo $
        Endo $
          L.collateralInputsTxBodyL
            .~ (fromList . map toShelleyTxIn $ compatibleTxInsCollateral bodyContent)

  -- Compatibility lens over 'ttlTxBodyL' (Shelley) and 'vldtTxBodyL'
  -- (Allegra onwards), preserving the validity interval's lower bound.
  setValidityUpperBound :: Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setValidityUpperBound =
    Endo $ \txb ->
      A.unTxBody $
        A.LedgerTxBody txb
          & A.invalidHereAfterTxBodyL sbe
            .~ compatibleTxValidityUpperBound bodyContent

  -- Only the body-side auxiliary data hash; the auxiliary data itself is
  -- set on the 'L.Tx' via 'L.auxDataTxL' in the main function body.
  setMetadataHash
    :: Maybe (L.TxAuxData (ShelleyLedgerEra era))
    -> Endo (L.TxBody L.TopTx (ShelleyLedgerEra era))
  setMetadataHash auxData =
    shelleyBasedEraConstraints sbe $
      Endo $
        L.auxDataHashTxBodyL
          .~ maybe SNothing (SJust . L.hashTxAuxData) auxData

  -- The hash is only computed when there is something for it to cover.
  -- It is an error to have something to cover without protocol
  -- parameters to compute it with.
  --
  -- Follows ledger's own script integrity hash computation; ledger has
  -- no reusable function for this (see
  -- 'Cardano.Api.Tx.Internal.Body.convPParamsToScriptIntegrityHash' for
  -- the legacy API's equivalent).
  setScriptIntegrityHash
    :: Maybe (L.TxDats (ShelleyLedgerEra era), L.Redeemers (ShelleyLedgerEra era))
    -> [AnyWitness (ShelleyLedgerEra era)]
    -> Either CompatibleTxError (Endo (L.TxBody L.TopTx (ShelleyLedgerEra era)))
  setScriptIntegrityHash scriptData witnesses =
    monoidForEraInEonA era $ \aeo ->
      alonzoEraOnwardsConstraints aeo $ do
        let
          (datums, redeemers) = fromMaybe mempty scriptData

          languages :: Set L.Language
          languages = fromList $ mapMaybe getAnyWitnessPlutusLanguage witnesses

          shouldCalculateHash =
            not $
              null (redeemers ^. L.unRedeemersL)
                && null (datums ^. L.unTxDatsL)
                && null languages

        if not shouldCalculateHash
          then pure mempty
          else do
            protocolParams <-
              compatibleTxProtocolParams bodyContent
                ?! CompatibleTxMissingScriptIntegrityPParams
            pure $
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
    obtainCommonConstraints (convert conwayOnwards) $
      (L.bodyTxL . L.votingProceduresTxBodyL) .~ votingProcedures

  setScriptWitnesses
    :: Maybe (L.TxDats (ShelleyLedgerEra era), L.Redeemers (ShelleyLedgerEra era))
    -> [AnyWitness (ShelleyLedgerEra era)]
    -> L.TxWits (ShelleyLedgerEra era)
    -> L.TxWits (ShelleyLedgerEra era)
  setScriptWitnesses scriptData scriptWitnesses = plutusAdditions . simpleScriptAdditions
   where
    plutusAdditions :: L.TxWits (ShelleyLedgerEra era) -> L.TxWits (ShelleyLedgerEra era)
    plutusAdditions =
      forEraInEon era id $ \aeo ->
        alonzoEraOnwardsConstraints aeo $
          obtainAlonzoScriptPurposeConstraints aeo $
            let
              (datums, redeemers) =
                fromMaybe mempty scriptData
                  :: (L.TxDats (ShelleyLedgerEra era), L.Redeemers (ShelleyLedgerEra era))
              -- 'getAnyWitnessScript' covers both plutus and simple
              -- scripts. The simple ones overlap harmlessly (same
              -- hash, same script) with the allegra-onwards branch
              -- below, which is still needed for pre-Alonzo eras
              -- that this branch does not run in.
              plutusAndSimpleScripts =
                mapMaybe getAnyWitnessScript scriptWitnesses
             in
              (L.datsTxWitsL .~ datums)
                . (L.rdmrsTxWitsL %~ (<> redeemers))
                . ( L.scriptTxWitsL
                      %~ (<> Map.fromList [(L.hashScript sw, sw) | sw <- plutusAndSimpleScripts])
                  )

    simpleScriptAdditions :: L.TxWits (ShelleyLedgerEra era) -> L.TxWits (ShelleyLedgerEra era)
    simpleScriptAdditions =
      forEraInEon era id $ \aeo ->
        allegraEraOnwardsConstraints aeo $
          let ledgerScripts = convSimpleScripts sbe scriptWitnesses
           in L.scriptTxWitsL
                %~ ( <>
                       Map.fromList
                         [ (L.hashScript sw, sw)
                         | sw <- ledgerScripts
                         ]
                   )

convSimpleScripts
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> [Exp.AnyWitness (ShelleyLedgerEra era)]
  -> [L.Script ledgerera]
convSimpleScripts sbe scriptWitnesses =
  catMaybes
    [ shelleyBasedEraConstraints sbe $ Exp.getAnyWitnessSimpleScript anywit
    | anywit <- scriptWitnesses
    ]

convCertificates
  :: Exp.TxCertificates (ShelleyLedgerEra era)
  -> Seq.StrictSeq (L.TxCert (ShelleyLedgerEra era))
convCertificates (Exp.TxCertificates cs) =
  fromList . map (\(Exp.Certificate c, _) -> c) $ toList cs

convScriptData'
  :: ShelleyBasedEra era
  -> [(TxIn, Exp.AnyWitness (ShelleyLedgerEra era))]
  -> Exp.TxCertificates (ShelleyLedgerEra era)
  -> [(Witnessable ProposalItem (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  -> [(Witnessable VoterItem (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  -> [AnyWitness (ShelleyLedgerEra era)]
  -> Map L.DataHash (L.Data (ShelleyLedgerEra era))
  -> Maybe (L.TxDats (ShelleyLedgerEra era), L.Redeemers (ShelleyLedgerEra era))
convScriptData' sbe ins txCertificates' proposalWits voteWits allWitnesses extraDatums =
  forEraInEon
    (convert sbe)
    Nothing
    ( \w ->
        alonzoEraOnwardsConstraints w $ do
          let
            -- Four disjoint per-category redeemer maps merged into one.
            -- Ledger 'L.PlutusPurpose' tags (Spending/Certifying/Voting/Proposing)
            -- never collide across categories, so '<>' below cannot drop
            -- or overwrite an entry.
            --
            -- All four share the Witnessable-based indexing that
            -- 'Cardano.Api.Experimental.Tx.Internal.BodyContent.New.makeUnsignedTx'
            -- uses, via 'getAnyWitnessRedeemerPointerMap'.
            certRedeemers = getAnyWitnessRedeemerPointerMap $ witnessableCerts txCertificates'
            inputRedeemers = getAnyWitnessRedeemerPointerMap $ witnessableTxIns ins
            proposalRedeemers = getAnyWitnessRedeemerPointerMap proposalWits
            voteRedeemers = getAnyWitnessRedeemerPointerMap voteWits
            redeemers = certRedeemers <> inputRedeemers <> voteRedeemers <> proposalRedeemers

            datums = mconcat [getAnyWitnessScriptData wit | wit <- allWitnesses]
            supplementalDatums = Alonzo.TxDats extraDatums
          Just (datums <> supplementalDatums, redeemers)
    )

-- | Copy of an experimental extractWitnessableTxIns not requiring 'IsEra era'
witnessableTxIns
  :: L.AlonzoEraScript ledgerera
  => [(TxIn, AnyWitness ledgerera)]
  -> [(Witnessable TxInItem ledgerera, AnyWitness ledgerera)]
witnessableTxIns txIns' = L.nub [(WitTxIn txIn, wit) | (txIn, wit) <- txIns']

-- Every certificate, witnessed or not, wrapped as a 'Witnessable'.
--
-- Unwitnessed certs MUST be included, paired with
-- 'AnyKeyWitnessPlaceholder'. The ledger resolves 'Certifying' indices
-- against the full 'certsTxBodyL' sequence, not just the witnessed
-- subset, so dropping an unwitnessed cert here would shift every later
-- index.
--
-- Mirrors
-- 'Cardano.Api.Experimental.Tx.Internal.BodyContent.New.extractWitnessableCertificates'.
witnessableCerts
  :: L.AlonzoEraScript ledgerera
  => L.EraTxCert ledgerera
  => Exp.TxCertificates ledgerera
  -> [(Witnessable CertItem ledgerera, AnyWitness ledgerera)]
witnessableCerts (Exp.TxCertificates certsWits) =
  [ (WitTxCert cert, fromMaybe AnyKeyWitnessPlaceholder mWit)
  | (Exp.Certificate cert, mWit) <- toList certsWits
  ]

createCommonTxBody
  :: ShelleyBasedEra era
  -> Set L.TxIn
  -- ^ The final set of ledger inputs, in ascending 'Ord' order. See
  -- 'ledgerTxIns' at the 'createCompatibleTx' call site.
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
