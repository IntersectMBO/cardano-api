{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Experimental.Tx.Internal.BodyContent.New
  ( TxBodyContent (..)
  , TxCertificates (..)
  , TxCollateral (..)
  , TxExtraKeyWitnesses (..)
  , TxInsReference (..)
  , TxMintValue (..)
  , TxOut (..)
  , TxProposalProcedures (..)
  , TxVotingProcedures (..)
  , TxValidityLowerBound (..)
  , TxWithdrawals (..)
  , collectTxBodyScriptWitnessRequirements
  , createUnsignedTx
  , extractAllIndexedPlutusScriptWitnesses
  , txMintValueToValue
  )
where

import Cardano.Api.Address
import Cardano.Api.Experimental.Certificate qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus
  ( AnyIndexedPlutusScriptWitness (..)
  , IndexedPlutusScriptWitness (..)
  , PlutusScriptDatum (..)
  , PlutusScriptWitness (..)
  , Witnessable (..)
  , WitnessableItem (..)
  , createIndexedPlutusScriptWitnesses
  )
import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
  ( AnyWitness (..)
  , getAnyWitnessPlutusLanguage
  , getAnyWitnessSimpleScript
  , getPlutusDatum
  )
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
  ( TxScriptWitnessRequirements (..)
  )
import Cardano.Api.Experimental.Tx.Internal.Type
import Cardano.Api.Key.Internal
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script (toAlonzoExUnits)
import Cardano.Api.Plutus.Internal.ScriptData
import Cardano.Api.Tx.Internal.Body
  ( CtxTx
  , TxIn
  , toShelleyTxIn
  , toShelleyWithdrawal
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal (PolicyAssets, PolicyId, Value, policyAssetsToValue, toMaryValue)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Conway.Scripts qualified as L
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Dijkstra.Scripts qualified as Dijkstra
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad
import Data.Functor
import Data.List qualified as List
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Lens.Micro

createUnsignedTx
  :: forall era
   . Era era
  -> TxBodyContent CtxTx (LedgerEra era)
  -> Either String (UnsignedTx era)
createUnsignedTx DijkstraEra _ = error "makeUnsignedTx: Dijkstra era not supported yet"
createUnsignedTx era@ConwayEra bc = obtainCommonConstraints era $ do
  TxScriptWitnessRequirements languages scripts datums redeemers <-
    collectTxBodyScriptWitnessRequirements bc

  -- cardano-api types
  let apiMintValue = txMintValue bc
      apiReferenceInputs = txInsReference bc
      apiExtraKeyWitnesses = txExtraKeyWits bc

      -- Ledger types
      txins = convTxIns $ txIns bc
      collTxIns = convCollateralTxIns bc
      refTxIns = convReferenceInputs apiReferenceInputs
      outs = fromList [o | TxOut o _ <- txOuts bc]
      protocolParameters = txProtocolParams bc
      fee = txFee bc
      withdrawals = convWithdrawals $ txWithdrawals bc
      certs = convCertificates $ txCertificates bc
      retCollateral = returnCollateral <$> txCollateral bc
      totCollateral = totalCollateral <$> txCollateral bc
      txAuxData = toAuxiliaryData (txMetadata bc) (txAuxScripts bc)
      scriptValidity = scriptValidityToIsValid $ txScriptValidity bc
      scriptIntegrityHash =
        convPParamsToScriptIntegrityHash
          protocolParameters
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
          & L.totalCollateralTxBodyL .~ L.maybeToStrictMaybe totCollateral
          & L.collateralReturnTxBodyL .~ L.maybeToStrictMaybe retCollateral
          & L.feeTxBodyL .~ fee
          & L.vldtTxBodyL . L.invalidBeforeL .~ txValidityLowerBound bc
          & L.vldtTxBodyL . L.invalidHereAfterL .~ txValidityUpperBound bc
          & L.reqSignerHashesTxBodyL .~ setReqSignerHashes
          & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
          & L.withdrawalsTxBodyL .~ withdrawals
          & L.certsTxBodyL .~ certs
          & L.mintTxBodyL .~ setMint
          & L.auxDataHashTxBodyL .~ L.maybeToStrictMaybe (Ledger.hashTxAuxData <$> txAuxData)

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
      & L.auxDataTxL .~ L.maybeToStrictMaybe (toAuxiliaryData (txMetadata bc) (txAuxScripts bc))
      & L.isValidTxL .~ scriptValidity

convTxIns :: [(TxIn, AnyWitness era)] -> Set L.TxIn
convTxIns inputs =
  Set.fromList [toShelleyTxIn txin | (txin, _) <- inputs]

convCollateralTxIns :: TxBodyContent ctx era -> Set L.TxIn
convCollateralTxIns b =
  fromList (map toShelleyTxIn $ txInsCollateral b)

convReferenceInputs :: TxInsReference era -> Set L.TxIn
convReferenceInputs (TxInsReference ins _) =
  fromList $ map toShelleyTxIn ins

convWithdrawals :: TxWithdrawals era -> L.Withdrawals
convWithdrawals (TxWithdrawals ws) =
  toShelleyWithdrawal ws

convMintValue :: TxMintValue era -> L.MultiAsset
convMintValue v = do
  let L.MaryValue _coin multiAsset = toMaryValue $ txMintValueToValue v
  multiAsset

convExtraKeyWitnesses
  :: TxExtraKeyWitnesses -> Set (L.KeyHash L.Witness)
convExtraKeyWitnesses (TxExtraKeyWitnesses khs) =
  fromList
    [ L.asWitness kh
    | PaymentKeyHash kh <- khs
    ]

convCertificates
  :: TxCertificates (LedgerEra era)
  -> Seq.StrictSeq (L.TxCert (LedgerEra era))
convCertificates (TxCertificates cs) =
  fromList . map (\(Exp.Certificate c, _) -> c) $ toList cs

convPParamsToScriptIntegrityHash
  :: forall era
   . IsEra era
  => Maybe (Ledger.PParams (LedgerEra era))
  -> L.Redeemers (LedgerEra era)
  -> L.TxDats (LedgerEra era)
  -> Set Plutus.Language
  -> StrictMaybe L.ScriptIntegrityHash
convPParamsToScriptIntegrityHash mTxProtocolParams redeemers datums languages = obtainCommonConstraints (useEra @era) $ do
  pp <- L.maybeToStrictMaybe mTxProtocolParams
  -- This logic is copied from ledger, because their code is not reusable
  -- c.f. https://github.com/IntersectMBO/cardano-ledger/commit/5a975d9af507c9ee835a86d3bb77f3e2670ad228#diff-8236dfec9688f22550b91fc9a87af9915523ab9c5bd817218ecceec8ca7a789bR282
  let shouldCalculateHash =
        not $
          null (redeemers ^. L.unRedeemersL)
            && null (datums ^. L.unTxDatsL)
            && null languages
  guard shouldCalculateHash
  let scriptIntegrity = L.ScriptIntegrity redeemers datums (Set.map (L.getLanguageView pp) languages)
  pure $ L.hashScriptIntegrity scriptIntegrity

convProposalProcedures
  :: forall era
   . IsEra era
  => Maybe (TxProposalProcedures (LedgerEra era)) -> OSet (L.ProposalProcedure (LedgerEra era))
convProposalProcedures Nothing = OSet.empty
convProposalProcedures (Just (TxProposalProcedures proposals)) =
  obtainCommonConstraints (useEra @era) $ fromList $ fst <$> toList proposals

convVotingProcedures
  :: Maybe (TxVotingProcedures (LedgerEra era)) -> L.VotingProcedures (LedgerEra era)
convVotingProcedures (Just (TxVotingProcedures vps _)) = vps
convVotingProcedures Nothing = L.VotingProcedures mempty

-- | Auxiliary data consists of the tx metadata
-- and the auxiliary scripts, and the auxiliary script data.
toAuxiliaryData
  :: forall era
   . IsEra era
  => TxMetadata
  -> [SimpleScript (LedgerEra era)]
  -> Maybe (L.TxAuxData (LedgerEra era))
toAuxiliaryData txMData ss' =
  let ms = toShelleyMetadata $ unTxMetadata txMData
   in case useEra @era of
        ConwayEra ->
          -- guard (not (Map.null ms && null ss)) $>
          let ss = [L.NativeScript s | SimpleScript s <- ss']
           in guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
        DijkstraEra ->
          let ss = [L.NativeScript s | SimpleScript s <- ss']
           in guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss

eraSpecificLedgerTxBody
  :: Era era
  -> L.TxBody (LedgerEra era)
  -> TxBodyContent ctx (LedgerEra era)
  -> L.TxBody (LedgerEra era)
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
              .~ convProposalProcedures propProcedures
            & L.votingProceduresTxBodyL
              .~ convVotingProcedures voteProcedures
            & L.treasuryDonationTxBodyL
              .~ fromMaybe (L.Coin 0) treasuryDonation
            & L.currentTreasuryValueTxBodyL
              .~ L.maybeToStrictMaybe currentTresuryValue

data TxOut ctx era where
  TxOut :: L.EraTxOut era => L.TxOut era -> Maybe (Datum ctx era) -> TxOut ctx era

deriving instance (Show (TxOut ctx era))

deriving instance (Eq (TxOut ctx era))

data Datum ctx era where
  TxOutDatumHash
    :: L.DataHash
    -> Datum ctx era
  TxOutSupplementalDatum
    :: L.DataHash
    -> L.Data era
    -> Datum CtxTx era
  TxOutDatumInline
    :: L.DataHash
    -> L.Data era
    -> Datum ctx era

deriving instance (Show (Datum ctx era))

deriving instance (Eq (Datum ctx era))

extractDatumsAndHashes :: Datum ctx era -> Maybe (L.DataHash, L.Data era)
extractDatumsAndHashes TxOutDatumHash{} = Nothing
extractDatumsAndHashes (TxOutSupplementalDatum h d) = Just (h, d)
extractDatumsAndHashes (TxOutDatumInline h d) = Just (h, d)

data TxInsReference era = TxInsReference [TxIn] (Set (Datum CtxTx era))

data TxCollateral era
  = TxCollateral
  { totalCollateral :: L.Coin
  , returnCollateral :: L.TxOut era
  }

newtype TxValidityLowerBound = TxValidityLowerBound L.SlotNo

newtype TxExtraKeyWitnesses = TxExtraKeyWitnesses [Hash PaymentKey]

newtype TxWithdrawals era = TxWithdrawals [(StakeAddress, L.Coin, AnyWitness era)]

newtype TxCertificates era
  = TxCertificates
      ( OMap
          (Exp.Certificate era)
          (Maybe (StakeCredential, AnyWitness era))
      )

-- This is incorrect. Only scripts can witness minting!
newtype TxMintValue era
  = TxMintValue
      ( Map
          PolicyId
          ( PolicyAssets
          , AnyWitness era
          )
      )

-- | Convert 'TxMintValue' to a more handy 'Value'.
txMintValueToValue :: TxMintValue era -> Value
txMintValueToValue (TxMintValue policiesWithAssets) =
  mconcat
    [ policyAssetsToValue policyId assets
    | (policyId, (assets, _witness)) <- toList policiesWithAssets
    ]

newtype TxProposalProcedures era
  = TxProposalProcedures
      ( OMap
          (L.ProposalProcedure era)
          (Maybe (AnyWitness era))
      )

data TxVotingProcedures era
  = TxVotingProcedures
      (L.VotingProcedures era)
      (Map L.Voter (AnyWitness era))

data TxBodyContent ctx era
  = TxBodyContent
  { txIns :: [(TxIn, AnyWitness era)]
  , txInsCollateral :: [TxIn]
  , txInsReference :: TxInsReference era
  , txOuts :: [TxOut ctx era]
  , txCollateral :: Maybe (TxCollateral era)
  , txFee :: L.Coin
  , txValidityLowerBound :: Maybe L.SlotNo
  , txValidityUpperBound :: Maybe L.SlotNo
  , txMetadata :: TxMetadata
  , txAuxScripts :: [SimpleScript era]
  , txExtraKeyWits :: TxExtraKeyWitnesses
  , txProtocolParams :: Maybe (L.PParams era)
  , txWithdrawals :: TxWithdrawals era
  , txCertificates :: TxCertificates era
  , txMintValue :: TxMintValue era
  , txScriptValidity :: ScriptValidity
  , txProposalProcedures :: Maybe (TxProposalProcedures era)
  , txVotingProcedures :: Maybe (TxVotingProcedures era)
  , txCurrentTreasuryValue :: Maybe L.Coin
  , -- -- ^ Current treasury value
    txTreasuryDonation :: Maybe L.Coin
    -- -- ^ Treasury donation to perform
  }

extractAllIndexedPlutusScriptWitnesses
  :: forall era ctx
   . Era era
  -> TxBodyContent ctx (LedgerEra era)
  -> Either
       CBOR.DecoderError
       [AnyIndexedPlutusScriptWitness (LedgerEra era)]
extractAllIndexedPlutusScriptWitnesses era b = obtainCommonConstraints era $ do
  let txInWits = extractWitnessableTxIns $ txIns b
      certWits = extractWitnessableCertificates $ txCertificates b
      mintWits = extractWitnessableMints $ txMintValue b
      withdrawalWits = extractWitnessableWithdrawals $ txWithdrawals b
      proposalScriptWits = extractWitnessableProposals $ txProposalProcedures b
      voteWits = extractWitnessableVotes $ txVotingProcedures b

  let indexedScriptTxInWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses txInWits
      indexedCertScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses certWits
      indexedMintScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses mintWits
      indexedWithdrawalScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses withdrawalWits
      indexedProposalScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses proposalScriptWits
      indexedVoteScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses voteWits
  return $
    mconcat
      [ indexedScriptTxInWits
      , indexedMintScriptWits
      , indexedCertScriptWits
      , indexedWithdrawalScriptWits
      , indexedProposalScriptWits
      , indexedVoteScriptWits
      ]

extractWitnessableTxIns
  :: forall era
   . IsEra era
  => [(TxIn, AnyWitness (LedgerEra era))]
  -> [(Witnessable TxInItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableTxIns tIns =
  obtainCommonConstraints (useEra @era) $
    List.nub [(WitTxIn txin, wit) | (txin, wit) <- tIns]

extractWitnessableCertificates
  :: forall era
   . IsEra era
  => TxCertificates (LedgerEra era)
  -> [(Witnessable CertItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableCertificates txCerts =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ ( WitTxCert cert stakeCred
        , wit
        )
      | (Exp.Certificate cert, Just (stakeCred, wit)) <- getCertificates txCerts
      ]
 where
  getCertificates (TxCertificates txcs) = toList txcs

extractWitnessableMints
  :: forall era
   . IsEra era
  => TxMintValue (LedgerEra era)
  -> [(Witnessable MintItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableMints mVal =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ (WitMint policyId policyAssets, wit)
      | (policyId, (policyAssets, wit)) <- getMints mVal
      ]
 where
  getMints (TxMintValue txms) = toList txms

extractWitnessableWithdrawals
  :: forall era
   . IsEra era
  => TxWithdrawals (LedgerEra era)
  -> [(Witnessable WithdrawalItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableWithdrawals txWithDrawals =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ (WitWithdrawal addr withAmt, wit)
      | (addr, withAmt, wit) <- getWithdrawals txWithDrawals
      ]
 where
  getWithdrawals (TxWithdrawals txws) = txws

extractWitnessableVotes
  :: forall era
   . IsEra era
  => Maybe (TxVotingProcedures (LedgerEra era))
  -> [(Witnessable VoterItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableVotes Nothing = []
extractWitnessableVotes (Just txVoteProc) =
  case useEra @era of
    DijkstraEra -> error "extractWitnessableVotes: Dijkstra era not supported"
    ConwayEra ->
      List.nub
        [ (WitVote vote, wit)
        | (vote, wit) <- getVotes txVoteProc
        ]
 where
  getVotes
    :: TxVotingProcedures (LedgerEra era)
    -> [(L.Voter, AnyWitness (LedgerEra era))]
  getVotes (TxVotingProcedures allVotingProcedures scriptWitnessedVotes) =
    [ (voter, wit)
    | (voter, _) <- toList $ L.unVotingProcedures allVotingProcedures
    , wit <- maybe [] return (Map.lookup voter scriptWitnessedVotes)
    ]

extractWitnessableProposals
  :: forall era
   . IsEra era
  => Maybe
       (TxProposalProcedures (LedgerEra era))
  -> [(Witnessable ProposalItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableProposals Nothing = []
extractWitnessableProposals (Just txPropProcedures) =
  List.nub
    [ (obtainCommonConstraints (useEra @era) (WitProposal prop), wit)
    | (prop, wit) <-
        getProposals txPropProcedures
    ]
 where
  getProposals
    :: TxProposalProcedures (LedgerEra era)
    -> [(L.ProposalProcedure (LedgerEra era), AnyWitness (LedgerEra era))]
  getProposals (TxProposalProcedures txps) =
    [ (p, wit)
    | (p, mScriptWit) <- obtainCommonConstraints (useEra @era) (toList txps)
    , wit <- maybe [] return mScriptWit
    ]

collectTxBodyScriptWitnessRequirements
  :: forall era
   . IsEra era
  => TxBodyContent CtxTx (LedgerEra era)
  -> Either
       String
       (TxScriptWitnessRequirements (LedgerEra era))
collectTxBodyScriptWitnessRequirements
  TxBodyContent
    { txIns
    , txInsReference
    , txOuts
    , txCertificates
    , txMintValue
    , txWithdrawals
    , txVotingProcedures
    , txProposalProcedures
    } = obtainCommonConstraints (useEra @era) $ do
    let supplementaldatums =
          TxScriptWitnessRequirements
            mempty
            mempty
            (getDatums txInsReference txOuts)
            mempty

    let txInWits = getTxScriptWitnessRequirements $ extractWitnessableTxIns txIns
        txWithdrawalWits = getTxScriptWitnessRequirements $ extractWitnessableWithdrawals txWithdrawals
        txCertWits = getTxScriptWitnessRequirements $ extractWitnessableCertificates txCertificates
        txMintWits = getTxScriptWitnessRequirements $ extractWitnessableMints txMintValue
        txVotingWits = getTxScriptWitnessRequirements $ extractWitnessableVotes txVotingProcedures
        txProposalWits = getTxScriptWitnessRequirements $ extractWitnessableProposals txProposalProcedures

    return $
      obtainMonoidConstraint (useEra @era) $
        mconcat
          [ supplementaldatums
          , txInWits
          , txWithdrawalWits
          , txCertWits
          , txMintWits
          , txVotingWits
          , txProposalWits
          ]

getTxScriptWitnessRequirements
  :: forall era witnessable
   . IsEra era
  => [(Witnessable witnessable (LedgerEra era), AnyWitness (LedgerEra era))]
  -> TxScriptWitnessRequirements (LedgerEra era)
getTxScriptWitnessRequirements wits =
  let era = useEra @era
      TxScriptWitnessRequirements l s d _ =
        obtainMonoidConstraint era $
          mconcat
            [ TxScriptWitnessRequirements
                (maybe mempty Set.singleton $ getAnyWitnessPlutusLanguage anyWit)
                (maybe mempty return $ getAnyWitnessScript anyWit)
                (getAnyWitnessScriptData anyWit)
                (obtainCommonConstraints era mempty)
            | (_, anyWit) <- wits
            ]
   in TxScriptWitnessRequirements l s d (getAnyWitnessRedeemerPointerMap wits)

-- | The transaction's redeemer pointer map allows the ledger to connect a redeemer and execution unit pairing to the relevant
-- script. The ledger basically reconstructs the indicies (redeemer pointers) of this map can then look up the relevant
-- execution units/redeemer pairing. NB: the redeemer pointer has been renamed to 'PlutusPurpose AsIndex' in the ledger.
getAnyWitnessRedeemerPointerMap
  :: forall era witnessable
   . IsEra era
  => [(Witnessable witnessable (LedgerEra era), AnyWitness (LedgerEra era))]
  -> L.Redeemers (LedgerEra era)
getAnyWitnessRedeemerPointerMap anyWit =
  constructRedeeemerPointerMap $
    obtainCommonConstraints (useEra @era) $
      createIndexedPlutusScriptWitnesses anyWit

constructRedeeemerPointerMap
  :: forall era
   . IsEra era
  => [AnyIndexedPlutusScriptWitness (LedgerEra era)]
  -> L.Redeemers (LedgerEra era)
constructRedeeemerPointerMap scriptWits =
  let redeemerPointers = map constructRedeemerPointer scriptWits
   in obtainCommonConstraints (useEra @era) $ mconcat redeemerPointers

-- | An 'IndexedPlutusScriptWitness' contains everything we need to construct a single
-- entry in the redeemer pointer map.
constructRedeemerPointer
  :: AnyIndexedPlutusScriptWitness (LedgerEra era)
  -> L.Redeemers (LedgerEra era)
constructRedeemerPointer (AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness _ purpose scriptWit)) =
  let PlutusScriptWitness _ _ _ redeemer execUnits = scriptWit
   in L.Redeemers $
        fromList [(purpose, (toAlonzoData redeemer, toAlonzoExUnits execUnits))]

obtainMonoidConstraint
  :: Era era
  -> (Monoid (TxScriptWitnessRequirements (LedgerEra era)) => a)
  -> a
obtainMonoidConstraint eon = case eon of
  ConwayEra -> id
  DijkstraEra -> id

getAnyWitnessScript
  :: forall era
   . IsEra era
  => AnyWitness (LedgerEra era)
  -> Maybe (L.Script (LedgerEra era))
getAnyWitnessScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessScript ss@(AnySimpleScriptWitness{}) =
  case useEra @era of
    ConwayEra -> obtainCommonConstraints (useEra @era) (getAnyWitnessSimpleScript ss)
    DijkstraEra -> obtainCommonConstraints (useEra @era) (getAnyWitnessSimpleScript ss)
getAnyWitnessScript ps@(AnyPlutusScriptWitness{}) =
  case useEra @era of
    ConwayEra -> L.PlutusScript <$> getAnyWitnessPlutusScript ps
    DijkstraEra -> L.PlutusScript <$> getAnyWitnessPlutusScript ps

getAnyWitnessPlutusScript
  :: forall era
   . IsEra era
  => AnyWitness (LedgerEra era)
  -> Maybe (L.PlutusScript (LedgerEra era))
getAnyWitnessPlutusScript AnyKeyWitnessPlaceholder = Nothing
getAnyWitnessPlutusScript (AnySimpleScriptWitness _) = Nothing
getAnyWitnessPlutusScript
  ( AnyPlutusScriptWitness
      (PlutusScriptWitness l (PScript (PlutusScriptInEra plutusScriptRunnable)) _ _ _)
    ) = return $ fromPlutusRunnable l plutusScriptRunnable
getAnyWitnessPlutusScript (AnyPlutusScriptWitness (PlutusScriptWitness _ (PReferenceScript{}) _ _ _)) =
  Nothing

-- It should be noted that 'PlutusRunnable' is constructed via deserialization. The deserialization
-- instance lives in ledger and will fail for an invalid script language/era pairing.
fromPlutusRunnable
  :: forall era lang
   . IsEra era
  => L.SLanguage lang
  -> L.PlutusRunnable lang
  -> L.PlutusScript (LedgerEra era)
fromPlutusRunnable L.SPlutusV1 runnable =
  case useEra @era of
    ConwayEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in L.ConwayPlutusV1 plutusScript
    DijkstraEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in Dijkstra.DijkstraPlutusV1 plutusScript
fromPlutusRunnable L.SPlutusV2 runnable =
  case useEra @era of
    ConwayEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in L.ConwayPlutusV2 plutusScript
    DijkstraEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in Dijkstra.DijkstraPlutusV2 plutusScript
fromPlutusRunnable L.SPlutusV3 runnable =
  case useEra @era of
    ConwayEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in L.ConwayPlutusV3 plutusScript
    DijkstraEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in Dijkstra.DijkstraPlutusV3 plutusScript
fromPlutusRunnable L.SPlutusV4 runnable =
  case useEra @era of
    ConwayEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in error "fromPlutusRunnable: ConwayPlutusV4" plutusScript
    DijkstraEra ->
      let plutusScript = L.plutusFromRunnable runnable
       in Dijkstra.DijkstraPlutusV4 plutusScript

-- | NB this does not include datums from inline datums existing at tx outputs!
getAnyWitnessScriptData
  :: forall era. IsEra era => AnyWitness (LedgerEra era) -> L.TxDats (LedgerEra era)
getAnyWitnessScriptData AnyKeyWitnessPlaceholder = obtainCommonConstraints (useEra @era) mempty
getAnyWitnessScriptData AnySimpleScriptWitness{} = obtainCommonConstraints (useEra @era) mempty
getAnyWitnessScriptData (AnyPlutusScriptWitness (PlutusScriptWitness l _ scriptDatum _ _)) =
  let alonzoSdat = toAlonzoDatum l scriptDatum
   in case alonzoSdat of
        Nothing -> obtainCommonConstraints (useEra @era) mempty
        Just (d :: L.Data (LedgerEra era)) -> obtainCommonConstraints (useEra @era) $ L.TxDats $ fromList [(L.hashData d, d)]

toAlonzoDatum
  :: forall era lang purpose
   . IsEra era
  => L.SLanguage lang
  -> PlutusScriptDatum lang purpose
  -> Maybe (L.Data (LedgerEra era))
toAlonzoDatum l d =
  let mHashableData = getPlutusDatum l d
   in case mHashableData of
        Just h -> Just $ obtainCommonConstraints (useEra @era) $ toAlonzoData h
        Nothing -> Nothing

-- | Extract datum:
-- 1. supplemental datums from transaction outputs
-- 2. datums from reference inputs
--
-- Note that this function does not check whose datum hashes are present in the reference inputs. This means if there
-- are redundant datums in 'TxInsReference', a submission of such transaction will fail.
getDatums
  :: forall era
   . IsEra era
  => TxInsReference (LedgerEra era)
  -- ^ reference inputs
  -> [TxOut CtxTx (LedgerEra era)]
  -> L.TxDats (LedgerEra era)
getDatums txInsRef txOutsFromTx = do
  let TxInsReference _ datumSet = txInsRef
      refInDatums = mapMaybe extractDatumsAndHashes $ Set.toList datumSet
      -- use only supplemental datum
      txOutsDats =
        [(h, d) | TxOut _ (Just (TxOutSupplementalDatum h d)) <- txOutsFromTx]
          :: [(L.DataHash, L.Data (LedgerEra era))]
  obtainCommonConstraints (useEra @era) $
    L.TxDats $
      fromList $
        refInDatums <> txOutsDats
