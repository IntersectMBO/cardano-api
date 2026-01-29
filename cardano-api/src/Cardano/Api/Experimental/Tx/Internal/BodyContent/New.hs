{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Experimental.Tx.Internal.BodyContent.New
  ( TxCertificates (..)
  , TxReturnCollateral (..)
  , TxTotalCollateral (..)
  , TxExtraKeyWitnesses (..)
  , TxInsReference (..)
  , TxMintValue (..)
  , TxOut (..)
  , TxProposalProcedures (..)
  , TxValidityLowerBound (..)
  , TxVotingProcedures (..)
  , TxWithdrawals (..)
  , TxBodyContent (..)
  , Datum (..)
  , defaultTxBodyContent
  , extractDatumsAndHashes
  , getDatums
  , collectTxBodyScriptWitnessRequirements
  , makeUnsignedTx
  , extractAllIndexedPlutusScriptWitnesses
  , txMintValueToValue
  , mkTxCertificates
  , mkTxVotingProcedures
  , mkTxProposalProcedures

    -- * Getters and Setters
  , modTxOuts
  , setTxAuxScripts
  , setTxCertificates
  , setTxReturnCollateral
  , setTxTotalCollateral
  , setTxCurrentTreasuryValue
  , setTxExtraKeyWits
  , setTxFee
  , setTxIns
  , setTxInsCollateral
  , setTxInsReference
  , setTxMetadata
  , setTxMintValue
  , setTxOuts
  , setTxProposalProcedures
  , setTxProtocolParams
  , setTxScriptValidity
  , setTxTreasuryDonation
  , setTxValidityLowerBound
  , setTxValidityUpperBound
  , setTxVotingProcedures
  , setTxWithdrawals

    -- * Internal conversions
  , convProposalProcedures

    -- * Legacy conversions
  , DatumDecodingError (..)
  , legacyDatumToDatum
  , fromLegacyTxOut
  )
where

import Cardano.Api.Address
import Cardano.Api.Error
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Certificate qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus
  ( AnyIndexedPlutusScriptWitness (..)
  , Witnessable (..)
  , WitnessableItem (..)
  , createIndexedPlutusScriptWitnesses
  )
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
  ( AnyWitness (..)
  , anyScriptWitnessToAnyWitness
  )
import Cardano.Api.Experimental.Tx.Internal.Certificate.Compatible (getTxCertWitness)
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
  ( TxScriptWitnessRequirements (..)
  , getTxScriptWitnessesRequirements
  )
import Cardano.Api.Experimental.Tx.Internal.Type
import Cardano.Api.Governance.Internal.Action.VotingProcedure
  ( VotesMergingConflict (..)
  , mergeVotingProcedures
  )
import Cardano.Api.Key.Internal
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.ScriptData qualified as Api
import Cardano.Api.Pretty
import Cardano.Api.Tx.Internal.Body
  ( CtxTx
  , TxIn
  , toShelleyTxIn
  , toShelleyWithdrawal
  )
import Cardano.Api.Tx.Internal.Output qualified as OldApi
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal (PolicyAssets, PolicyId, Value, policyAssetsToValue, toMaryValue)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad
import Data.ByteString.Short qualified as SBS
import Data.Functor
import Data.List qualified as List
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
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

makeUnsignedTx
  :: forall era
   . Era era
  -> TxBodyContent (LedgerEra era)
  -> UnsignedTx (LedgerEra era)
makeUnsignedTx DijkstraEra _ = error "makeUnsignedTx: Dijkstra era not supported yet"
makeUnsignedTx era@ConwayEra bc = obtainCommonConstraints era $ do
  let TxScriptWitnessRequirements languages scripts datums redeemers = collectTxBodyScriptWitnessRequirements bc

  -- cardano-api types
  let apiMintValue = txMintValue bc
      apiReferenceInputs = txInsReference bc
      apiExtraKeyWitnesses = txExtraKeyWits bc

      -- Ledger types
      txins = convTxIns $ txIns bc
      collTxIns = convCollateralTxIns bc
      refTxIns = convReferenceInputs apiReferenceInputs
      outs = fromList [o | TxOut o <- txOuts bc]
      protocolParameters = txProtocolParams bc
      fee = txFee bc
      withdrawals = convWithdrawals $ txWithdrawals bc
      certs = convCertificates $ txCertificates bc
      retCollateral = unTxReturnCollateral <$> txReturnCollateral bc
      totCollateral = unTxTotalCollateral <$> txTotalCollateral bc
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
  UnsignedTx $
    L.mkBasicTx eraSpecificTxBody
      & L.witsTxL .~ scriptWitnesses
      & L.auxDataTxL .~ L.maybeToStrictMaybe (toAuxiliaryData (txMetadata bc) (txAuxScripts bc))
      & L.isValidTxL .~ scriptValidity

convTxIns :: [(TxIn, AnyWitness era)] -> Set L.TxIn
convTxIns inputs =
  Set.fromList [toShelleyTxIn txin | (txin, _) <- inputs]

convCollateralTxIns :: TxBodyContent (LedgerEra era) -> Set L.TxIn
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
          let ss = [L.NativeScript s | SimpleScript s <- ss']
           in guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
        DijkstraEra ->
          let ss = [L.NativeScript s | SimpleScript s <- ss']
           in guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss

eraSpecificLedgerTxBody
  :: Era era
  -> L.TxBody (LedgerEra era)
  -> TxBodyContent (LedgerEra era)
  -> L.TxBody (LedgerEra era)
eraSpecificLedgerTxBody era ledgerbody bc =
  body era
 where
  body e =
    let propProcedures = txProposalProcedures bc
        voteProcedures = txVotingProcedures bc
        treasuryDonation = txTreasuryDonation bc
        currentTreasuryValue = txCurrentTreasuryValue bc
     in obtainCommonConstraints e $
          ledgerbody
            & L.proposalProceduresTxBodyL
              .~ convProposalProcedures propProcedures
            & L.votingProceduresTxBodyL
              .~ convVotingProcedures voteProcedures
            & L.treasuryDonationTxBodyL
              .~ fromMaybe (L.Coin 0) treasuryDonation
            & L.currentTreasuryValueTxBodyL
              .~ L.maybeToStrictMaybe currentTreasuryValue

data TxOut era where
  TxOut :: L.EraTxOut era => L.TxOut era -> TxOut era

deriving instance (Show (TxOut era))

deriving instance (Eq (TxOut era))

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

hashableScriptDatumToDatumAndHash :: L.Era era => Api.HashableScriptData -> (L.DataHash, L.Data era)
hashableScriptDatumToDatumAndHash sd =
  (Api.unScriptDataHash $ Api.hashScriptDataBytes sd, Api.toAlonzoData sd)

legacyDatumToDatum
  :: forall era. IsEra era => OldApi.TxOutDatum CtxTx era -> Maybe (Datum CtxTx (LedgerEra era))
legacyDatumToDatum (OldApi.TxOutDatumHash _ h) = Just (TxOutDatumHash $ Api.unScriptDataHash h)
legacyDatumToDatum (OldApi.TxOutSupplementalDatum _ hd) = do
  let (hash, d) = obtainCommonConstraints (useEra @era) $ hashableScriptDatumToDatumAndHash hd
  Just (TxOutSupplementalDatum hash d)
legacyDatumToDatum (OldApi.TxOutDatumInline _ hd) = do
  let (hash, d) = obtainCommonConstraints (useEra @era) $ hashableScriptDatumToDatumAndHash hd
  Just (TxOutDatumInline hash d)
legacyDatumToDatum OldApi.TxOutDatumNone = Nothing

fromLegacyTxOut
  :: forall era. IsEra era => OldApi.TxOut CtxTx era -> Either DatumDecodingError (TxOut (LedgerEra era))
fromLegacyTxOut tOut@(OldApi.TxOut _ _ d _) = do
  let o = OldApi.toShelleyTxOutAny (convert $ useEra @era) tOut
  newDatum :: L.Datum (LedgerEra era) <- obtainCommonConstraints (useEra @era) $ toLedgerDatum d
  return $ obtainCommonConstraints (useEra @era) $ TxOut $ o & L.datumTxOutL .~ newDatum

newtype DatumDecodingError = DataDecodingError String
  deriving (Show, Eq)

instance Error DatumDecodingError where
  prettyError (DataDecodingError msg) = "Datum decoding error: " <> pshow msg

toLedgerDatum
  :: L.Era (LedgerEra era)
  => OldApi.TxOutDatum CtxTx era -> Either DatumDecodingError (L.Datum (LedgerEra era))
toLedgerDatum OldApi.TxOutDatumNone = Right L.NoDatum
toLedgerDatum (OldApi.TxOutDatumHash _ (Api.ScriptDataHash h)) = Right $ L.DatumHash h
toLedgerDatum (OldApi.TxOutSupplementalDatum _ h) =
  case L.makeBinaryData $ SBS.toShort $ Api.getOriginalScriptDataBytes h of
    Left e -> Left $ DataDecodingError e
    Right bd -> Right $ L.Datum bd
toLedgerDatum (OldApi.TxOutDatumInline _ h) =
  case L.makeBinaryData $ SBS.toShort $ Api.getOriginalScriptDataBytes h of
    Left e -> Left $ DataDecodingError e
    Right bd -> Right $ L.Datum bd

data TxInsReference era = TxInsReference [TxIn] (Set (Datum CtxTx era))

newtype TxTotalCollateral = TxTotalCollateral {unTxTotalCollateral :: L.Coin}

newtype TxReturnCollateral era = TxReturnCollateral {unTxReturnCollateral :: L.TxOut era}

newtype TxValidityLowerBound = TxValidityLowerBound L.SlotNo

newtype TxExtraKeyWitnesses = TxExtraKeyWitnesses [Hash PaymentKey]

newtype TxWithdrawals era = TxWithdrawals {unTxWithdrawals :: [(StakeAddress, L.Coin, AnyWitness era)]}
  deriving (Eq, Show)

newtype TxCertificates era
  = TxCertificates
  {unTxCertificates :: OMap (Exp.Certificate era) (Maybe (StakeCredential, AnyWitness era))}
  deriving (Show, Eq)

-- | Create 'TxCertificates'. Note that 'Certificate era' will be deduplicated. Only Certificates with a
-- stake credential will be in the result.
--
-- Note that, when building a transaction in Conway era, a witness is not required for staking credential
-- registration, but this is only the case during the transitional period of Conway era and only for staking
-- credential registration certificates without a deposit. Future eras will require a witness for
-- registration certificates, because the one without a deposit will be removed.
mkTxCertificates
  :: forall era
   . Era era
  -> [(Exp.Certificate (LedgerEra era), AnyWitness (LedgerEra era))]
  -> TxCertificates (LedgerEra era)
mkTxCertificates era certs = TxCertificates . OMap.fromList $ map getStakeCred certs
 where
  getStakeCred
    :: (Exp.Certificate (LedgerEra era), AnyWitness (LedgerEra era))
    -> ( Exp.Certificate (LedgerEra era)
       , Maybe (StakeCredential, AnyWitness (LedgerEra era))
       )
  getStakeCred (c@(Exp.Certificate cert), wit) =
    (c, (,wit) <$> getTxCertWitness (convert era) (obtainCommonConstraints era cert))

newtype TxMintValue era
  = TxMintValue
  { unTxMintValue
      :: Map
           PolicyId
           ( PolicyAssets
           , AnyScriptWitness era
           )
  }
  deriving (Eq, Show)

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
          (AnyWitness era)
      )
  deriving (Show, Eq)

-- | A smart constructor for 'TxProposalProcedures'. It makes sure that the value produced is consistent - the
-- witnessed proposals are also present in the first constructor parameter.
mkTxProposalProcedures
  :: forall era
   . IsEra era
  => [(L.ProposalProcedure (LedgerEra era), AnyWitness (LedgerEra era))]
  -> TxProposalProcedures (LedgerEra era)
mkTxProposalProcedures proposals = do
  TxProposalProcedures $
    obtainCommonConstraints (useEra @era) $
      OMap.fromList proposals

data TxVotingProcedures era
  = TxVotingProcedures
      (L.VotingProcedures era)
      (Map L.Voter (AnyWitness era))
  deriving (Eq, Show)

-- | Create voting procedures from map of voting procedures and optional witnesses.
-- Validates the function argument, to make sure the list of votes is legal.
-- See 'mergeVotingProcedures' for validation rules.
mkTxVotingProcedures
  :: forall era
   . [(L.VotingProcedures era, AnyWitness era)]
  -> Either (VotesMergingConflict era) (TxVotingProcedures era)
mkTxVotingProcedures votingProcedures = do
  procedure <-
    foldM f (L.VotingProcedures Map.empty) votingProcedures
  pure $ TxVotingProcedures procedure votingScriptWitnessMap
 where
  votingScriptWitnessMap :: Map L.Voter (AnyWitness era)
  votingScriptWitnessMap =
    foldl
      (\acc next -> acc `Map.union` uncurry votingScriptWitnessSingleton next)
      Map.empty
      votingProcedures

  f
    :: L.VotingProcedures era
    -> (L.VotingProcedures era, AnyWitness era)
    -> Either (VotesMergingConflict era) (L.VotingProcedures era)
  f acc (procedure, _witness) = mergeVotingProcedures acc procedure

  votingScriptWitnessSingleton
    :: L.VotingProcedures era
    -> AnyWitness era
    -> Map L.Voter (AnyWitness era)
  votingScriptWitnessSingleton votingProcedures' scriptWitness = do
    let voter = fromJust $ getVotingScriptCredentials votingProcedures'
    Map.singleton voter scriptWitness

  getVotingScriptCredentials
    :: L.VotingProcedures era
    -> Maybe L.Voter
  getVotingScriptCredentials (L.VotingProcedures m) =
    listToMaybe $ Map.keys m

data TxBodyContent era
  = TxBodyContent
  { txIns :: [(TxIn, AnyWitness era)]
  , txInsCollateral :: [TxIn]
  , txInsReference :: TxInsReference era
  , txOuts :: [TxOut era]
  , txTotalCollateral :: Maybe TxTotalCollateral
  , txReturnCollateral :: Maybe (TxReturnCollateral era)
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
  -- ^ Current treasury value
  , txTreasuryDonation :: Maybe L.Coin
  -- ^ Treasury donation to perform
  }

defaultTxBodyContent
  :: TxBodyContent era
defaultTxBodyContent =
  TxBodyContent
    { txIns = []
    , txInsCollateral = []
    , txInsReference = TxInsReference mempty Set.empty
    , txOuts = []
    , txTotalCollateral = Nothing
    , txReturnCollateral = Nothing
    , txFee = 0
    , txValidityLowerBound = Nothing
    , txValidityUpperBound = Nothing
    , txMetadata = TxMetadata mempty
    , txAuxScripts = []
    , txExtraKeyWits = TxExtraKeyWitnesses []
    , txProtocolParams = Nothing
    , txWithdrawals = TxWithdrawals mempty
    , txCertificates = TxCertificates OMap.empty
    , txMintValue = TxMintValue mempty
    , txScriptValidity = ScriptValid
    , txProposalProcedures = Nothing
    , txVotingProcedures = Nothing
    , txCurrentTreasuryValue = Nothing
    , txTreasuryDonation = Nothing
    }

extractAllIndexedPlutusScriptWitnesses
  :: forall era
   . Era era
  -> TxBodyContent (LedgerEra era)
  -> Either
       CBOR.DecoderError
       [AnyIndexedPlutusScriptWitness (LedgerEra era)]
extractAllIndexedPlutusScriptWitnesses era b = obtainCommonConstraints era $ do
  let txInWits = extractWitnessableTxIns $ txIns b
      certWits = extractWitnessableCertificates $ txCertificates b
      mintWits = [(wit, anyScriptWitnessToAnyWitness sw) | (wit, sw) <- extractWitnessableMints $ txMintValue b]
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
  -> [(Witnessable MintItem (LedgerEra era), AnyScriptWitness (LedgerEra era))]
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
    obtainCommonConstraints (useEra @era) (toList txps)

collectTxBodyScriptWitnessRequirements
  :: forall era
   . IsEra era
  => TxBodyContent (LedgerEra era)
  -> TxScriptWitnessRequirements (LedgerEra era)
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

    let txInWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableTxIns txIns
        txWithdrawalWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableWithdrawals txWithdrawals
        txCertWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableCertificates txCertificates
        txMintWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            [(wit, anyScriptWitnessToAnyWitness sw) | (wit, sw) <- extractWitnessableMints txMintValue]
        txVotingWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableVotes txVotingProcedures
        txProposalWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableProposals txProposalProcedures

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

obtainMonoidConstraint
  :: Era era
  -> (Monoid (TxScriptWitnessRequirements (LedgerEra era)) => a)
  -> a
obtainMonoidConstraint eon = case eon of
  ConwayEra -> id
  DijkstraEra -> id

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
  -> [TxOut (LedgerEra era)]
  -> L.TxDats (LedgerEra era)
getDatums txInsRef txOutsFromTx = do
  let TxInsReference _ datumSet = txInsRef
      refInDatums = mapMaybe extractDatumsAndHashes $ Set.toList datumSet
      -- use only supplemental datum
      txOutsDats =
        [ (L.hashData d, d)
        | TxOut txout <- txOutsFromTx
        , d <-
            maybeToList $ L.strictMaybeToMaybe $ txout ^. obtainCommonConstraints (useEra @era) L.dataTxOutL
        ]
          :: [(L.DataHash, L.Data (LedgerEra era))]
  obtainCommonConstraints (useEra @era) $
    L.TxDats $
      fromList $
        refInDatums <> txOutsDats

-- Getters and Setters

setTxAuxScripts :: [SimpleScript era] -> TxBodyContent era -> TxBodyContent era
setTxAuxScripts v txBodyContent = txBodyContent{txAuxScripts = v}

setTxExtraKeyWits :: TxExtraKeyWitnesses -> TxBodyContent era -> TxBodyContent era
setTxExtraKeyWits v txBodyContent = txBodyContent{txExtraKeyWits = v}

setTxIns :: [(TxIn, AnyWitness era)] -> TxBodyContent era -> TxBodyContent era
setTxIns v txBodyContent = txBodyContent{txIns = v}

setTxInsCollateral :: [TxIn] -> TxBodyContent era -> TxBodyContent era
setTxInsCollateral v txBodyContent = txBodyContent{txInsCollateral = v}

setTxInsReference :: TxInsReference era -> TxBodyContent era -> TxBodyContent era
setTxInsReference v txBodyContent = txBodyContent{txInsReference = v}

setTxProtocolParams :: L.PParams era -> TxBodyContent era -> TxBodyContent era
setTxProtocolParams v txBodyContent = txBodyContent{txProtocolParams = Just v}

setTxReturnCollateral :: TxReturnCollateral era -> TxBodyContent era -> TxBodyContent era
setTxReturnCollateral v txBodyContent = txBodyContent{txReturnCollateral = Just v}

setTxTotalCollateral :: TxTotalCollateral -> TxBodyContent era -> TxBodyContent era
setTxTotalCollateral v txBodyContent = txBodyContent{txTotalCollateral = Just v}

setTxValidityLowerBound :: L.SlotNo -> TxBodyContent era -> TxBodyContent era
setTxValidityLowerBound v txBodyContent = txBodyContent{txValidityLowerBound = Just v}

setTxValidityUpperBound :: L.SlotNo -> TxBodyContent era -> TxBodyContent era
setTxValidityUpperBound v txBodyContent = txBodyContent{txValidityUpperBound = Just v}

setTxMetadata :: TxMetadata -> TxBodyContent era -> TxBodyContent era
setTxMetadata v txBodyContent = txBodyContent{txMetadata = v}

setTxFee :: L.Coin -> TxBodyContent era -> TxBodyContent era
setTxFee v txBodyContent = txBodyContent{txFee = v}

setTxOuts :: [TxOut era] -> TxBodyContent era -> TxBodyContent era
setTxOuts v txBodyContent = txBodyContent{txOuts = v}

setTxMintValue :: TxMintValue era -> TxBodyContent era -> TxBodyContent era
setTxMintValue v txBodyContent = txBodyContent{txMintValue = v}

setTxScriptValidity :: ScriptValidity -> TxBodyContent era -> TxBodyContent era
setTxScriptValidity v txBodyContent = txBodyContent{txScriptValidity = v}

setTxCertificates :: TxCertificates era -> TxBodyContent era -> TxBodyContent era
setTxCertificates v txBodyContent = txBodyContent{txCertificates = v}

setTxWithdrawals :: TxWithdrawals era -> TxBodyContent era -> TxBodyContent era
setTxWithdrawals v txBodyContent = txBodyContent{txWithdrawals = v}

setTxVotingProcedures :: TxVotingProcedures era -> TxBodyContent era -> TxBodyContent era
setTxVotingProcedures v txBodyContent = txBodyContent{txVotingProcedures = Just v}

setTxProposalProcedures :: TxProposalProcedures era -> TxBodyContent era -> TxBodyContent era
setTxProposalProcedures v txBodyContent = txBodyContent{txProposalProcedures = Just v}

setTxCurrentTreasuryValue :: L.Coin -> TxBodyContent era -> TxBodyContent era
setTxCurrentTreasuryValue v txBodyContent = txBodyContent{txCurrentTreasuryValue = Just v}

setTxTreasuryDonation :: L.Coin -> TxBodyContent era -> TxBodyContent era
setTxTreasuryDonation v txBodyContent = txBodyContent{txTreasuryDonation = Just v}

modTxOuts
  :: ([TxOut era] -> [TxOut era]) -> TxBodyContent era -> TxBodyContent era
modTxOuts f txBodyContent = txBodyContent{txOuts = f (txOuts txBodyContent)}
