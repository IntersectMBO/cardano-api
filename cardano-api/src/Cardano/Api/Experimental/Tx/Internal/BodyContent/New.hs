{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  , MakeUnsignedTxError (..)
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
  , setTxSupplementalDatums
  , setTxTreasuryDonation
  , setTxValidityLowerBound
  , setTxValidityUpperBound
  , setTxVotingProcedures
  , setTxWithdrawals

    -- * Internal conversions
  , convProposalProcedures
  , extractWitnessableTxIns
  , extractWitnessableMints
  , extractWitnessableCertificates
  , extractWitnessableWithdrawals
  , extractWitnessableVotes
  , extractWitnessableProposals
  )
where

import Cardano.Api.Address
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
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
  ( VotingError (..)
  , mergeVotingProcedures
  )
import Cardano.Api.Key.Internal
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Monad.Error (failEitherWith, liftMaybe)
import Cardano.Api.Plutus.Internal.Script
  ( PlutusScript (..)
  , PlutusScriptVersion (..)
  , ScriptInAnyLang (..)
  , ScriptLanguage (..)
  , fromAllegraTimelock
  , toAllegraTimelock
  )
import Cardano.Api.Plutus.Internal.Script qualified as OldScript
import Cardano.Api.Plutus.Internal.ScriptData qualified as Api
import Cardano.Api.Serialise.Cbor (serialiseToCBOR)
import Cardano.Api.Tx.Internal.Body
  ( CtxTx
  , TxIn
  , asGuard
  , toShelleyTxIn
  , toShelleyWithdrawal
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal
  ( PolicyAssets
  , PolicyId
  , Value
  , fromLedgerValue
  , policyAssetsToValue
  , toMaryValue
  )

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Plutus.Language (PlutusBinary (..), plutusLanguage)
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Base16 qualified as Base16
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
import Data.Text.Encoding qualified as Text
import GHC.Exts (IsList (..))
import Lens.Micro

-- | Error that can occur when constructing an unsigned transaction.
data MakeUnsignedTxError
  = -- | Plutus scripts are present in the transaction but no protocol
    -- parameters were provided. Protocol parameters are required to
    -- compute the script integrity hash (script_data_hash).
    MakeUnsignedTxMissingProtocolParams
  deriving (Eq, Show)

instance Error MakeUnsignedTxError where
  prettyError MakeUnsignedTxMissingProtocolParams =
    mconcat
      [ "Transaction uses Plutus scripts but no protocol parameters were provided. "
      , "Protocol parameters are required to compute the script integrity hash "
      , "(script_data_hash) from the cost models."
      ]

makeUnsignedTx
  :: forall era
   . Era era
  -> TxBodyContent (LedgerEra era)
  -> Either MakeUnsignedTxError (UnsignedTx (LedgerEra era))
makeUnsignedTx DijkstraEra _ = error "TODO Dijkstra: makeUnsignedTx: era not supported"
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

  scriptIntegrityHash <-
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
          & L.vldtTxBodyL . L.invalidBeforeL .~ L.maybeToStrictMaybe (txValidityLowerBound bc)
          & L.vldtTxBodyL . L.invalidHereAfterL .~ L.maybeToStrictMaybe (txValidityUpperBound bc)
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
  Right $
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
  :: TxExtraKeyWitnesses -> Set (L.KeyHash L.Guard)
convExtraKeyWitnesses (TxExtraKeyWitnesses khs) =
  fromList
    [ asGuard kh
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
  -> Either MakeUnsignedTxError (StrictMaybe L.ScriptIntegrityHash)
convPParamsToScriptIntegrityHash mTxProtocolParams redeemers datums languages = obtainCommonConstraints (useEra @era) $ do
  -- This logic is copied from ledger, because their code is not reusable
  -- c.f. https://github.com/IntersectMBO/cardano-ledger/commit/5a975d9af507c9ee835a86d3bb77f3e2670ad228#diff-8236dfec9688f22550b91fc9a87af9915523ab9c5bd817218ecceec8ca7a789bR282
  let shouldCalculateHash =
        not $
          null (redeemers ^. L.unRedeemersL)
            && null (datums ^. L.unTxDatsL)
            && null languages
  if shouldCalculateHash
    then do
      pp <- liftMaybe MakeUnsignedTxMissingProtocolParams mTxProtocolParams
      pure $
        SJust $
          L.hashScriptIntegrity $
            L.ScriptIntegrity redeemers datums (Set.map (L.getLanguageView pp) languages)
    else pure SNothing

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
  -> L.TxBody L.TopTx (LedgerEra era)
  -> TxBodyContent (LedgerEra era)
  -> L.TxBody L.TopTx (LedgerEra era)
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

instance ToJSON (TxOut L.ShelleyEra) where toJSON = txOutToJson ShelleyBasedEraShelley

instance ToJSON (TxOut L.AllegraEra) where toJSON = txOutToJson ShelleyBasedEraAllegra

instance ToJSON (TxOut L.MaryEra) where toJSON = txOutToJson ShelleyBasedEraMary

-- | Note: Unlike the legacy API's @TxOut@, this instance does not render
-- supplemental datums. At the ledger level, a supplemental datum is not
-- stored in the @TxOut@ — only its hash is. The full datum lives in the
-- transaction witness set (@TxDats@). The legacy API bundled the full
-- datum into @TxOut@ for convenience, but since this type wraps the
-- ledger's @TxOut@ directly, supplemental datums are indistinguishable
-- from hash-only datums here.
instance ToJSON (TxOut L.AlonzoEra) where toJSON = txOutToJson ShelleyBasedEraAlonzo

instance ToJSON (TxOut L.BabbageEra) where toJSON = txOutToJson ShelleyBasedEraBabbage

instance ToJSON (TxOut L.ConwayEra) where toJSON = txOutToJson ShelleyBasedEraConway

txOutToJson :: ShelleyBasedEra era -> TxOut (ShelleyLedgerEra era) -> Aeson.Value
txOutToJson sbe (TxOut o) =
  Aeson.object $
    txOutBaseJsonFields sbe o <> alonzoOnwardsFields
 where
  alonzoOnwardsFields = case sbe of
    ShelleyBasedEraShelley -> []
    ShelleyBasedEraAllegra -> []
    ShelleyBasedEraMary -> []
    ShelleyBasedEraAlonzo -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)
    ShelleyBasedEraBabbage -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)
    ShelleyBasedEraConway -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)
    ShelleyBasedEraDijkstra -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)

-- | Emit the datum, inline-datum, and reference-script JSON fields appropriate
-- for the era. Pre-Alonzo emits nothing; Alonzo emits @datumhash@ and @datum@;
-- Babbage+ additionally emits @inlineDatum@, @inlineDatumRaw@, @inlineDatumhash@
-- and @referenceScript@.
datumAndRefScriptFields
  :: L.AlonzoEraScript era
  => Maybe (L.Datum era)
  -> Maybe (Maybe (L.Script era))
  -> [Pair]
datumAndRefScriptFields mDatum mRefScript =
  datumFields <> inlineDatumFields <> refScriptFields
 where
  isBabbagePlus = isJust mRefScript

  datumFields = case mDatum of
    Nothing -> []
    Just L.NoDatum -> ["datumhash" .= Aeson.Null, "datum" .= Aeson.Null]
    Just (L.DatumHash dh) -> ["datumhash" .= dh, "datum" .= Aeson.Null]
    Just (L.Datum _) -> ["datum" .= Aeson.Null]

  inlineDatumFields = case mDatum of
    Just (L.Datum bd) ->
      let hsd = Api.fromAlonzoData (L.binaryDataToData bd)
       in [ "inlineDatumhash" .= L.hashBinaryData bd
          , "inlineDatum" .= Api.scriptDataToJsonDetailedSchema hsd
          , "inlineDatumRaw"
              .= (Aeson.String . Text.decodeUtf8 . Base16.encode . serialiseToCBOR $ hsd)
          ]
    _
      | isBabbagePlus -> ["inlineDatum" .= Aeson.Null, "inlineDatumRaw" .= Aeson.Null]
      | otherwise -> []

  refScriptFields = case mRefScript of
    Nothing -> []
    Just Nothing -> ["referenceScript" .= Aeson.Null]
    Just (Just script) -> ["referenceScript" .= ledgerScriptToScriptInAnyLang script]

-- | Render just the base fields (address and value) shared by all eras.
txOutBaseJsonFields
  :: L.EraTxOut (ShelleyLedgerEra era) => ShelleyBasedEra era -> L.TxOut (ShelleyLedgerEra era) -> [Pair]
txOutBaseJsonFields sbe o =
  [ "address" .= addrToJson (o ^. L.addrTxOutL)
  , "value" .= fromLedgerValue sbe (o ^. L.valueTxOutL)
  ]

-- | Convert a ledger 'L.Addr' to JSON using the same format as the legacy API
-- (bech32 for Shelley addresses, base58 for Byron addresses).
addrToJson :: L.Addr -> Aeson.Value
addrToJson (L.Addr nw pc scr) = toJSON (ShelleyAddress nw pc scr)
addrToJson (L.AddrBootstrap (L.BootstrapAddress addr)) = toJSON (ByronAddress addr)

-- | Convert a ledger 'Script' to a cardano-api 'ScriptInAnyLang' without
-- per-era pattern matching, using 'AlonzoEraScript' methods.
ledgerScriptToScriptInAnyLang
  :: L.AlonzoEraScript era => L.Script era -> ScriptInAnyLang
ledgerScriptToScriptInAnyLang script =
  case L.getNativeScript script of
    Just ns ->
      ScriptInAnyLang SimpleScriptLanguage (OldScript.SimpleScript (fromAllegraTimelock ns))
    Nothing ->
      case L.toPlutusScript script of
        Just ps -> L.withPlutusScript ps $ \plutus ->
          let sbs = unPlutusBinary (L.plutusBinary plutus)
           in case plutusLanguage plutus of
                Plutus.PlutusV1 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) $
                    OldScript.PlutusScript PlutusScriptV1 (PlutusScriptSerialised sbs)
                Plutus.PlutusV2 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) $
                    OldScript.PlutusScript PlutusScriptV2 (PlutusScriptSerialised sbs)
                Plutus.PlutusV3 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) $
                    OldScript.PlutusScript PlutusScriptV3 (PlutusScriptSerialised sbs)
                Plutus.PlutusV4 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV4) $
                    OldScript.PlutusScript PlutusScriptV4 (PlutusScriptSerialised sbs)
        Nothing -> error "ledgerScriptToScriptInAnyLang: script is neither native nor Plutus"

-- | Convert a 'ScriptInAnyLang' to a ledger 'L.Script'. Reverse of 'ledgerScriptToScriptInAnyLang'.
scriptInAnyLangToLedgerScript
  :: forall era
   . ( L.AlonzoEraScript era
     , L.NativeScript era ~ Timelock era
     )
  => ScriptInAnyLang -> Parser (L.Script era)
scriptInAnyLangToLedgerScript (ScriptInAnyLang lang script) =
  case (lang, script) of
    (SimpleScriptLanguage, OldScript.SimpleScript ss) ->
      pure $ Ledger.fromNativeScript (toAllegraTimelock ss)
    (PlutusScriptLanguage PlutusScriptV1, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV1)
    (PlutusScriptLanguage PlutusScriptV2, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV2)
    (PlutusScriptLanguage PlutusScriptV3, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV3)
    (PlutusScriptLanguage PlutusScriptV4, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV4)

deriving instance (Show (TxOut era))

deriving instance (Eq (TxOut era))

-- | Pre-Alonzo eras have no datums or reference scripts, so parsing
instance FromJSON (TxOut L.ShelleyEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraShelley)

instance FromJSON (TxOut L.AllegraEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraAllegra)

instance FromJSON (TxOut L.MaryEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraMary)

instance FromJSON (TxOut L.AlonzoEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraAlonzo)

instance FromJSON (TxOut L.BabbageEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraBabbage)

instance FromJSON (TxOut L.ConwayEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraConway)

txOutParseJson
  :: ShelleyBasedEra era -> Aeson.Object -> Parser (TxOut (ShelleyLedgerEra era))
txOutParseJson sbe o = do
  addr <- addrFromJson =<< o .: "address"
  apiVal <- parseJSON =<< o .: "value"
  let mv = toMaryValue apiVal
  case sbe of
    ShelleyBasedEraShelley -> do
      let L.MaryValue _ ma = mv
      unless (ma == mempty) $
        fail "txOutParseJson: ada-only era output cannot carry a multi-asset value"
      pure . TxOut $ L.mkBasicTxOut addr (L.coin mv)
    ShelleyBasedEraAllegra -> do
      let L.MaryValue _ ma = mv
      unless (ma == mempty) $
        fail "txOutParseJson: ada-only era output cannot carry a multi-asset value"
      pure . TxOut $ L.mkBasicTxOut addr (L.coin mv)
    ShelleyBasedEraMary -> pure . TxOut $ L.mkBasicTxOut addr mv
    ShelleyBasedEraAlonzo -> do
      let base = L.mkBasicTxOut addr mv
      mDatumHash <- o .:? "datumhash"
      pure . TxOut $ case mDatumHash of
        Nothing -> base
        Just dh -> base & L.dataHashTxOutL .~ SJust dh
    ShelleyBasedEraBabbage ->
      babbageOnwardsTxOutParseJson (L.mkBasicTxOut addr mv) o
    ShelleyBasedEraConway ->
      babbageOnwardsTxOutParseJson (L.mkBasicTxOut addr mv) o
    ShelleyBasedEraDijkstra -> error "TODO Dijkstra: txOutParseJson: era not supported"

-- | Parse a ledger 'L.Addr' from JSON. Reverse of 'addrToJson'.
addrFromJson :: Aeson.Value -> Parser L.Addr
addrFromJson = Aeson.withText "Address" $ \txt ->
  case deserialiseAddress AsAddressAny txt of
    Nothing -> fail $ "addrFromJson: invalid address: " <> show txt
    Just addrAny -> pure $ case addrAny of
      AddressByron (ByronAddress addr) -> L.AddrBootstrap (L.BootstrapAddress addr)
      AddressShelley (ShelleyAddress nw pc scr) -> L.Addr nw pc scr

-- | Parse a Babbage+ TxOut with datum and reference script support.
babbageOnwardsTxOutParseJson
  :: forall era
   . ( L.BabbageEraTxOut era
     , L.NativeScript era ~ Timelock era
     )
  => L.TxOut era -> Aeson.Object -> Parser (TxOut era)
babbageOnwardsTxOutParseJson baseTxOut o = do
  -- Parse datum fields
  mDatumHash <- o .:? "datumhash"
  mInlineDatumRaw <- o .:? "inlineDatumRaw"
  mInlineDatumHash <- o .:? "inlineDatumhash"
  -- Parse reference script
  mRefScript <- o .:? "referenceScript"
  -- Determine datum
  datum <- case mInlineDatumRaw of
    Just rawHex -> do
      expectedHash <-
        maybe
          (fail "babbageOnwardsTxOutParseJson: inlineDatumRaw present without inlineDatumhash")
          pure
          mInlineDatumHash
      rawBytes <-
        failEitherWith
          (("babbageOnwardsTxOutParseJson: failed to hex-decode inlineDatumRaw: " <>) . show)
          $ Base16.decode (Text.encodeUtf8 rawHex)
      binaryData <-
        failEitherWith
          ("babbageOnwardsTxOutParseJson: failed to CBOR-decode inlineDatumRaw: " <>)
          $ L.makeBinaryData (SBS.toShort rawBytes)
      when (L.hashBinaryData binaryData /= expectedHash) $
        fail $
          mconcat
            [ "babbageOnwardsTxOutParseJson: inline datum hash mismatch: "
            , "expected "
            , show expectedHash
            , ", got "
            , show (L.hashBinaryData binaryData)
            ]
      pure $ L.Datum binaryData
    Nothing -> do
      when (isJust mInlineDatumHash) $
        fail "babbageOnwardsTxOutParseJson: inlineDatumhash present without inlineDatumRaw"
      pure $ maybe L.NoDatum L.DatumHash mDatumHash
  -- Determine reference script
  refScript <- L.maybeToStrictMaybe <$> forM mRefScript scriptInAnyLangToLedgerScript
  -- Construct TxOut
  pure . TxOut $
    baseTxOut
      & L.datumTxOutL .~ datum
      & L.referenceScriptTxOutL .~ refScript

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
  -> Either (VotingError era) (TxVotingProcedures era)
mkTxVotingProcedures votingProcedures = do
  procedure <-
    foldM f (L.VotingProcedures Map.empty) votingProcedures
  votingScriptWitnessMap <-
    foldM
      (\acc next -> Map.union acc <$> uncurry votingScriptWitnessSingleton next)
      Map.empty
      votingProcedures
  pure $ TxVotingProcedures procedure votingScriptWitnessMap
 where
  f
    :: L.VotingProcedures era
    -> (L.VotingProcedures era, AnyWitness era)
    -> Either (VotingError era) (L.VotingProcedures era)
  f acc (procedure, _witness) = mergeVotingProcedures acc procedure

  votingScriptWitnessSingleton
    :: L.VotingProcedures era
    -> AnyWitness era
    -> Either (VotingError era) (Map L.Voter (AnyWitness era))
  votingScriptWitnessSingleton lVotingProcedures scriptWitness =
    case fst <$> Map.lookupMin (L.unVotingProcedures lVotingProcedures) of
      Nothing -> Left $ VotingScriptWitnessWithoutVoter lVotingProcedures
      Just voter -> Right $ Map.singleton voter scriptWitness

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
  , txSupplementalDatums :: Map L.DataHash (L.Data era)
  -- ^ Supplemental datums are datums whose hashes correspond to output datum hashes.
  -- They are included in the transaction witness set for communication purposes only.
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
    , txSupplementalDatums = mempty
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
    DijkstraEra -> error "TODO Dijkstra: extractWitnessableVotes: era not supported"
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
    , txCertificates
    , txMintValue
    , txWithdrawals
    , txVotingProcedures
    , txProposalProcedures
    , txSupplementalDatums
    } = obtainCommonConstraints (useEra @era) $ do
    let supplementaldatums =
          TxScriptWitnessRequirements
            mempty
            mempty
            (getDatums txInsReference txSupplementalDatums)
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

-- | Collect datums for the transaction witness set ('TxDats'):
-- 1. supplemental datums provided explicitly
-- 2. datums from reference inputs
--
-- Supplemental datums are datums whose hashes correspond to datum hashes
-- in transaction outputs. They are included for communication purposes only
-- (the Alonzo ledger spec uses subset equality for these).
--
-- Note that this function does not check whose datum hashes are present in the reference inputs. This means if there
-- are redundant datums in 'TxInsReference', a submission of such transaction will fail.
getDatums
  :: forall era
   . IsEra era
  => TxInsReference (LedgerEra era)
  -- ^ reference inputs
  -> Map L.DataHash (L.Data (LedgerEra era))
  -- ^ supplemental datums
  -> L.TxDats (LedgerEra era)
getDatums txInsRef supplementalDats = do
  let TxInsReference _ datumSet = txInsRef
      refInDatums = mapMaybe extractDatumsAndHashes $ Set.toList datumSet
  obtainCommonConstraints (useEra @era) $
    L.TxDats $
      fromList refInDatums <> supplementalDats

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

setTxSupplementalDatums :: Map L.DataHash (L.Data era) -> TxBodyContent era -> TxBodyContent era
setTxSupplementalDatums v txBodyContent = txBodyContent{txSupplementalDatums = v}

modTxOuts
  :: ([TxOut era] -> [TxOut era]) -> TxBodyContent era -> TxBodyContent era
modTxOuts f txBodyContent = txBodyContent{txOuts = f (txOuts txBodyContent)}
