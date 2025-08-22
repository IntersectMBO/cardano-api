{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Cardano.Api.Internal.Orphans.Serialisation
  ( AsType
        ( AsColdCommitteeCredential
        , AsHotCommitteeCredential
        , AsDrepCredential
        , AsGovActionId
        )
  )
where

import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Orphans.Misc
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Monad.Error (MonadError (..), (?!))
import Cardano.Api.Pretty (Pretty (..), prettyException, (<+>))
import Cardano.Api.Pretty.Internal.ShowOf
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx.Internal.TxIn

import Cardano.Binary (DecoderError (..))
import Cardano.Binary qualified as CBOR
import Cardano.Chain.Byron.API qualified as L
import Cardano.Chain.Common qualified as L
import Cardano.Chain.Delegation.Validation.Scheduling qualified as L.Scheduling
import Cardano.Chain.UTxO.UTxO qualified as L
import Cardano.Chain.UTxO.Validation qualified as L
import Cardano.Chain.Update qualified as L
import Cardano.Chain.Update.Validation.Endorsement qualified as L.Endorsement
import Cardano.Chain.Update.Validation.Interface qualified as L.Interface
import Cardano.Chain.Update.Validation.Registration qualified as L.Registration
import Cardano.Chain.Update.Validation.Voting qualified as L.Voting
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Allegra.Rules qualified as L
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Alonzo.Rules qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.PParams qualified as Ledger
import Cardano.Ledger.Babbage.Rules qualified as L
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Conway.Rules qualified as L
import Cardano.Ledger.Conway.TxCert qualified as L
import Cardano.Ledger.Core qualified as L hiding (KeyHash)
import Cardano.Ledger.HKD (NoUpdate (..))
import Cardano.Ledger.Hashes qualified as L hiding (KeyHash)
import Cardano.Ledger.Keys qualified as L.Keys
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Shelley.API.Mempool qualified as L
import Cardano.Ledger.Shelley.PParams qualified as Ledger
import Cardano.Ledger.Shelley.Rules qualified as L
import Cardano.Ledger.Shelley.TxBody qualified as L
import Cardano.Ledger.Shelley.TxCert qualified as L
import Cardano.Protocol.Crypto qualified as P
import Cardano.Protocol.TPraos.API qualified as Ledger
import Cardano.Protocol.TPraos.BHeader (HashHeader (..))
import Cardano.Protocol.TPraos.Rules.Prtcl qualified as L
import Cardano.Protocol.TPraos.Rules.Prtcl qualified as Ledger
import Cardano.Protocol.TPraos.Rules.Tickn qualified as Ledger
import Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Consensus.Protocol.Praos (PraosState)
import Ouroboros.Consensus.Protocol.Praos qualified as Consensus
import Ouroboros.Consensus.Protocol.TPraos (TPraosState)
import Ouroboros.Consensus.Protocol.TPraos qualified as Consensus
import Ouroboros.Consensus.Shelley.Eras qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import Ouroboros.Consensus.Shelley.Ledger.Query qualified as Consensus
import Ouroboros.Network.Block (HeaderHash, Tip (..))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as Net.Tx
import PlutusLedgerApi.Common qualified as P
import PlutusLedgerApi.V2 qualified as V2

import Codec.Binary.Bech32 qualified as Bech32
import Codec.CBOR.Read qualified as CBOR
import Data.Aeson (KeyValue ((.=)), ToJSON (..), ToJSONKey (..), object, pairs)
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.Bits (Bits (..))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Short qualified as SBS
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.ListMap (ListMap)
import Data.ListMap qualified as ListMap
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Exts (IsList (..), IsString (..))
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Lens.Micro
import Network.Mux qualified as Mux
import Numeric (showHex)
import Prettyprinter (punctuate, viaShow)
import Text.Read

deriving instance Generic (L.ApplyTxError era)

deriving instance Generic (L.Registration.TooLarge a)

deriving instance Generic L.ApplicationNameError

deriving instance Generic L.ApplyMempoolPayloadErr

deriving instance Generic L.Endorsement.Error

deriving instance Generic L.Interface.Error

deriving instance Generic L.LovelaceError

deriving instance Generic L.Registration.Adopted

deriving instance Generic L.Registration.Error

deriving instance Generic L.Scheduling.Error

deriving instance Generic L.SoftwareVersionError

deriving instance Generic L.SystemTagError

deriving instance Generic L.TxValidationError

deriving instance Generic L.UTxOError

deriving instance Generic L.UTxOValidationError

deriving instance Generic L.Voting.Error

deriving anyclass instance ToJSON L.ApplicationNameError

deriving anyclass instance ToJSON L.ApplyMempoolPayloadErr

deriving anyclass instance ToJSON L.Endorsement.Error

deriving anyclass instance ToJSON L.Interface.Error

deriving anyclass instance ToJSON L.LovelaceError

deriving anyclass instance ToJSON L.Registration.Adopted

deriving anyclass instance ToJSON L.Registration.ApplicationVersion

deriving anyclass instance ToJSON L.Registration.Error

deriving anyclass instance ToJSON L.Scheduling.Error

deriving anyclass instance ToJSON L.SoftwareVersionError

deriving anyclass instance ToJSON L.SystemTagError

deriving anyclass instance ToJSON L.TxValidationError

deriving anyclass instance ToJSON L.UTxOError

deriving anyclass instance ToJSON L.UTxOValidationError

deriving anyclass instance ToJSON L.Voting.Error

deriving anyclass instance ToJSON L.VotingPeriod

deriving anyclass instance
  ( ToJSON (L.PredicateFailure (L.EraRule "UTXOW" ledgerera))
  , ToJSON (L.PredicateFailure (L.EraRule "DELEGS" ledgerera))
  )
  => ToJSON (L.ShelleyLedgerPredFailure ledgerera)

deriving anyclass instance
  ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  => ToJSON (L.ShelleyUtxowPredFailure ledgerera)

deriving anyclass instance
  ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  => ToJSON (L.ShelleyPpupPredFailure ledgerera)

deriving anyclass instance
  ( ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  , ToJSON (L.PlutusPurpose L.AsItem ledgerera)
  , ToJSON (L.PlutusPurpose L.AsIx ledgerera)
  )
  => ToJSON (L.AlonzoUtxowPredFailure ledgerera)

deriving anyclass instance
  ( ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  , ToJSON (L.TxCert ledgerera)
  , ToJSON (L.PlutusPurpose L.AsItem ledgerera)
  , ToJSON (L.PlutusPurpose L.AsIx ledgerera)
  )
  => ToJSON (L.BabbageUtxowPredFailure ledgerera)

deriving anyclass instance
  ToJSON (L.PredicateFailure (L.EraRule "LEDGER" ledgerera))
  => ToJSON (L.ApplyTxError ledgerera)

deriving via
  ShowOf (L.Keys.VKey L.Keys.Witness)
  instance
    ToJSON (L.Keys.VKey L.Keys.Witness)

deriving via
  ShowOf (L.AllegraUtxoPredFailure ledgerera)
  instance
    Show (L.AllegraUtxoPredFailure ledgerera) => ToJSON (L.AllegraUtxoPredFailure ledgerera)

deriving via
  ShowOf (L.AlonzoUtxoPredFailure ledgerera)
  instance
    Show (L.AlonzoUtxoPredFailure ledgerera) => ToJSON (L.AlonzoUtxoPredFailure ledgerera)

deriving via
  ShowOf (L.BabbageUtxoPredFailure ledgerera)
  instance
    Show (L.BabbageUtxoPredFailure ledgerera) => ToJSON (L.BabbageUtxoPredFailure ledgerera)

deriving via
  ShowOf (L.ConwayLedgerPredFailure ledgerera)
  instance
    Show (L.ConwayLedgerPredFailure ledgerera) => ToJSON (L.ConwayLedgerPredFailure ledgerera)

deriving via
  ShowOf (L.ShelleyDelegsPredFailure ledgerera)
  instance
    Show (L.ShelleyDelegsPredFailure ledgerera) => ToJSON (L.ShelleyDelegsPredFailure ledgerera)

deriving via
  ShowOf (L.ShelleyUtxoPredFailure ledgerera)
  instance
    Show (L.ShelleyUtxoPredFailure ledgerera) => ToJSON (L.ShelleyUtxoPredFailure ledgerera)

deriving instance ToJSON a => ToJSON (L.Registration.TooLarge a)

deriving via ShowOf L.KeyHash instance ToJSON L.KeyHash

deriving via ShowOf L.ApplicationName instance ToJSONKey L.ApplicationName

instance Pretty L.Coin where
  pretty (L.Coin n) = pretty n <+> "Lovelace"

instance Pretty L.MultiAsset where
  pretty (L.MultiAsset assetsMap) =
    mconcat $
      punctuate
        ", "
        [ pretty quantity <+> pretty pId <> "." <> pretty name
        | (pId, assets) <- toList assetsMap
        , (name, quantity) <- toList assets
        ]

instance Pretty L.PolicyID where
  pretty (L.PolicyID (L.ScriptHash sh)) = pretty $ Crypto.hashToStringAsHex sh

instance Pretty L.AssetName where
  pretty = pretty . L.assetNameToTextAsHex

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance ToJSON Consensus.StakeSnapshots where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair
  :: Aeson.KeyValue e a => Consensus.StakeSnapshots -> [a]
stakeSnapshotsToPair
  Consensus.StakeSnapshots
    { Consensus.ssStakeSnapshots
    , Consensus.ssMarkTotal
    , Consensus.ssSetTotal
    , Consensus.ssGoTotal
    } =
    [ "pools" .= ssStakeSnapshots
    , "total"
        .= object
          [ "stakeMark" .= ssMarkTotal
          , "stakeSet" .= ssSetTotal
          , "stakeGo" .= ssGoTotal
          ]
    ]

instance ToJSON Consensus.StakeSnapshot where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue e a => Consensus.StakeSnapshot -> [a]
stakeSnapshotToPair
  Consensus.StakeSnapshot
    { Consensus.ssMarkPool
    , Consensus.ssSetPool
    , Consensus.ssGoPool
    } =
    [ "stakeMark" .= ssMarkPool
    , "stakeSet" .= ssSetPool
    , "stakeGo" .= ssGoPool
    ]

instance ToJSON (OneEraHash xs) where
  toJSON =
    toJSON
      . Text.decodeLatin1
      . Base16.encode
      . SBS.fromShort
      . getOneEraHash

deriving newtype instance ToJSON ByronHash

-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = Aeson.object ["genesis" .= True]
  toJSON (Tip slotNo headerHash blockNo) =
    Aeson.object
      [ "slotNo" .= slotNo
      , "headerHash" .= headerHash
      , "blockNo" .= blockNo
      ]

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance ToJSON ShelleyHash

deriving newtype instance ToJSON HashHeader

deriving instance ToJSON Ledger.PrtclState

deriving instance ToJSON Ledger.TicknState

deriving instance ToJSON Ledger.ChainDepState

instance ToJSON TPraosState where
  toJSON s =
    Aeson.object
      [ "lastSlot" .= Consensus.tpraosStateLastSlot s
      , "chainDepState" .= Consensus.tpraosStateChainDepState s
      ]

instance ToJSON PraosState where
  toJSON s =
    Aeson.object
      [ "lastSlot" .= Consensus.praosStateLastSlot s
      , "oCertCounters" .= Consensus.praosStateOCertCounters s
      , "evolvingNonce" .= Consensus.praosStateEvolvingNonce s
      , "candidateNonce" .= Consensus.praosStateCandidateNonce s
      , "epochNonce" .= Consensus.praosStateEpochNonce s
      , "labNonce" .= Consensus.praosStateLabNonce s
      , "lastEpochBlockNonce" .= Consensus.praosStateLastEpochBlockNonce s
      ]

deriving instance Show a => Show (Net.Tx.SubmitResult a)

instance A.FromJSON V2.ParamName where
  parseJSON = A.withText "ParamName" parsePlutusParamName

instance A.FromJSONKey V2.ParamName where
  fromJSONKey = A.FromJSONKeyTextParser parsePlutusParamName

parsePlutusParamName :: (P.IsParamName a, MonadFail f) => T.Text -> f a
parsePlutusParamName t =
  case P.readParamName t of
    Just p -> pure p
    Nothing -> fail $ "Cannot parse cost model parameter name: " <> T.unpack t

deriving instance Show V2.ParamName

instance HasTypeProxy (Ledger.Credential L.ColdCommitteeRole) where
  data AsType (Ledger.Credential L.ColdCommitteeRole) = AsColdCommitteeCredential
  proxyToAsType _ = AsColdCommitteeCredential

instance SerialiseAsRawBytes (Ledger.Credential L.ColdCommitteeRole) where
  serialiseToRawBytes = CBOR.serialize'
  deserialiseFromRawBytes AsColdCommitteeCredential =
    first
      ( \e ->
          SerialiseAsRawBytesError
            ("Unable to deserialise Credential ColdCommitteeRole: " ++ show e)
      )
      . CBOR.decodeFull'

instance HasTypeProxy (Ledger.Credential L.HotCommitteeRole) where
  data AsType (Ledger.Credential L.HotCommitteeRole) = AsHotCommitteeCredential
  proxyToAsType _ = AsHotCommitteeCredential

instance SerialiseAsRawBytes (Ledger.Credential L.HotCommitteeRole) where
  serialiseToRawBytes = CBOR.serialize'
  deserialiseFromRawBytes AsHotCommitteeCredential =
    first
      ( \e ->
          SerialiseAsRawBytesError
            ("Unable to deserialise Credential HotCommitteeRole: " ++ show e)
      )
      . CBOR.decodeFull'

instance HasTypeProxy (Ledger.Credential L.DRepRole) where
  data AsType (Ledger.Credential L.DRepRole) = AsDrepCredential
  proxyToAsType _ = AsDrepCredential

instance SerialiseAsRawBytes (Ledger.Credential L.DRepRole) where
  serialiseToRawBytes = CBOR.serialize'
  deserialiseFromRawBytes AsDrepCredential =
    first
      ( \e ->
          SerialiseAsRawBytesError ("Unable to deserialise Credential DRepRole: " ++ show e)
      )
      . CBOR.decodeFull'

instance HasTypeProxy L.GovActionIx where
  data AsType L.GovActionIx = AsGovActionIx
  proxyToAsType _ = AsGovActionIx

instance HasTypeProxy L.GovActionId where
  data AsType L.GovActionId = AsGovActionId
  proxyToAsType _ = AsGovActionId

instance SerialiseAsRawBytes L.GovActionIx where
  serialiseToRawBytes (L.GovActionIx actionIx) = C8.toStrict . BSB.toLazyByteString $ BSB.word16BE actionIx
  deserialiseFromRawBytes _ bs =
    L.GovActionIx
      <$> case fromIntegral <$> BS.unpack bs of
        [] -> throwError $ SerialiseAsRawBytesError "Cannot deserialise empty bytes into GovActionIx"
        [b0] -> pure b0 -- just return the single byte present
        [b0, b1] ->
          -- we have number > 255, so we have to convert from big endian
          pure $ b0 `shiftL` 8 .|. b1
        _ ->
          -- we cannot have more than two bytes for the index here
          throwError . SerialiseAsRawBytesError $
            "Governance action index is larger than two bytes! hex index: " <> C8.unpack (Base16.encode bs)

instance SerialiseAsRawBytes L.GovActionId where
  serialiseToRawBytes (L.GovActionId txid govActIx) =
    serialiseToRawBytes (fromShelleyTxId txid) <> serialiseToRawBytes govActIx

  deserialiseFromRawBytes AsGovActionId bytes = do
    let (txIdBs, index) = BS.splitAt 32 bytes
    L.GovActionId . toShelleyTxId
      <$> deserialiseFromRawBytes AsTxId txIdBs
      <*> deserialiseFromRawBytes AsGovActionIx index
