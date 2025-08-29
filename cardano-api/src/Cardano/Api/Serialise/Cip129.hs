{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Serialise.Cip129
  ( Cip129 (..)
  , Cip129EncodingError
  , deserialiseFromBech32Cip129
  , serialiseToBech32Cip129
  , serialiseGovActionIdToBech32Cip129
  , deserialiseGovActionIdFromBech32Cip129
  , AsType (AsColdCommitteeCredential, AsDrepCredential, AsHotCommitteeCredential)
  )
where

import Cardano.Api.Error
import Cardano.Api.Governance.Internal.Action.ProposalProcedure
import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Orphans (AsType (..))
import Cardano.Api.Monad.Error
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Bech32
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing

import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.Conway.Governance qualified as Gov
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Credential qualified as L

import Codec.Binary.Bech32 qualified as Bech32
import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Typeable
import Data.Word (Word8)
import GHC.Exts (IsList (..))

-- | Cip-129 is a typeclass that captures the serialisation requirements of https://cips.cardano.org/cip/CIP-0129
-- which pertain to governance credentials and governance action ids.
class (SerialiseAsRawBytes a, HasTypeProxy a) => Cip129 a where
  -- | The human readable part of the Bech32 encoding for the credential.
  cip129Bech32PrefixFor :: AsType a -> Bech32.HumanReadablePart

  -- | Permitted bech32 prefixes according to CIP-129.
  cip129Bech32PrefixesPermitted :: AsType a -> [Text]
  default cip129Bech32PrefixesPermitted :: AsType a -> [Text]
  cip129Bech32PrefixesPermitted = return . Bech32.humanReadablePartToText . cip129Bech32PrefixFor

  -- | Serialise a value to a binary representation used in CIP 129. It's usually distinct from CBOR serialisation.
  -- Internal conversion function. Use 'serialiseToBech32Cip129' instead of calling this function directly.
  cip129SerialiseRaw :: a -> BS.ByteString

  -- | Deserialise a value from the bytes representation. Internal conversion function. Use
  -- 'deserialiseFromBech32Cip129' instead of calling this function directly.
  cip129DeserialiseRaw :: BS.ByteString -> Either Cip129EncodingError a

-- | CIP-129 decoding errors
data Cip129EncodingError
  = CeeTypeDecodingError TypeRep BS.ByteString
  | CeeUnknownHeaderError TypeRep Word8
  | CeeEmptyBytesError TypeRep
  | CeeBech32Error TypeRep Bech32DecodeError
  deriving (Eq, Show)

instance Error Cip129EncodingError where
  prettyError = \case
    CeeTypeDecodingError tr bytes ->
      "Cannot decode CIP129 encoding of a type \""
        <> pretty tr
        <> "\", bytes hex: "
        <> pretty (UsingRawBytesHex bytes)
    CeeUnknownHeaderError tr header ->
      "Cannot decode CIP129 header of a type \""
        <> pretty tr
        <> "\", header bytes hex: "
        <> pretty (UsingRawBytesHex header)
    CeeEmptyBytesError tr ->
      "Cannot decode CIP129 header of a type \"" <> pretty tr <> "\", cannot decode empty bytes"
    CeeBech32Error tr be ->
      "Cannot decode CIP129 encoding of a type \""
        <> pretty tr
        <> "\", due to Bech32 decoding error: "
        <> prettyError be

instance Cip129 (Credential L.ColdCommitteeRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "cc_cold"
  cip129Bech32PrefixesPermitted AsColdCommitteeCredential = ["cc_cold"]

  cip129SerialiseRaw = \case
    L.KeyHashObj (L.KeyHash kh) -> BS.singleton 0b0001_0010 <> Hash.hashToBytes kh
    L.ScriptHashObj (L.ScriptHash sh) -> BS.singleton 0b0001_0011 <> Hash.hashToBytes sh

  cip129DeserialiseRaw
    :: forall a
     . a ~ Credential L.ColdCommitteeRole
    => BS.ByteString
    -> Either Cip129EncodingError a
  cip129DeserialiseRaw bytes = do
    let t = typeRep $ Proxy @a
    case BS.uncons bytes of
      Just (0b0001_0010, cred) -> L.KeyHashObj . L.KeyHash <$> Hash.hashFromBytes cred ?! CeeTypeDecodingError t bytes
      Just (0b0001_0011, cred) -> L.ScriptHashObj . L.ScriptHash <$> Hash.hashFromBytes cred ?! CeeTypeDecodingError t bytes
      Just (header, _) -> throwError $ CeeUnknownHeaderError t header
      Nothing -> throwError $ CeeEmptyBytesError t

instance Cip129 (Credential L.HotCommitteeRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "cc_hot"
  cip129Bech32PrefixesPermitted AsHotCommitteeCredential = ["cc_hot"]

  cip129SerialiseRaw = \case
    L.KeyHashObj (L.KeyHash kh) -> BS.singleton 0b0000_0010 <> Hash.hashToBytes kh
    L.ScriptHashObj (L.ScriptHash sh) -> BS.singleton 0b0000_0011 <> Hash.hashToBytes sh

  cip129DeserialiseRaw
    :: forall a
     . a ~ Credential L.HotCommitteeRole
    => BS.ByteString
    -> Either Cip129EncodingError a
  cip129DeserialiseRaw bytes = do
    let t = typeRep $ Proxy @a
    case BS.uncons bytes of
      Just (0b0000_0010, cred) -> L.KeyHashObj . L.KeyHash <$> Hash.hashFromBytes cred ?! CeeTypeDecodingError t bytes
      Just (0b0000_0011, cred) -> L.ScriptHashObj . L.ScriptHash <$> Hash.hashFromBytes cred ?! CeeTypeDecodingError t bytes
      Just (header, _) -> throwError $ CeeUnknownHeaderError t header
      Nothing -> throwError $ CeeEmptyBytesError t

instance Cip129 (Credential L.DRepRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "drep"
  cip129Bech32PrefixesPermitted AsDrepCredential = ["drep"]

  cip129SerialiseRaw = \case
    L.KeyHashObj (L.KeyHash kh) -> BS.singleton 0b0010_0010 <> Hash.hashToBytes kh
    L.ScriptHashObj (L.ScriptHash sh) -> BS.singleton 0b0010_0011 <> Hash.hashToBytes sh

  cip129DeserialiseRaw
    :: forall a
     . a ~ Credential L.DRepRole
    => BS.ByteString
    -> Either Cip129EncodingError a
  cip129DeserialiseRaw bytes = do
    let t = typeRep $ Proxy @a
    case BS.uncons bytes of
      Just (0b0010_0010, cred) -> L.KeyHashObj . L.KeyHash <$> Hash.hashFromBytes cred ?! CeeTypeDecodingError t bytes
      Just (0b0010_0011, cred) -> L.ScriptHashObj . L.ScriptHash <$> Hash.hashFromBytes cred ?! CeeTypeDecodingError t bytes
      Just (header, _) -> throwError $ CeeUnknownHeaderError t header
      Nothing -> throwError $ CeeEmptyBytesError t

instance Cip129 Gov.GovActionId where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "gov_action"
  cip129Bech32PrefixesPermitted AsGovActionId = ["gov_action"]

  cip129SerialiseRaw = serialiseToRawBytes

  cip129DeserialiseRaw
    :: forall a
     . a ~ Gov.GovActionId
    => BS.ByteString
    -> Either Cip129EncodingError a
  cip129DeserialiseRaw bs =
    deserialiseFromRawBytes AsGovActionId bs ?!& const (CeeTypeDecodingError (typeRep $ Proxy @a) bs)

-- | Serialise a accoding to the serialisation requirements of https://cips.cardano.org/cip/CIP-0129
-- which currently pertain to governance credentials.
serialiseToBech32Cip129 :: forall a. Cip129 a => a -> Text
serialiseToBech32Cip129 a =
  Bech32.encodeLenient
    humanReadablePart
    (Bech32.dataPartFromBytes $ cip129SerialiseRaw a)
 where
  humanReadablePart = cip129Bech32PrefixFor (asType @a)

-- | Deserialise a governance identifier from CIP-129 format.
deserialiseFromBech32Cip129
  :: forall a
   . Cip129 a
  => Text
  -- ^ A Bech32-encoded governance identifier
  -> Either Cip129EncodingError a
deserialiseFromBech32Cip129 bech32Str = do
  let type' = typeRep $ Proxy @a
  (prefix, dataPart) <-
    Bech32.decodeLenient bech32Str
      ?!& CeeBech32Error type'
      . Bech32DecodingError

  let actualPrefix = Bech32.humanReadablePartToText prefix
      permittedPrefixes = cip129Bech32PrefixesPermitted (asType @a)
  guard (actualPrefix `elem` permittedPrefixes)
    ?! CeeBech32Error type' (Bech32UnexpectedPrefix actualPrefix (fromList permittedPrefixes))

  payload <-
    Bech32.dataPartToBytes dataPart
      ?! CeeBech32Error type' (Bech32DataPartToBytesError (Bech32.dataPartToText dataPart))

  value <-
    cip129DeserialiseRaw payload
      ?!& const (CeeBech32Error type' . Bech32DeserialiseFromBytesError $ Base16.encode payload)

  let expectedPrefix = Bech32.humanReadablePartToText $ cip129Bech32PrefixFor (asType @a)
  guard (actualPrefix == expectedPrefix)
    ?! CeeBech32Error type' (Bech32WrongPrefix actualPrefix expectedPrefix)

  pure value

-- | Governance Action ID
-- According to Cip129 there is no header byte for GovActionId.
-- Instead they append the txid and index to form the payload.
{-# DEPRECATED serialiseGovActionIdToBech32Cip129 "Use serialiseToBech32Cip129 instead" #-}
serialiseGovActionIdToBech32Cip129 :: Gov.GovActionId -> Text
serialiseGovActionIdToBech32Cip129 = serialiseToBech32Cip129

{-# DEPRECATED deserialiseGovActionIdFromBech32Cip129 "Use deserialiseFromBech32Cip129 instead" #-}
deserialiseGovActionIdFromBech32Cip129 :: Text -> Either Cip129EncodingError Gov.GovActionId
deserialiseGovActionIdFromBech32Cip129 = deserialiseFromBech32Cip129
