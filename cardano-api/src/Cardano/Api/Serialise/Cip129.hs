{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Serialise.Cip129
  ( Cip129 (..)
  , deserialiseFromBech32Cip129
  , serialiseToBech32Cip129
  , serialiseGovActionIdToBech32Cip129
  , deserialiseGovActionIdFromBech32Cip129
  , AsType (AsColdCommitteeCredential, AsDrepCredential, AsHotCommitteeCredential)
  )
where

import Cardano.Api.Governance.Internal.Action.ProposalProcedure
import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Orphans (AsType (..))
import Cardano.Api.Monad.Error
import Cardano.Api.Serialise.Bech32
import Cardano.Api.Serialise.Raw

import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.Conway.Governance qualified as Gov
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Keys qualified as L

import Codec.Binary.Bech32 qualified as Bech32
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import GHC.Exts (IsList (..))
import Numeric (showHex)

-- | Cip-129 is a typeclass that captures the serialisation requirements of https://cips.cardano.org/cip/CIP-0129
-- which pertain to governance credentials and governance action ids.
class (SerialiseAsRawBytes a, HasTypeProxy a) => Cip129 a where
  -- | The human readable part of the Bech32 encoding for the credential.
  cip129Bech32PrefixFor :: AsType a -> Bech32.HumanReadablePart

  -- | The header byte that identifies the credential type according to CIP-129.
  cip129Header :: a -> Word8

  -- | Permitted bech32 prefixes according to Cip-129.
  cip129Bech32PrefixesPermitted :: AsType a -> [Text]
  default cip129Bech32PrefixesPermitted :: AsType a -> [Text]
  cip129Bech32PrefixesPermitted = return . Bech32.humanReadablePartToText . cip129Bech32PrefixFor

  -- | Serialise a value. It's usually distinct from CBOR serialisation, since it's not encoding constructor number
  -- and it's using a special header value.
  cip129SerialiseRaw :: a -> BS.ByteString

  cip129DeserialiseRaw :: BS.ByteString -> Either SerialiseAsRawBytesError a

instance Cip129 (Credential L.ColdCommitteeRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "cc_cold"
  cip129Bech32PrefixesPermitted AsColdCommitteeCredential = ["cc_cold"]

  cip129SerialiseRaw = \case
    L.KeyHashObj (L.KeyHash kh) -> BS.singleton 0b0001_0010 <> Hash.hashToBytes kh
    L.ScriptHashObj (L.ScriptHash sh) -> BS.singleton 0b0001_0011 <> Hash.hashToBytes sh

  cip129DeserialiseRaw bytes =
    -- TODO use a better error here
    maybe (Left $ SerialiseAsRawBytesError $ show $ Base16.encode bytes) pure $
      case BS.uncons bytes of
        Just (0b0001_0010, cred) -> L.KeyHashObj . L.KeyHash <$> Hash.hashFromBytes cred
        Just (0b0001_0011, cred) -> L.ScriptHashObj . L.ScriptHash <$> Hash.hashFromBytes cred
        _ -> Nothing

instance Cip129 (Credential L.HotCommitteeRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "cc_hot"
  cip129Bech32PrefixesPermitted AsHotCommitteeCredential = ["cc_hot"]

  cip129SerialiseRaw = \case
    L.KeyHashObj (L.KeyHash kh) -> BS.singleton 0b0000_0010 <> Hash.hashToBytes kh
    L.ScriptHashObj (L.ScriptHash sh) -> BS.singleton 0b0000_0011 <> Hash.hashToBytes sh

  cip129DeserialiseRaw bytes =
    -- TODO use a better error here
    maybe (Left $ SerialiseAsRawBytesError $ show $ Base16.encode bytes) pure $
      case BS.uncons bytes of
        Just (0b0000_0010, cred) -> L.KeyHashObj . L.KeyHash <$> Hash.hashFromBytes cred
        Just (0b0000_0011, cred) -> L.ScriptHashObj . L.ScriptHash <$> Hash.hashFromBytes cred
        _ -> Nothing

instance Cip129 (Credential L.DRepRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "drep"
  cip129Bech32PrefixesPermitted AsDrepCredential = ["drep"]

  cip129SerialiseRaw = \case
    L.KeyHashObj (L.KeyHash kh) -> BS.singleton 0b0010_0010 <> Hash.hashToBytes kh
    L.ScriptHashObj (L.ScriptHash sh) -> BS.singleton 0b0010_0011 <> Hash.hashToBytes sh

  cip129DeserialiseRaw bytes =
    -- TODO use a better error here
    maybe (Left $ SerialiseAsRawBytesError $ show $ Base16.encode bytes) pure $
      case BS.uncons bytes of
        Just (0b0010_0010, cred) -> L.KeyHashObj . L.KeyHash <$> Hash.hashFromBytes cred
        Just (0b0010_0011, cred) -> L.ScriptHashObj . L.ScriptHash <$> Hash.hashFromBytes cred
        _ -> Nothing

instance Cip129 Gov.GovActionId where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "gov_action"
  cip129Bech32PrefixesPermitted AsGovActionId = ["gov_action"]

  cip129SerialiseRaw = serialiseToRawBytes
  cip129DeserialiseRaw = deserialiseFromRawBytes AsGovActionId

-- | Serialize a accoding to the serialisation requirements of https://cips.cardano.org/cip/CIP-0129
-- which currently pertain to governance credentials. Governance action ids are dealt separately with
-- via 'serialiseGovActionIdToBech32Cip129'.
serialiseToBech32Cip129 :: forall a. Cip129 a => a -> Text
serialiseToBech32Cip129 a =
  Bech32.encodeLenient
    humanReadablePart
    (Bech32.dataPartFromBytes $ cip129SerialiseRaw a)
 where
  humanReadablePart = cip129Bech32PrefixFor (asType @a)

deserialiseFromBech32Cip129
  :: forall a
   . Cip129 a
  => Text
  -> Either Bech32DecodeError a
deserialiseFromBech32Cip129 bech32Str = do
  (prefix, dataPart) <-
    Bech32.decodeLenient bech32Str
      ?!& Bech32DecodingError

  let actualPrefix = Bech32.humanReadablePartToText prefix
      permittedPrefixes = cip129Bech32PrefixesPermitted (asType @a)
  guard (actualPrefix `elem` permittedPrefixes)
    ?! Bech32UnexpectedPrefix actualPrefix (fromList permittedPrefixes)

  payload <-
    Bech32.dataPartToBytes dataPart
      ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

  -- (header, _) <- BS.uncons payload ?! Bech32DeserialiseFromBytesError payload
  value <-
    cip129DeserialiseRaw payload ?!& const (Bech32DeserialiseFromBytesError $ Base16.encode payload)

  -- let expectedHeader = cip129Header value
  --
  -- guard (header == expectedHeader)
  --   ?! Bech32UnexpectedHeader (toBase16Text expectedHeader) (toBase16Text header)

  let expectedPrefix = Bech32.humanReadablePartToText $ cip129Bech32PrefixFor (asType @a)
  guard (actualPrefix == expectedPrefix)
    ?! Bech32WrongPrefix actualPrefix expectedPrefix

  pure value
 where
  toBase16Text w = Text.pack $ showHex w ""

-- | Governance Action ID
-- According to Cip129 there is no header byte for GovActionId.
-- Instead they append the txid and index to form the payload.

-- TODO REMOVE
serialiseGovActionIdToBech32Cip129 :: Gov.GovActionId -> Text
serialiseGovActionIdToBech32Cip129 govActionId = do
  let humanReadablePart = unsafeHumanReadablePartFromText "gov_action"
  Bech32.encodeLenient
    humanReadablePart
    (Bech32.dataPartFromBytes $ serialiseToRawBytes govActionId)

-- TODO REMOVE
deserialiseGovActionIdFromBech32Cip129
  :: Text -> Either Bech32DecodeError Gov.GovActionId
deserialiseGovActionIdFromBech32Cip129 bech32Str = do
  let permittedPrefixes = ["gov_action"]
  (prefix, dataPart) <-
    Bech32.decodeLenient bech32Str
      ?!& Bech32DecodingError
  let actualPrefix = Bech32.humanReadablePartToText prefix
  guard (actualPrefix `elem` permittedPrefixes)
    ?! Bech32UnexpectedPrefix actualPrefix (fromList permittedPrefixes)

  payload <-
    Bech32.dataPartToBytes dataPart
      ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

  deserialiseFromRawBytes AsGovActionId payload
    ?!& const (Bech32DeserialiseFromBytesError payload)
