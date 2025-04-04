{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Byron key types and their 'Key' class instances
module Cardano.Api.Internal.Keys.Byron
  ( -- * Key types
    ByronKey
  , ByronKeyLegacy

    -- * Data family instances
  , AsType (..)
  , VerificationKey (..)
  , SigningKey (..)
  , Hash (..)

    -- * Legacy format
  , IsByronKey (..)
  , ByronKeyFormat (..)
  , SomeByronSigningKey (..)
  , toByronSigningKey
  )
where

import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Hash
import Cardano.Api.Internal.Keys.Class
import Cardano.Api.Internal.Keys.Shelley
import Cardano.Api.Internal.Serialise.Cbor
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseTextEnvelope
import Cardano.Api.Internal.SerialiseUsing

import Cardano.Binary (cborError, toStrictByteString)
import Cardano.Chain.Common qualified as Crypto
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Crypto.Hashing qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Crypto.Signing qualified as Crypto
import Cardano.Crypto.Wallet qualified as Crypto.HD
import Cardano.Crypto.Wallet qualified as Wallet

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad
import Data.Bifunctor
import Data.ByteString.Lazy qualified as LB
import Data.Either.Combinators
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Formatting (build, formatToString)

-- | Byron-era payment keys. Used for Byron addresses and witnessing
-- transactions that spend from these addresses.
--
-- These use Ed25519 but with a 32byte \"chaincode\" used in HD derivation.
-- The inclusion of the chaincode is a design mistake but one that cannot
-- be corrected for the Byron era. The Shelley era 'PaymentKey's do not include
-- a chaincode. It is safe to use a zero or random chaincode for new Byron keys.
--
-- This is a type level tag, used with other interfaces like 'Key'.
data ByronKey

data ByronKeyLegacy

class IsByronKey key where
  byronKeyFormat :: ByronKeyFormat key

data ByronKeyFormat key where
  ByronLegacyKeyFormat :: ByronKeyFormat ByronKeyLegacy
  ByronModernKeyFormat :: ByronKeyFormat ByronKey

data SomeByronSigningKey
  = AByronSigningKeyLegacy (SigningKey ByronKeyLegacy)
  | AByronSigningKey (SigningKey ByronKey)

toByronSigningKey :: SomeByronSigningKey -> Crypto.SigningKey
toByronSigningKey bWit =
  case bWit of
    AByronSigningKeyLegacy (ByronSigningKeyLegacy sKey) -> sKey
    AByronSigningKey (ByronSigningKey sKey) -> sKey

--
-- Byron key
--

instance Key ByronKey where
  newtype VerificationKey ByronKey
    = ByronVerificationKey Crypto.VerificationKey
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ByronKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey ByronKey
    = ByronSigningKey Crypto.SigningKey
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey ByronKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  deterministicSigningKey :: AsType ByronKey -> Crypto.Seed -> SigningKey ByronKey
  deterministicSigningKey AsByronKey seed =
    ByronSigningKey (snd (Crypto.runMonadRandomWithSeed seed Crypto.keyGen))

  deterministicSigningKeySeedSize :: AsType ByronKey -> Word
  deterministicSigningKeySeedSize AsByronKey = 32

  getVerificationKey :: SigningKey ByronKey -> VerificationKey ByronKey
  getVerificationKey (ByronSigningKey sk) =
    ByronVerificationKey (Crypto.toVerification sk)

  verificationKeyHash :: VerificationKey ByronKey -> Hash ByronKey
  verificationKeyHash (ByronVerificationKey vkey) =
    ByronKeyHash (Crypto.hashKey vkey)

instance HasTypeProxy ByronKey where
  data AsType ByronKey = AsByronKey
  proxyToAsType _ = AsByronKey

instance HasTextEnvelope (VerificationKey ByronKey) where
  textEnvelopeType _ = "PaymentVerificationKeyByron_ed25519_bip32"

instance HasTextEnvelope (SigningKey ByronKey) where
  textEnvelopeType _ = "PaymentSigningKeyByron_ed25519_bip32"

instance SerialiseAsRawBytes (VerificationKey ByronKey) where
  serialiseToRawBytes (ByronVerificationKey (Crypto.VerificationKey xvk)) =
    Crypto.HD.unXPub xvk

  deserialiseFromRawBytes (AsVerificationKey AsByronKey) bs =
    first (\msg -> SerialiseAsRawBytesError ("Unable to deserialise VerificationKey ByronKey" ++ msg)) $
      ByronVerificationKey . Crypto.VerificationKey <$> Crypto.HD.xpub bs

instance SerialiseAsRawBytes (SigningKey ByronKey) where
  serialiseToRawBytes (ByronSigningKey sk) = toStrictByteString $ toCBOR sk

  deserialiseFromRawBytes (AsSigningKey AsByronKey) bs =
    first (\e -> SerialiseAsRawBytesError ("Unable to deserialise SigningKey ByronKey" ++ show e)) $
      ByronSigningKey . snd <$> CBOR.deserialiseFromBytes fromCBOR (LB.fromStrict bs)

newtype instance Hash ByronKey = ByronKeyHash Crypto.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ByronKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash ByronKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash ByronKey) where
  serialiseToRawBytes (ByronKeyHash (Crypto.KeyHash vkh)) =
    Crypto.abstractHashToBytes vkh

  deserialiseFromRawBytes (AsHash AsByronKey) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash ByronKey") $
      ByronKeyHash . Crypto.KeyHash <$> Crypto.abstractHashFromBytes bs

instance CastVerificationKeyRole ByronKey PaymentExtendedKey where
  castVerificationKey (ByronVerificationKey vk) =
    PaymentExtendedVerificationKey
      (Crypto.unVerificationKey vk)

instance CastVerificationKeyRole ByronKey PaymentKey where
  castVerificationKey =
    ( castVerificationKey
        :: VerificationKey PaymentExtendedKey
        -> VerificationKey PaymentKey
    )
      . ( castVerificationKey
            :: VerificationKey ByronKey
            -> VerificationKey PaymentExtendedKey
        )

instance IsByronKey ByronKey where
  byronKeyFormat = ByronModernKeyFormat

--
-- Legacy Byron key
--

instance Key ByronKeyLegacy where
  newtype VerificationKey ByronKeyLegacy
    = ByronVerificationKeyLegacy Crypto.VerificationKey
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ByronKeyLegacy)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey ByronKeyLegacy
    = ByronSigningKeyLegacy Crypto.SigningKey
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey ByronKeyLegacy)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  deterministicSigningKey :: AsType ByronKeyLegacy -> Crypto.Seed -> SigningKey ByronKeyLegacy
  deterministicSigningKey _ _ = error "Please generate a non legacy Byron key instead"

  deterministicSigningKeySeedSize :: AsType ByronKeyLegacy -> Word
  deterministicSigningKeySeedSize AsByronKeyLegacy = 32

  getVerificationKey :: SigningKey ByronKeyLegacy -> VerificationKey ByronKeyLegacy
  getVerificationKey (ByronSigningKeyLegacy sk) =
    ByronVerificationKeyLegacy (Crypto.toVerification sk)

  verificationKeyHash :: VerificationKey ByronKeyLegacy -> Hash ByronKeyLegacy
  verificationKeyHash (ByronVerificationKeyLegacy vkey) =
    ByronKeyHashLegacy (Crypto.hashKey vkey)

instance HasTypeProxy ByronKeyLegacy where
  data AsType ByronKeyLegacy = AsByronKeyLegacy
  proxyToAsType _ = AsByronKeyLegacy

instance HasTextEnvelope (VerificationKey ByronKeyLegacy) where
  textEnvelopeType _ = "PaymentVerificationKeyByronLegacy_ed25519_bip32"

instance HasTextEnvelope (SigningKey ByronKeyLegacy) where
  textEnvelopeType _ = "PaymentSigningKeyByronLegacy_ed25519_bip32"

newtype instance Hash ByronKeyLegacy = ByronKeyHashLegacy Crypto.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ByronKeyLegacy)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash ByronKeyLegacy)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash ByronKeyLegacy) where
  serialiseToRawBytes (ByronKeyHashLegacy (Crypto.KeyHash vkh)) =
    Crypto.abstractHashToBytes vkh

  deserialiseFromRawBytes (AsHash AsByronKeyLegacy) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash ByronKeyLegacy") $
      ByronKeyHashLegacy . Crypto.KeyHash <$> Crypto.abstractHashFromBytes bs

instance SerialiseAsRawBytes (VerificationKey ByronKeyLegacy) where
  serialiseToRawBytes (ByronVerificationKeyLegacy (Crypto.VerificationKey xvk)) =
    Crypto.HD.unXPub xvk

  deserialiseFromRawBytes (AsVerificationKey AsByronKeyLegacy) bs =
    first
      (\msg -> SerialiseAsRawBytesError ("Unable to deserialise VerificationKey ByronKeyLegacy" ++ msg))
      $ ByronVerificationKeyLegacy . Crypto.VerificationKey <$> Crypto.HD.xpub bs

instance SerialiseAsRawBytes (SigningKey ByronKeyLegacy) where
  serialiseToRawBytes (ByronSigningKeyLegacy (Crypto.SigningKey xsk)) =
    Crypto.HD.unXPrv xsk

  deserialiseFromRawBytes (AsSigningKey AsByronKeyLegacy) bs =
    first (\e -> SerialiseAsRawBytesError ("Unable to deserialise SigningKey ByronKeyLegacy" ++ show e)) $
      ByronSigningKeyLegacy . snd <$> CBOR.deserialiseFromBytes decodeLegacyDelegateKey (LB.fromStrict bs)
   where
    -- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
    -- \| Enforces that the input size is the same as the decoded one, failing in
    -- case it's not.
    enforceSize :: Text -> Int -> CBOR.Decoder s ()
    enforceSize lbl requestedSize = CBOR.decodeListLenCanonical >>= matchSize requestedSize lbl

    -- Stolen from: cardano-sl/binary/src/Pos/Binary/Class/Core.hs
    -- \| Compare two sizes, failing if they are not equal.
    matchSize :: Int -> Text -> Int -> CBOR.Decoder s ()
    matchSize requestedSize lbl actualSize =
      when (actualSize /= requestedSize) $
        cborError
          ( lbl
              <> " failed the size check. Expected "
              <> Text.pack (show requestedSize)
              <> ", found "
              <> Text.pack (show actualSize)
          )

    decodeXPrv :: CBOR.Decoder s Wallet.XPrv
    decodeXPrv = CBOR.decodeBytesCanonical >>= either (fail . formatToString build) pure . Wallet.xprv

    -- \| Decoder for a Byron/Classic signing key.
    --   Lifted from cardano-sl legacy codebase.
    decodeLegacyDelegateKey :: CBOR.Decoder s Crypto.SigningKey
    decodeLegacyDelegateKey = do
      enforceSize "UserSecret" 4
      _ <- do
        enforceSize "vss" 1
        CBOR.decodeBytes
      pkey <- do
        enforceSize "pkey" 1
        Crypto.SigningKey <$> decodeXPrv
      _ <- do
        CBOR.decodeListLenIndef
        CBOR.decodeSequenceLenIndef (flip (:)) [] reverse CBOR.decodeNull
      _ <- do
        enforceSize "wallet" 0
      pure pkey

instance CastVerificationKeyRole ByronKeyLegacy ByronKey where
  castVerificationKey (ByronVerificationKeyLegacy vk) =
    ByronVerificationKey vk

instance IsByronKey ByronKeyLegacy where
  byronKeyFormat = ByronLegacyKeyFormat
