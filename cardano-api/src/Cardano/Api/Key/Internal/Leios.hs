{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Leios specific key types and their 'Key' class instances
module Cardano.Api.Key.Internal.Leios
  ( -- * Key types
    BlsKey

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  , VerificationKey (..)
  , SigningKey (..)
  )
where

import Cardano.Api.HasTypeProxy
import Cardano.Api.Hash
import Cardano.Api.Key.Internal.Class
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Bech32
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing
import Cardano.Api.Serialise.TextEnvelope.Internal

import Cardano.Crypto.DSIGN.BLS12381 qualified as Crypto
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Hashes (HASH)

import Data.Either.Combinators (maybeToRight)
import Data.String (IsString (..))

--
-- BLS keys
--

data BlsKey

instance HasTypeProxy BlsKey where
  data AsType BlsKey = AsBlsKey
  proxyToAsType _ = AsBlsKey

instance Key BlsKey where
  newtype VerificationKey BlsKey
    = BlsVerificationKey (Crypto.VerKeyDSIGN Crypto.BLS12381MinSigDSIGN)
    deriving stock Eq
    deriving (Show, Pretty) via UsingRawBytesHex (VerificationKey BlsKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey BlsKey
    = BlsSigningKey (Crypto.SignKeyDSIGN Crypto.BLS12381MinSigDSIGN)
    deriving (Show, Pretty) via UsingRawBytesHex (SigningKey BlsKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  deterministicSigningKey :: AsType BlsKey -> Crypto.Seed -> SigningKey BlsKey
  deterministicSigningKey AsBlsKey =
    BlsSigningKey . Crypto.genKeyDSIGN

  deterministicSigningKeySeedSize :: AsType BlsKey -> Word
  deterministicSigningKeySeedSize AsBlsKey =
    Crypto.seedSizeDSIGN proxy
   where
    proxy :: Proxy Crypto.BLS12381MinSigDSIGN
    proxy = Proxy

  getVerificationKey :: SigningKey BlsKey -> VerificationKey BlsKey
  getVerificationKey (BlsSigningKey sk) =
    BlsVerificationKey (Crypto.deriveVerKeyDSIGN sk)

  verificationKeyHash :: VerificationKey BlsKey -> Hash BlsKey
  verificationKeyHash (BlsVerificationKey vkey) =
    BlsKeyHash (Crypto.hashVerKeyDSIGN vkey)

instance SerialiseAsRawBytes (VerificationKey BlsKey) where
  serialiseToRawBytes (BlsVerificationKey vk) =
    Crypto.rawSerialiseVerKeyDSIGN vk

  deserialiseFromRawBytes (AsVerificationKey AsBlsKey) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise VerificationKey BlsKey") $
      BlsVerificationKey <$> Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey BlsKey) where
  serialiseToRawBytes (BlsSigningKey sk) =
    Crypto.rawSerialiseSignKeyDSIGN sk

  deserialiseFromRawBytes (AsSigningKey AsBlsKey) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise SigningKey BlsKey") $
      BlsSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey BlsKey) where
  bech32PrefixFor _ = unsafeHumanReadablePartFromText "bls_vk"
  bech32PrefixesPermitted _ = unsafeHumanReadablePartFromText <$> ["bls_vk"]

instance SerialiseAsBech32 (SigningKey BlsKey) where
  bech32PrefixFor _ = unsafeHumanReadablePartFromText "bls_sk"
  bech32PrefixesPermitted _ = unsafeHumanReadablePartFromText <$> ["bls_sk"]

newtype instance Hash BlsKey
  = BlsKeyHash
      ( Crypto.Hash
          HASH
          (Crypto.VerKeyDSIGN Crypto.BLS12381MinSigDSIGN)
      )
  deriving stock (Eq, Ord)
  deriving (Show, Pretty) via UsingRawBytesHex (Hash BlsKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash BlsKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash BlsKey) where
  serialiseToRawBytes (BlsKeyHash vkh) =
    Crypto.hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsBlsKey) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash BlsKey") $
      BlsKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey BlsKey) where
  textEnvelopeType _ =
    "BlsVerificationKey_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Crypto.BLS12381MinSigDSIGN
    proxy = Proxy

instance HasTextEnvelope (SigningKey BlsKey) where
  textEnvelopeType _ =
    "BlsSigningKey_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Crypto.BLS12381MinSigDSIGN
    proxy = Proxy
