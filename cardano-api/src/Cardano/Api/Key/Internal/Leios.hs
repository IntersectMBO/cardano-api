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

    -- * Possession proof
  , BlsPossessionProof
  , blsPossessionProof
  , createBlsPossessionProof
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

import Data.ByteString (ByteString)
import Data.Either.Combinators (maybeToRight)
import Data.String (IsString (..))

-- | BLS keys. To participate in the Leios protocol as voting member/block producing node, stake pool operators must
-- register one additional cryptographic key for the voting scheme alongside their existing VRF and KES keys.
-- In this implementation, the BLS key is over the BLS12-381 elliptic curve.
--
-- The reason we use BLS keys for the voting scheme of Leios is that they support signature aggregation, which allows
-- multiple signature to be combined resulting in a single signature that is compact.
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
  textEnvelopeType :: AsType (VerificationKey BlsKey) -> TextEnvelopeType
  textEnvelopeType _ =
    "BlsVerificationKey_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Crypto.BLS12381MinSigDSIGN
    proxy = Proxy

  textEnvelopeDefaultDescr :: VerificationKey BlsKey -> TextEnvelopeDescr
  textEnvelopeDefaultDescr _ = "BLS12-381 verification key"

instance HasTextEnvelope (SigningKey BlsKey) where
  textEnvelopeType :: AsType (SigningKey BlsKey) -> TextEnvelopeType
  textEnvelopeType _ =
    "BlsSigningKey_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Crypto.BLS12381MinSigDSIGN
    proxy = Proxy

  textEnvelopeDefaultDescr :: SigningKey BlsKey -> TextEnvelopeDescr
  textEnvelopeDefaultDescr _ = "BLS12-381 signing key"

-- | BlsPossessionProof is used in the Leios protocol to prove ownership of a BLS signing key
-- when registering a BLS verification key for a stake pool. This is required to prevent malicious
-- actors from registering a BLS verification key for a stake pool without actually owning the
-- corresponding signing key.
newtype BlsPossessionProof = BlsPossessionProof (Crypto.PossessionProofDSIGN Crypto.BLS12381MinSigDSIGN)
  deriving stock Eq
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass SerialiseAsCBOR

instance Show BlsPossessionProof where
  show p = "blsPossessionProof " ++ show (serialiseToRawBytesHex p)

instance Pretty BlsPossessionProof where
  pretty p = "blsPossessionProof" <+> pretty (serialiseToRawBytesHexText p)

instance SerialiseAsRawBytes BlsPossessionProof where
  serialiseToRawBytes (BlsPossessionProof proof) =
    Crypto.rawSerialisePossessionProofDSIGN proof

  deserialiseFromRawBytes AsBlsPossessionProof bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise BlsPossessionProof") $
      BlsPossessionProof <$> Crypto.rawDeserialisePossessionProofDSIGN bs

-- | Construct a 'BlsPossessionProof' from a hex-encoded raw 'ByteString'.
--
-- This is a partial function that calls 'error' if the input is not valid.
-- It is intended to be used with the output of 'show' or 'pretty' to
-- reconstruct a 'BlsPossessionProof' value.
blsPossessionProof :: ByteString -> BlsPossessionProof
blsPossessionProof hexBs =
  case deserialiseFromRawBytesHex hexBs of
    Left e -> error $ "blsPossessionProof: " ++ show e
    Right p -> p

-- | Signing context including the Domain Separation Tag (DST) for the proofs-of-possession of
-- BLS keys using the minimal-signature-size BLS12-381 variant.
--
-- A Domain Separation Tag is a unique tag (like a magic number) that we add to ensure that
-- the signature is used only in the context that it was intended for.
-- This is because BLS keys and signatures can be used for multiple purposes, and
-- we don't want a proof of possession for one purpose to be interpreted as something different
-- in a different context.
minSigPoPContext :: Crypto.BLS12381SignContext
minSigPoPContext = Crypto.BLS12381SignContext (Just minSigPoPDST) Nothing

-- TODO: This is a provisional definition. Import @minSigPoPDST@ from
-- @Cardano.Crypto.DSIGN.BLS12381@ (cardano-crypto-class) when
-- IntersectMBO/cardano-base#635 is merged and the dependency is bumped.
minSigPoPDST :: ByteString
minSigPoPDST = "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_POP_"

-- | Create a proof of possession for a BLS signing key.
--
-- This proof demonstrates that the holder of a BLS verification key knows the corresponding
-- secret key, which is required before the key can safely participate in signature aggregation.
-- Without this proof, an attacker could register a crafted verification key that cancels out
-- honest participants' keys during aggregation (a rogue key attack).
createBlsPossessionProof :: SigningKey BlsKey -> BlsPossessionProof
createBlsPossessionProof (BlsSigningKey sk) =
  BlsPossessionProof (Crypto.createPossessionProofDSIGN minSigPoPContext sk)

instance HasTypeProxy BlsPossessionProof where
  data AsType BlsPossessionProof = AsBlsPossessionProof
  proxyToAsType _ = AsBlsPossessionProof

instance HasTextEnvelope BlsPossessionProof where
  textEnvelopeType :: AsType BlsPossessionProof -> TextEnvelopeType
  textEnvelopeType _ =
    "BlsPossessionProof_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Crypto.BLS12381MinSigDSIGN
    proxy = Proxy

  textEnvelopeDefaultDescr :: BlsPossessionProof -> TextEnvelopeDescr
  textEnvelopeDefaultDescr _ = "BLS12-381 possession proof"
