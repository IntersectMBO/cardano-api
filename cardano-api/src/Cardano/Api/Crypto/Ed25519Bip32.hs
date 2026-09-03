{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | BIP32-Ed25519 digital signatures.
module Cardano.Api.Crypto.Ed25519Bip32
  ( Ed25519Bip32DSIGN
  , SigDSIGN (..)
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)

    -- * Serialisation
  , xPrvToBytes
  , xPrvFromBytes
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary.FixedSizeCodec (FixedSizeCodec (..), decodeFixedSized, encodeFixedSized)
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Seed
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Crypto.Wallet qualified as CC

import Control.DeepSeq (NFData)
import Data.ByteArray as BA (ByteArrayAccess, convert, length, withByteArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)

import Crypto.ECC.Edwards25519 qualified as Ed25519
import Crypto.Error (eitherCryptoError)
import NoThunks.Class (InspectHeap (..), NoThunks)

data Ed25519Bip32DSIGN

instance DSIGNAlgorithm Ed25519Bip32DSIGN where
  type SeedSizeDSIGN Ed25519Bip32DSIGN = 32

  -- \| BIP32-Ed25519 extended verification key size is 64 octets.
  type VerKeySizeDSIGN Ed25519Bip32DSIGN = 64

  -- \| BIP32-Ed25519 extended signing key size is 96 octets.
  type SignKeySizeDSIGN Ed25519Bip32DSIGN = 96

  -- \| BIP32-Ed25519 extended signature size is 64 octets.
  type SigSizeDSIGN Ed25519Bip32DSIGN = 64

  --
  -- Key and signature types
  --

  newtype VerKeyDSIGN Ed25519Bip32DSIGN = VerKeyEd25519Bip32DSIGN CC.XPub
    deriving (Show, Eq, Generic)
    deriving newtype NFData
    deriving NoThunks via InspectHeap CC.XPub

  newtype SignKeyDSIGN Ed25519Bip32DSIGN = SignKeyEd25519Bip32DSIGN CC.XPrv
    deriving Generic
    deriving newtype NFData
    deriving NoThunks via InspectHeap CC.XPrv

  newtype SigDSIGN Ed25519Bip32DSIGN = SigEd25519Bip32DSIGN CC.XSignature
    deriving (Show, Eq, Generic)
    deriving NoThunks via InspectHeap CC.XSignature

  --
  -- Metadata and basic key operations
  --

  algorithmNameDSIGN _ = "ed25519_bip32"

  deriveVerKeyDSIGN (SignKeyEd25519Bip32DSIGN sk) =
    VerKeyEd25519Bip32DSIGN $ CC.toXPub sk

  --
  -- Core algorithm operations
  --

  type Signable Ed25519Bip32DSIGN = SignableRepresentation

  signDSIGN () a (SignKeyEd25519Bip32DSIGN sk) =
    SigEd25519Bip32DSIGN $
      CC.sign (mempty :: ByteString) sk (getSignableRepresentation a)

  verifyDSIGN () (VerKeyEd25519Bip32DSIGN vk) a (SigEd25519Bip32DSIGN sig) =
    if CC.verify vk (getSignableRepresentation a) sig
      then Right ()
      else Left "Verification failed"

  --
  -- Key generation
  --

  genKeyDSIGN seed =
    SignKeyEd25519Bip32DSIGN $
      CC.generateNew
        (getSeedBytes seed)
        (mempty :: ByteString)
        (mempty :: ByteString)

instance ByteArrayAccess (SignKeyDSIGN Ed25519Bip32DSIGN) where
  length (SignKeyEd25519Bip32DSIGN sk) = BA.length (CC.unXPrv sk)
  withByteArray (SignKeyEd25519Bip32DSIGN sk) = BA.withByteArray (CC.unXPrv sk)

instance ByteArrayAccess (SigDSIGN Ed25519Bip32DSIGN) where
  length (SigEd25519Bip32DSIGN sig) = BA.length (CC.unXSignature sig)
  withByteArray (SigEd25519Bip32DSIGN sig) = BA.withByteArray (CC.unXSignature sig)

instance FixedSizeCodec (VerKeyDSIGN Ed25519Bip32DSIGN) where
  type FixedSize (VerKeyDSIGN Ed25519Bip32DSIGN) = 64
  rawEncodeFixedSized (VerKeyEd25519Bip32DSIGN vk) = CC.unXPub vk
  rawDecodeFixedSized bs =
    either fail (pure . VerKeyEd25519Bip32DSIGN) (CC.xpub bs)

instance FixedSizeCodec (SignKeyDSIGN Ed25519Bip32DSIGN) where
  type FixedSize (SignKeyDSIGN Ed25519Bip32DSIGN) = 96
  rawEncodeFixedSized (SignKeyEd25519Bip32DSIGN sk) = xPrvToBytes sk
  rawDecodeFixedSized bs =
    maybe (fail "invalid Ed25519Bip32DSIGN signing key") (pure . SignKeyEd25519Bip32DSIGN) $
      xPrvFromBytes bs

instance FixedSizeCodec (SigDSIGN Ed25519Bip32DSIGN) where
  type FixedSize (SigDSIGN Ed25519Bip32DSIGN) = 64
  rawEncodeFixedSized = BA.convert
  rawDecodeFixedSized bs =
    either fail (pure . SigEd25519Bip32DSIGN) (CC.xsignature bs)

instance Show (SignKeyDSIGN Ed25519Bip32DSIGN) where
  show (SignKeyEd25519Bip32DSIGN sk) = show $ xPrvToBytes sk

instance ToCBOR (VerKeyDSIGN Ed25519Bip32DSIGN) where
  toCBOR = encodeFixedSized
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance FromCBOR (VerKeyDSIGN Ed25519Bip32DSIGN) where
  fromCBOR = decodeFixedSized

instance ToCBOR (SignKeyDSIGN Ed25519Bip32DSIGN) where
  toCBOR = encodeFixedSized
  encodedSizeExpr _ = encodedSignKeyDSIGNSizeExpr

instance FromCBOR (SignKeyDSIGN Ed25519Bip32DSIGN) where
  fromCBOR = decodeFixedSized

instance ToCBOR (SigDSIGN Ed25519Bip32DSIGN) where
  toCBOR = encodeFixedSized
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance FromCBOR (SigDSIGN Ed25519Bip32DSIGN) where
  fromCBOR = decodeFixedSized

-- | Serialise an 'CC.XPrv' to a 'ByteString' (96 bytes).
--
-- In @cardano-crypto@, an 'CC.XPrv' was originally serialised using the
-- following 128-byte binary format:
--
-- +---------------------------------+-----------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Public Key (32 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+-----------------------+
--
-- However, this function serialises an 'CC.XPrv' using a more compact 96-byte
-- binary format:
--
-- +---------------------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+
xPrvToBytes :: CC.XPrv -> ByteString
xPrvToBytes xPrv = privateKeyBytes <> chainCodeBytes
 where
  privateKeyBytes :: ByteString
  privateKeyBytes = BS.take 64 (CC.unXPrv xPrv)

  chainCodeBytes :: ByteString
  chainCodeBytes = BS.drop 96 (CC.unXPrv xPrv)

-- | Deserialise an 'CC.XPrv' from a 'ByteString' (96 bytes).
--
-- In @cardano-crypto@, an 'CC.XPrv' was originally deserialised using the
-- following 128-byte binary format:
--
-- +---------------------------------+-----------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Public Key (32 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+-----------------------+
--
-- However, this function deserialises an 'CC.XPrv' using a more compact
-- 96-byte binary format:
--
-- +---------------------------------+-----------------------+
-- | Extended Private Key (64 bytes) | Chain Code (32 bytes) |
-- +---------------------------------+-----------------------+
xPrvFromBytes :: ByteString -> Maybe CC.XPrv
xPrvFromBytes bytes
  | BS.length bytes /= 96 = Nothing
  | otherwise = do
      let (prv, cc) = BS.splitAt 64 bytes
      pub <- ed25519ScalarMult (BS.take 32 prv)
      eitherToMaybe $ CC.xprv $ prv <> pub <> cc
 where
  eitherToMaybe :: Either a b -> Maybe b
  eitherToMaybe = either (const Nothing) Just

  ed25519ScalarMult :: ByteString -> Maybe ByteString
  ed25519ScalarMult bs = do
    scalar <- eitherToMaybe . eitherCryptoError $ Ed25519.scalarDecodeLong bs
    pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar
