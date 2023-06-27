{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | DRep off-chain metadata
--
module Cardano.Api.DRepMetadata (
    -- * DRep off-chain metadata
    DRepMetadata(..),
    validateAndHashDRepMetadata,
    DRepMetadataValidationError(..),

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Script
import           Cardano.Api.SerialiseRaw

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Either.Combinators (maybeToRight)

-- ----------------------------------------------------------------------------
-- DRep metadata
--

-- | A representation of the required fields for off-chain drep metadata.
--
newtype DRepMetadata = DRepMetadata
  { unDRepMetadata :: ByteString
  } deriving (Eq, Show)

newtype instance Hash DRepMetadata = DRepMetadataHash (Shelley.Hash StandardCrypto ByteString)
  deriving (Eq, Show)

instance HasTypeProxy DRepMetadata where
  data AsType DRepMetadata = AsDRepMetadata
  proxyToAsType _ = AsDRepMetadata

instance SerialiseAsRawBytes (Hash DRepMetadata) where
  serialiseToRawBytes (DRepMetadataHash h) = Crypto.hashToBytes h

  deserialiseFromRawBytes (AsHash AsDRepMetadata) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash DRepMetadata") $
      DRepMetadataHash <$> Crypto.hashFromBytes bs

-- | A drep metadata validation error.
data DRepMetadataValidationError
  = DRepMetadataInvalidLengthError
    -- ^ The length of the JSON-encoded drep metadata exceeds the
    -- maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  deriving Show

instance Error DRepMetadataValidationError where
  displayError = \case
    DRepMetadataInvalidLengthError maxLen actualLen ->
      mconcat
        [ "DRep metadata must consist of at most "
        , show maxLen
        , " bytes, but it consists of "
        , show actualLen
        , " bytes."
        ]

-- | Decode and validate the provided JSON-encoded bytes as 'DRepMetadata'.
-- Return the decoded metadata and the hash of the original bytes.
validateAndHashDRepMetadata
  :: ByteString
  -> Either DRepMetadataValidationError (DRepMetadata, Hash DRepMetadata)
validateAndHashDRepMetadata bs
  -- TODO confirm if there are size limits to the DRep metadata
  | BS.length bs <= 512 = do
      let md = DRepMetadata bs
      let mdh = DRepMetadataHash (Crypto.hashWith id bs)
      return (md, mdh)
  | otherwise = Left $ DRepMetadataInvalidLengthError 512 (BS.length bs)
