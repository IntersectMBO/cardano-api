{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | DRep off-chain metadata
module Cardano.Api.Internal.DRepMetadata
  ( -- * DRep off-chain metadata
    DRepMetadata (..)
  , hashDRepMetadata

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  )
where

import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Hash
import Cardano.Api.Internal.Keys.Byron
import Cardano.Api.Internal.Keys.Praos
import Cardano.Api.Internal.Script
import Cardano.Api.Internal.SerialiseRaw

import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys qualified as Shelley

import Data.ByteString (ByteString)
import Data.Either.Combinators (maybeToRight)

-- ----------------------------------------------------------------------------
-- DRep metadata
--

-- | A representation of the required fields for off-chain drep metadata.
newtype DRepMetadata = DRepMetadata
  { unDRepMetadata :: ByteString
  }
  deriving (Eq, Show)

newtype instance Hash DRepMetadata = DRepMetadataHash (Shelley.Hash StandardCrypto ByteString)
  deriving (Eq, Show)

instance HasTypeProxy DRepMetadata where
  data AsType DRepMetadata = AsDRepMetadata
  proxyToAsType :: Proxy DRepMetadata -> AsType DRepMetadata
  proxyToAsType _ = AsDRepMetadata

instance SerialiseAsRawBytes (Hash DRepMetadata) where
  serialiseToRawBytes :: Hash DRepMetadata -> ByteString
  serialiseToRawBytes (DRepMetadataHash h) = Crypto.hashToBytes h

  deserialiseFromRawBytes
    :: AsType (Hash DRepMetadata) -> ByteString -> Either SerialiseAsRawBytesError (Hash DRepMetadata)
  deserialiseFromRawBytes (AsHash AsDRepMetadata) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash DRepMetadata") $
      DRepMetadataHash <$> Crypto.hashFromBytes bs

-- | Return the decoded metadata and the hash of the original bytes.
hashDRepMetadata
  :: ByteString
  -> (DRepMetadata, Hash DRepMetadata)
hashDRepMetadata bs =
  let md = DRepMetadata bs
      mdh = DRepMetadataHash (Crypto.hashWith id bs)
   in (md, mdh)
