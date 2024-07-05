{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | DRep off-chain metadata
module Cardano.Api.DRepMetadata
  ( -- * DRep off-chain metadata
    DRepMetadata (..)
  , hashDRepMetadata

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  )
where

import Cardano.Api.Eras
import Cardano.Api.HasTypeProxy
import Cardano.Api.Hash
import Cardano.Api.Keys.Byron
import Cardano.Api.Keys.Praos
import Cardano.Api.Script
import Cardano.Api.SerialiseRaw
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley
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
  proxyToAsType _ = AsDRepMetadata

instance SerialiseAsRawBytes (Hash DRepMetadata) where
  serialiseToRawBytes (DRepMetadataHash h) = Crypto.hashToBytes h

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
