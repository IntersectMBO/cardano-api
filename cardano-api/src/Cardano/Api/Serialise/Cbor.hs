{-# LANGUAGE DefaultSignatures #-}

-- | CBOR serialisation
module Cardano.Api.Serialise.Cbor
  ( SerialiseAsCBOR (..)
  , FromCBOR (..)
  , ToCBOR (..)
  )
where

import Cardano.Api.HasTypeProxy

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Binary qualified as CBOR

import Data.ByteString (ByteString)

class HasTypeProxy a => SerialiseAsCBOR a where
  serialiseToCBOR :: a -> ByteString
  deserialiseFromCBOR :: AsType a -> ByteString -> Either CBOR.DecoderError a

  default serialiseToCBOR :: ToCBOR a => a -> ByteString
  serialiseToCBOR = CBOR.serialize'

  default deserialiseFromCBOR
    :: FromCBOR a
    => AsType a
    -> ByteString
    -> Either CBOR.DecoderError a
  deserialiseFromCBOR _proxy = CBOR.decodeFull'
