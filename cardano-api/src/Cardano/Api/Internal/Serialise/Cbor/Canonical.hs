{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Internal.Serialise.Cbor.Canonical
  ( canonicaliseCborBs
  , canonicaliseTerm
  )
where

import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Serialise.Cbor

import Cardano.Binary (DecoderError (..))

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term
  ( Term (..)
  , decodeTerm
  , encodeTerm
  )
import Codec.CBOR.Write
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort, sortOn)
import Data.Tuple.Extra (both)

instance HasTypeProxy Term where
  data AsType Term = AsTerm
  proxyToAsType _ = AsTerm

instance SerialiseAsCBOR Term where
  serialiseToCBOR = LBS.toStrict . BSB.toLazyByteString . toBuilder . encodeTerm
  deserialiseFromCBOR _proxy = decodeTermFromBs . LBS.fromStrict

decodeTermFromBs
  :: LBS.ByteString
  -> Either DecoderError Term
decodeTermFromBs input = do
  (leftover, result) <-
    first (DecoderErrorDeserialiseFailure "Cannot decode Term") $
      deserialiseFromBytes decodeTerm input
  unless (LBS.null leftover) $ do
    throwError $
      DecoderErrorLeftover "Invalid CBOR: some bytes were not consumed" (LBS.toStrict leftover)
  pure result

canonicaliseCborBs :: BS.ByteString -> Either DecoderError BS.ByteString
canonicaliseCborBs originalCborBytes = (serialiseToCBOR . canonicaliseTerm) <$> deserialiseFromCBOR AsTerm originalCborBytes

canonicaliseTerm :: Term -> Term
canonicaliseTerm (TMap termPairs) = TMap . sortOn fst $ map (both canonicaliseTerm) termPairs
canonicaliseTerm (TMapI termPairs) = TMapI . sortOn fst $ map (both canonicaliseTerm) termPairs
canonicaliseTerm (TList terms) = TList . sort $ map canonicaliseTerm terms
canonicaliseTerm (TListI terms) = TListI . sort $ map canonicaliseTerm terms
canonicaliseTerm (TTagged tag term) = TTagged tag $ canonicaliseTerm term
canonicaliseTerm term = term
