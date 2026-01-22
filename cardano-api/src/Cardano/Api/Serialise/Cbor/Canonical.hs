{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Serialise.Cbor.Canonical
  ( canonicaliseCborBs
  , canonicaliseTerm
  )
where

import Cardano.Api.HasTypeProxy
import Cardano.Api.Serialise.Cbor

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term
  ( Term (..)
  , decodeTerm
  , encodeTerm
  )
import Codec.CBOR.Write (toBuilder)
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy)
import Data.Tuple.Extra (both)

-- | This function implements CBOR canonicalisation (RFC 7049):
--
-- * Map keys are sorted lexicographically
-- * Indefinite-length maps/lists are converted to finite-length maps/lists
-- * The representation of the CBOR major types is as small as possible (provided by "cborg" package)
--
-- This function implements only CBOR canonicalisation from CIP-21. Other requirements from CIP-21 are not implemented.
--
-- 1. CBOR RFC 7049, Canonicalisation description: https://datatracker.ietf.org/doc/html/rfc7049#section-3.9
-- 2. CIP-21: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0021/README.md#canonical-cbor-serialization-format
canonicaliseCborBs :: BS.ByteString -> Either DecoderError BS.ByteString
canonicaliseCborBs originalCborBytes = serialiseToCBOR . canonicaliseTerm <$> deserialiseFromCBOR AsTerm originalCborBytes

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

-- | This function implements CBOR canonicalisation at the Term level:
--
-- * Map keys are sorted lexicographically
-- * Indefinite-length maps/lists are converted to finite-length maps/lists
canonicaliseTerm :: Term -> Term
canonicaliseTerm = \case
  (TMap termPairs) ->
    TMap . sortBy compareKeyTerms $ map (both canonicaliseTerm) termPairs
  (TMapI termPairs) ->
    TMap . sortBy compareKeyTerms $ map (both canonicaliseTerm) termPairs
  (TTagged tag term) ->
    TTagged tag $ canonicaliseTerm term
  (TListI terms) ->
    TList $ map canonicaliseTerm terms
  (TList terms) ->
    TList $ map canonicaliseTerm terms
  term -> term

-- | Implements sorting of CBOR terms for canonicalisation. CBOR terms are compared by lexical order of their
-- bytes representation. We are only sorting the keys of the map here.
-- See: https://datatracker.ietf.org/doc/html/rfc7049#section-3.9
compareKeyTerms
  :: (Term, a)
  -- ^ (key, value) from a map
  -> (Term, a)
  -> Ordering
compareKeyTerms (t1, _) (t2, _) = compare (serialiseToCBOR t1) (serialiseToCBOR t2)

instance HasTypeProxy Term where
  data AsType Term = AsTerm
  proxyToAsType _ = AsTerm

instance SerialiseAsCBOR Term where
  serialiseToCBOR = LBS.toStrict . BSB.toLazyByteString . toBuilder . encodeTerm
  deserialiseFromCBOR _proxy = decodeTermFromBs . LBS.fromStrict
