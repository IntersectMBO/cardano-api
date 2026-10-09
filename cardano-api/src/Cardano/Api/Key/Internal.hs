{-# LANGUAGE FlexibleContexts #-}

-- | Shelley key types and their 'Key' class instances
--
-- The key types of this module now live in the cardano-keys package;
-- re-exported here for compatibility, together with the parser this API adds
-- on top of them.
module Cardano.Api.Key.Internal
  ( module Cardano.Keys.Shelley
  , parseHexHash
  )
where

import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Serialise.Raw (SerialiseAsRawBytes, parseRawBytesHex)

import Cardano.Keys.Shelley

-- | Parse hex representation of any 'Hash'
parseHexHash :: SerialiseAsRawBytes (Hash a) => P.Parser (Hash a)
parseHexHash = parseRawBytesHex
