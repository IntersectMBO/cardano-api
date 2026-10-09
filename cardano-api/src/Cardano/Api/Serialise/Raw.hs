{-# OPTIONS_GHC -Wno-orphans #-}

-- | Raw binary serialisation
--
-- The types and functions of this module now live in the cardano-keys package;
-- re-exported here for compatibility, together with the 'Error' instances and
-- the parser this API adds on top of them.
module Cardano.Api.Serialise.Raw
  ( module Cardano.Keys.Serialise.Raw
  , parseRawBytesHex
  )
where

import Cardano.Api.Error (Error (..), failEitherError)
import Cardano.Api.Parser.Text qualified as P

import Cardano.Keys.Serialise.Raw

import Data.ByteString.Char8 qualified as BSC

-- | Parse hex representation of a value
parseRawBytesHex :: SerialiseAsRawBytes a => P.Parser a
parseRawBytesHex = do
  input <- P.many P.hexDigit
  failEitherError . deserialiseFromRawBytesHex $ BSC.pack input

instance Error RawBytesHexError where
  prettyError = renderRawBytesHexError

instance Error SerialiseAsRawBytesError where
  prettyError = renderSerialiseAsRawBytesError
