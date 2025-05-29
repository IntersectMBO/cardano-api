{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Raw binary serialisation
module Cardano.Api.Internal.SerialiseRaw
  ( SerialiseAsRawBytes (..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  , parseRawBytesHex
  , RawBytesHexError (..)
  , SerialiseAsRawBytesError (..)
  )
where

import Cardano.Api.Internal.Error (Error, failEitherError, prettyError)
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Pretty
import Cardano.Api.Parser.Text qualified as P

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 as BSC
import Data.Data (typeRep)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Typeable (TypeRep, Typeable)

class (HasTypeProxy a, Typeable a) => SerialiseAsRawBytes a where
  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Either SerialiseAsRawBytesError a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

deserialiseFromRawBytesHex
  :: forall a
   . SerialiseAsRawBytes a
  => ByteString -> Either RawBytesHexError a
deserialiseFromRawBytesHex hex = do
  let type' = typeRep $ asType @a
  raw <- first (RawBytesHexErrorBase16DecodeFail hex type') $ Base16.decode hex
  first (RawBytesHexErrorRawBytesDecodeFail hex type') $
    deserialiseFromRawBytes asType raw

-- | Parse hex representation of a value
parseRawBytesHex :: SerialiseAsRawBytes a => P.Parser a
parseRawBytesHex = do
  input <- P.many P.hexDigit
  failEitherError . deserialiseFromRawBytesHex $ BSC.pack input

-- | The errors that the pure 'SerialiseAsRawBytes' parsing\/decoding functions can return.
data RawBytesHexError
  = RawBytesHexErrorBase16DecodeFail
      ByteString
      -- ^ original input
      TypeRep
      -- ^ expected type
      String
      -- ^ error message
  | RawBytesHexErrorRawBytesDecodeFail
      ByteString
      -- ^ original input
      TypeRep
      -- ^ expected type
      SerialiseAsRawBytesError
      -- ^ error message
  deriving Show

newtype SerialiseAsRawBytesError = SerialiseAsRawBytesError
  -- TODO We can do better than use String to carry the error message
  { unSerialiseAsRawBytesError :: String
  }
  deriving (Eq, Show)

instance Error RawBytesHexError where
  prettyError = \case
    RawBytesHexErrorBase16DecodeFail input typeRep' message ->
      "Failed to deserialise "
        <> pshow typeRep'
        <> ". Expected Base16-encoded bytestring, but got "
        <> pretty (toText input)
        <> "; "
        <> pretty message
    RawBytesHexErrorRawBytesDecodeFail input typeRep' (SerialiseAsRawBytesError e) ->
      "Failed to deserialise " <> pretty (toText input) <> " as " <> pshow typeRep' <> ": " <> pretty e
   where
    toText bs = case Text.decodeUtf8' bs of
      Right t -> Text.unpack t
      Left _ -> show bs
