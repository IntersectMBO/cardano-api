{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Raw binary serialisation
module Cardano.Api.Internal.SerialiseRaw
  ( RawBytesHexError (..)
  , SerialiseAsRawBytes (..)
  , SerialiseAsRawBytesError (..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  )
where

import Cardano.Api.Internal.Error (Error, prettyError)
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Pretty

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Data (typeRep)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Typeable (TypeRep, Typeable)

newtype SerialiseAsRawBytesError = SerialiseAsRawBytesError
  -- TODO We can do better than use String to carry the error message
  { unSerialiseAsRawBytesError :: String
  }
  deriving (Eq, Show)

class (HasTypeProxy a, Typeable a) => SerialiseAsRawBytes a where
  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Either SerialiseAsRawBytesError a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

-- | The errors that the pure 'SerialiseAsRawBytes' parsing\/decoding functions can return.
data RawBytesHexError
  = RawBytesHexErrorBase16DecodeFail
      ByteString
      -- ^ original input
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

instance Error RawBytesHexError where
  prettyError = \case
    RawBytesHexErrorBase16DecodeFail input message ->
      "Expected Base16-encoded bytestring, but got "
        <> pretty (toText input)
        <> "; "
        <> pretty message
    RawBytesHexErrorRawBytesDecodeFail input typeRep' (SerialiseAsRawBytesError e) ->
      "Failed to deserialise " <> pretty (toText input) <> " as " <> pshow typeRep' <> ". " <> pretty e
   where
    toText bs = case Text.decodeUtf8' bs of
      Right t -> Text.unpack t
      Left _ -> show bs

deserialiseFromRawBytesHex
  :: forall a
   . SerialiseAsRawBytes a
  => ByteString -> Either RawBytesHexError a
deserialiseFromRawBytesHex hex = do
  raw <- first (RawBytesHexErrorBase16DecodeFail hex) $ Base16.decode hex
  case deserialiseFromRawBytes asType raw of
    Left e -> Left $ RawBytesHexErrorRawBytesDecodeFail hex (typeRep $ asType @a) e
    Right a -> Right a
