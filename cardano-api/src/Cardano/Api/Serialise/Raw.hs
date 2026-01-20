{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Raw binary serialisation
module Cardano.Api.Serialise.Raw
  ( SerialiseAsRawBytes (..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  , parseRawBytesHex
  , RawBytesHexError (..)
  , SerialiseAsRawBytesError (..)
  )
where

import Cardano.Api.Error (Error, failEitherError, prettyError)
import Cardano.Api.HasTypeProxy
import Cardano.Api.Monad.Error (MonadError (..))
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Pretty

import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..), FiniteBits (finiteBitSize))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Data (typeRep)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Typeable (TypeRep, Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)

class (HasTypeProxy a, Typeable a) => SerialiseAsRawBytes a where
  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Either SerialiseAsRawBytesError a

instance SerialiseAsRawBytes Word8 where
  serialiseToRawBytes = BS.singleton
  deserialiseFromRawBytes AsWord8 = deserialiseWord

instance SerialiseAsRawBytes Word16 where
  serialiseToRawBytes = BS.toStrict . BSB.toLazyByteString . BSB.word16BE
  deserialiseFromRawBytes AsWord16 = deserialiseWord

instance SerialiseAsRawBytes Word32 where
  serialiseToRawBytes = BS.toStrict . BSB.toLazyByteString . BSB.word32BE
  deserialiseFromRawBytes AsWord32 = deserialiseWord

instance SerialiseAsRawBytes Word64 where
  serialiseToRawBytes = BS.toStrict . BSB.toLazyByteString . BSB.word64BE
  deserialiseFromRawBytes AsWord64 = deserialiseWord

-- | Deserialise any length number. Does not require the input to have the byte length of the target type.
deserialiseWord
  :: forall a
   . (FiniteBits a, Typeable a, Num a)
  => ByteString
  -- ^ bytes representation of the number
  -> Either SerialiseAsRawBytesError a
deserialiseWord bs
  | BS.null bs =
      throwError $
        SerialiseAsRawBytesError $
          "Cannot deserialise empty bytes into " <> typeName
  | BS.length bs > maxBytes =
      throwError $
        SerialiseAsRawBytesError $
          "Cannot decode " <> typeName <> ": Value too large (hex):" <> BSC.unpack (Base16.encode bs)
  | otherwise =
      pure $ BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0 bs
 where
  maxBytes = finiteBitSize (zeroBits @a) `div` 8
  typeName = show $ typeRep (Proxy @a)

-- | Convert the number into binary value
instance SerialiseAsRawBytes Natural where
  serialiseToRawBytes 0 = BS.singleton 0x00
  serialiseToRawBytes n = BS.toStrict . BSB.toLazyByteString $ go n mempty
   where
    go 0 acc = acc
    go x acc = go (x `shiftR` 8) (BSB.word8 (fromIntegral (x .&. 0xFF)) <> acc)
  deserialiseFromRawBytes AsNatural "\x00" = pure 0
  deserialiseFromRawBytes AsNatural input = pure $ BS.foldl' (\acc byte -> acc `shiftL` 8 .|. fromIntegral byte) 0 input

instance SerialiseAsRawBytes BS.ByteString where
  serialiseToRawBytes = id
  deserialiseFromRawBytes AsByteString = pure

instance SerialiseAsRawBytes BSL.ByteString where
  serialiseToRawBytes = BSL.toStrict
  deserialiseFromRawBytes AsByteStringLazy = pure . BSL.fromStrict

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

newtype SerialiseAsRawBytesError = SerialiseAsRawBytesError
  -- TODO We can do better than use String to carry the error message
  { unSerialiseAsRawBytesError :: String
  }
  deriving (Eq, Show)

instance Error SerialiseAsRawBytesError where
  prettyError = pshow . unSerialiseAsRawBytesError
