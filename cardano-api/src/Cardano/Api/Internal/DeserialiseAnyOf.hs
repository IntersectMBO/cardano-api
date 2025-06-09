{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Class of errors used in the Api.
module Cardano.Api.Internal.DeserialiseAnyOf
  ( InputFormat (..)
  , InputDecodeError (..)
  , deserialiseInput
  , deserialiseInputAnyOf
  , readFormattedFile
  , readFormattedFileTextEnvelope
  , readFormattedFileAnyOf
  , renderInputDecodeError
  )
where

import Cardano.Api.Internal.Error
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.IO
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseTextEnvelope

-- import Cardano.Api.Internal.Utils

import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Data
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Exts (IsList (..))
import Prettyprinter

------------------------------------------------------------------------------
-- Formatted/encoded input deserialisation
------------------------------------------------------------------------------

-- | Input format/encoding.
data InputFormat a where
  -- | Bech32 encoding.
  InputFormatBech32 :: SerialiseAsBech32 a => InputFormat a
  -- | Hex/Base16 encoding.
  InputFormatHex :: SerialiseAsRawBytes a => InputFormat a
  -- TODO: Specify TextEnvelope CBOR hex

  -- | Text envelope format.
  InputFormatTextEnvelope :: HasTextEnvelope a => InputFormat a

-- TODO: Add constructor for TextEnvelope Bech32

-- | Input decoding error.
data InputDecodeError
  = -- | The provided data seems to be a valid text envelope, but some error
    -- occurred in deserialising it.
    InputTextEnvelopeError !TextEnvelopeError
  | -- | The provided data is valid Bech32, but some error occurred in
    -- deserialising it.
    InputBech32DecodeError !Bech32DecodeError
  | -- | The provided data does not represent a valid value of the provided
    -- type.
    InputInvalidError
  deriving (Eq, Show, Data)

instance Error InputDecodeError where
  prettyError = renderInputDecodeError

-- | Render an error message for a 'InputDecodeError'.
renderInputDecodeError :: InputDecodeError -> Doc ann
renderInputDecodeError = \case
  InputTextEnvelopeError textEnvErr ->
    prettyError textEnvErr
  InputBech32DecodeError decodeErr ->
    prettyError decodeErr
  InputInvalidError ->
    "Invalid key."

-- | The result of a deserialisation function.
--
-- Note that this type isn't intended to be exported, but only used as a
-- helper within the 'deserialiseInput' function.
data DeserialiseInputResult a
  = -- | Input successfully deserialised.
    DeserialiseInputSuccess !a
  | -- | The provided data is of the expected format/encoding, but an error
    -- occurred in deserialising it.
    DeserialiseInputError !InputDecodeError
  | -- | The provided data's formatting/encoding does not match that which was
    -- expected. This error is an indication that one could attempt to
    -- deserialise the input again, but instead expecting a different format.
    DeserialiseInputErrorFormatMismatch

-- | Deserialise an input of some type that is formatted in some way.
deserialiseInput
  :: forall a
   . NonEmpty (InputFormat a)
  -> ByteString
  -> Either InputDecodeError a
deserialiseInput acceptedFormats inputBs =
  go (toList acceptedFormats)
 where
  inputText :: Text
  inputText = Text.decodeUtf8 inputBs

  go :: [InputFormat a] -> Either InputDecodeError a
  go [] = Left InputInvalidError
  go (kf : kfs) =
    let res =
          case kf of
            InputFormatBech32 -> deserialiseBech32
            InputFormatHex -> deserialiseHex
            InputFormatTextEnvelope -> deserialiseTextEnvelope
     in case res of
          DeserialiseInputSuccess a -> Right a
          DeserialiseInputError err -> Left err
          DeserialiseInputErrorFormatMismatch -> go kfs

  deserialiseTextEnvelope :: HasTextEnvelope a => DeserialiseInputResult a
  deserialiseTextEnvelope = do
    let textEnvRes :: Either TextEnvelopeError a
        textEnvRes =
          deserialiseFromTextEnvelope
            =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
    case textEnvRes of
      Right res -> DeserialiseInputSuccess res
      -- The input was valid a text envelope, but there was a type mismatch
      -- error.
      Left err@TextEnvelopeTypeError{} ->
        DeserialiseInputError (InputTextEnvelopeError err)
      -- The input was not valid a text envelope.
      Left _ -> DeserialiseInputErrorFormatMismatch

  deserialiseBech32 :: SerialiseAsBech32 a => DeserialiseInputResult a
  deserialiseBech32 =
    case deserialiseFromBech32 inputText of
      Right res -> DeserialiseInputSuccess res
      -- The input was not valid Bech32.
      Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch
      -- The input was valid Bech32, but some other error occurred.
      Left err -> DeserialiseInputError $ InputBech32DecodeError err

  deserialiseHex :: SerialiseAsRawBytes a => DeserialiseInputResult a
  deserialiseHex
    | isValidHex inputBs =
        case deserialiseFromRawBytesHex inputBs of
          Left _ -> DeserialiseInputError InputInvalidError
          Right x -> DeserialiseInputSuccess x
    | otherwise = DeserialiseInputErrorFormatMismatch

  isValidHex :: ByteString -> Bool
  isValidHex x =
    all ((`elem` hexAlpha) . toLower) (BSC.unpack x)
      && even (BSC.length x)
   where
    hexAlpha :: [Char]
    hexAlpha = "0123456789abcdef"

-- | Deserialise an input of some type that is formatted in some way.
--
-- The provided 'ByteString' can either be Bech32-encoded or in the text
-- envelope format.
deserialiseInputAnyOf
  :: forall b
   . [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> ByteString
  -> Either InputDecodeError b
deserialiseInputAnyOf bech32Types textEnvTypes inputBs =
  case deserialiseBech32 `orTry` deserialiseTextEnvelope of
    DeserialiseInputSuccess res -> Right res
    DeserialiseInputError err -> Left err
    DeserialiseInputErrorFormatMismatch -> Left InputInvalidError
 where
  inputText :: Text
  inputText = Text.decodeUtf8 inputBs

  orTry
    :: DeserialiseInputResult b
    -> DeserialiseInputResult b
    -> DeserialiseInputResult b
  orTry x y =
    case x of
      DeserialiseInputSuccess _ -> x
      DeserialiseInputError _ -> x
      DeserialiseInputErrorFormatMismatch -> y

  deserialiseTextEnvelope :: DeserialiseInputResult b
  deserialiseTextEnvelope = do
    let textEnvRes :: Either TextEnvelopeError b
        textEnvRes =
          deserialiseFromTextEnvelopeAnyOf textEnvTypes
            =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
    case textEnvRes of
      Right res -> DeserialiseInputSuccess res
      -- The input was valid a text envelope, but there was a type mismatch
      -- error.
      Left err@TextEnvelopeTypeError{} ->
        DeserialiseInputError (InputTextEnvelopeError err)
      -- The input was not valid a text envelope.
      Left _ -> DeserialiseInputErrorFormatMismatch

  deserialiseBech32 :: DeserialiseInputResult b
  deserialiseBech32 =
    case deserialiseAnyOfFromBech32 bech32Types inputText of
      Right res -> DeserialiseInputSuccess res
      -- The input was not valid Bech32.
      Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch
      -- The input was valid Bech32, but some other error occurred.
      Left err -> DeserialiseInputError $ InputBech32DecodeError err

-- | Read formatted file
readFormattedFile
  :: NonEmpty (InputFormat a)
  -- ^ one of expected input formats
  -> FilePath
  -> IO (Either (FileError InputDecodeError) a)
readFormattedFile acceptedFormats path = do
  eContent <- runExceptT $ fileIOExceptT path readFileBlocking
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError path) $ deserialiseInput acceptedFormats content

-- | Read text envelope file
readFormattedFileTextEnvelope
  :: HasTextEnvelope a
  => File content In
  -> IO (Either (FileError InputDecodeError) a)
readFormattedFileTextEnvelope fp =
  first (fmap InputTextEnvelopeError) <$> readFileTextEnvelope fp

-- | Read in in any of the format in the text envelope
readFormattedFileAnyOf
  :: forall content b
   . [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> File content In
  -> IO (Either (FileError InputDecodeError) b)
readFormattedFileAnyOf bech32Types textEnvTypes path = do
  eContent <- runExceptT $ fileIOExceptT (unFile path) readFileBlocking
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError (unFile path)) $ deserialiseInputAnyOf bech32Types textEnvTypes content
