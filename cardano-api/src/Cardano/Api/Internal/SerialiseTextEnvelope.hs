{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | TextEnvelope Serialisation
module Cardano.Api.Internal.SerialiseTextEnvelope
  ( HasTextEnvelope (..)
  , textEnvelopeTypeInEra
  , TextEnvelope (..)
  , TextEnvelopeType (..)
  , TextEnvelopeDescr (..)
  , textEnvelopeRawCBOR
  , TextEnvelopeError (..)
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile
  , textEnvelopeToJSON
  , serialiseTextEnvelope
  , legacyComparison

    -- * Reading one of several key types
  , FromSomeType (..)
  , deserialiseFromTextEnvelopeAnyOf
  , readFileTextEnvelopeAnyOf

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.Error
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.IO
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Internal.Pretty
import Cardano.Api.Internal.Serialise.Cbor
import Cardano.Api.Internal.Utils (readFileBlocking)

import Cardano.Binary (DecoderError)

import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Data)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

-- ----------------------------------------------------------------------------
-- Text envelopes
--

newtype TextEnvelopeType = TextEnvelopeType String
  deriving (Eq, Show, Data)
  deriving newtype (IsString, Semigroup, ToJSON, FromJSON)

newtype TextEnvelopeDescr = TextEnvelopeDescr String
  deriving (Eq, Show, Data)
  deriving newtype (IsString, Semigroup, ToJSON, FromJSON)

-- | A 'TextEnvelope' is a structured envelope for serialised binary values
-- with an external format with a semi-readable textual format.
--
-- It contains a \"type\" field, e.g. \"PublicKeyByron\" or \"TxSignedShelley\"
-- to indicate the type of the encoded data. This is used as a sanity check
-- and to help readers.
--
-- It also contains a \"title\" field which is free-form, and could be used
-- to indicate the role or purpose to a reader.
data TextEnvelope = TextEnvelope
  { teType :: !TextEnvelopeType
  , teDescription :: !TextEnvelopeDescr
  , teRawCBOR :: !ByteString
  }
  deriving (Eq, Show)

instance HasTypeProxy TextEnvelope where
  data AsType TextEnvelope = AsTextEnvelope
  proxyToAsType _ = AsTextEnvelope

instance ToJSON TextEnvelope where
  toJSON TextEnvelope{teType, teDescription, teRawCBOR} =
    object
      [ "type" .= teType
      , "description" .= teDescription
      , "cborHex" .= Text.decodeUtf8 (Base16.encode teRawCBOR)
      ]

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" $ \v ->
    TextEnvelope
      <$> (v .: "type")
      <*> (v .: "description")
      <*> (parseJSONBase16 =<< v .: "cborHex")
   where
    parseJSONBase16 v =
      either fail return . Base16.decode . Text.encodeUtf8 =<< parseJSON v

textEnvelopeJsonConfig :: Config
textEnvelopeJsonConfig = defConfig{confCompare = textEnvelopeJsonKeyOrder}

textEnvelopeJsonKeyOrder :: Text -> Text -> Ordering
textEnvelopeJsonKeyOrder = keyOrder ["type", "description", "cborHex"]

textEnvelopeRawCBOR :: TextEnvelope -> ByteString
textEnvelopeRawCBOR = teRawCBOR

-- | The errors that the pure 'TextEnvelope' parsing\/decoding functions can return.
data TextEnvelopeError
  = -- | expected, actual
    TextEnvelopeTypeError ![TextEnvelopeType] !TextEnvelopeType
  | TextEnvelopeDecodeError !DecoderError
  | TextEnvelopeAesonDecodeError !String
  deriving (Eq, Show, Data)

instance Error TextEnvelopeError where
  prettyError = \case
    TextEnvelopeTypeError [TextEnvelopeType expType] (TextEnvelopeType actType) ->
      mconcat
        [ "TextEnvelope type error: "
        , " Expected: " <> pretty expType
        , " Actual: " <> pretty actType
        ]
    TextEnvelopeTypeError expTypes (TextEnvelopeType actType) ->
      mconcat
        [ "TextEnvelope type error: "
        , " Expected one of: "
        , mconcat $ List.intersperse ", " [pretty expType | TextEnvelopeType expType <- expTypes]
        , " Actual: " <> pretty actType
        ]
    TextEnvelopeAesonDecodeError decErr ->
      "TextEnvelope aeson decode error: " <> pretty decErr
    TextEnvelopeDecodeError decErr ->
      "TextEnvelope decode error: " <> pshow decErr

-- | Check that the \"type\" of the 'TextEnvelope' is as expected.
--
-- For example, one might check that the type is \"TxSignedShelley\".
expectTextEnvelopeOfType :: TextEnvelopeType -> TextEnvelope -> Either TextEnvelopeError ()
expectTextEnvelopeOfType expectedType TextEnvelope{teType = actualType} =
  unless (expectedType `legacyComparison` actualType) $
    Left (TextEnvelopeTypeError [expectedType] actualType)

-- | This is a backwards-compatibility patch to ensure that old envelopes
-- generated by 'serialiseTxLedgerCddl' can be deserialised after switching
-- to the 'serialiseToTextEnvelope'.
legacyComparison :: TextEnvelopeType -> TextEnvelopeType -> Bool
legacyComparison (TextEnvelopeType expectedType) (TextEnvelopeType actualType) =
  case (expectedType, actualType) of
    ("TxSignedShelley", "Witnessed Tx ShelleyEra") -> True
    ("Tx AllegraEra", "Witnessed Tx AllegraEra") -> True
    ("Tx MaryEra", "Witnessed Tx MaryEra") -> True
    ("Tx AlonzoEra", "Witnessed Tx AlonzoEra") -> True
    ("Tx BabbageEra", "Witnessed Tx BabbageEra") -> True
    ("Tx ConwayEra", "Witnessed Tx ConwayEra") -> True
    ("TxSignedShelley", "Unwitnessed Tx ShelleyEra") -> True
    ("Tx AllegraEra", "Unwitnessed Tx AllegraEra") -> True
    ("Tx MaryEra", "Unwitnessed Tx MaryEra") -> True
    ("Tx AlonzoEra", "Unwitnessed Tx AlonzoEra") -> True
    ("Tx BabbageEra", "Unwitnessed Tx BabbageEra") -> True
    ("Tx ConwayEra", "Unwitnessed Tx ConwayEra") -> True
    (expectedOther, expectedActual) -> expectedOther == expectedActual

-- ----------------------------------------------------------------------------
-- Serialisation in text envelope format
--

class SerialiseAsCBOR a => HasTextEnvelope a where
  textEnvelopeType :: AsType a -> TextEnvelopeType

  textEnvelopeDefaultDescr :: a -> TextEnvelopeDescr
  textEnvelopeDefaultDescr _ = ""

textEnvelopeTypeInEra
  :: ()
  => HasTextEnvelope (f era)
  => CardanoEra era
  -> AsType (f era)
  -> TextEnvelopeType
textEnvelopeTypeInEra _ =
  textEnvelopeType

serialiseToTextEnvelope
  :: forall a
   . HasTextEnvelope a
  => Maybe TextEnvelopeDescr -> a -> TextEnvelope
serialiseToTextEnvelope mbDescr a =
  TextEnvelope
    { teType = textEnvelopeType ttoken
    , teDescription = fromMaybe (textEnvelopeDefaultDescr a) mbDescr
    , teRawCBOR = serialiseToCBOR a
    }
 where
  ttoken = asType :: AsType a

deserialiseFromTextEnvelope
  :: forall a
   . HasTextEnvelope a
  => TextEnvelope
  -> Either TextEnvelopeError a
deserialiseFromTextEnvelope te = do
  expectTextEnvelopeOfType (textEnvelopeType ttoken) te
  first TextEnvelopeDecodeError $
    deserialiseFromCBOR ttoken (teRawCBOR te) -- TODO: You have switched from CBOR to JSON
 where
  ttoken = asType :: AsType a

deserialiseFromTextEnvelopeAnyOf
  :: [FromSomeType HasTextEnvelope b]
  -> TextEnvelope
  -> Either TextEnvelopeError b
deserialiseFromTextEnvelopeAnyOf types te =
  case List.find matching types of
    Nothing ->
      Left (TextEnvelopeTypeError expectedTypes actualType)
    Just (FromSomeType ttoken f) ->
      first TextEnvelopeDecodeError $
        f <$> deserialiseFromCBOR ttoken (teRawCBOR te)
 where
  actualType = teType te
  expectedTypes =
    [ textEnvelopeType ttoken
    | FromSomeType ttoken _f <- types
    ]

  matching (FromSomeType ttoken _f) = textEnvelopeType ttoken `legacyComparison` actualType

writeFileTextEnvelope
  :: HasTextEnvelope a
  => File content Out
  -> Maybe TextEnvelopeDescr
  -> a
  -> IO (Either (FileError ()) ())
writeFileTextEnvelope outputFile mbDescr a =
  writeLazyByteStringFile outputFile (textEnvelopeToJSON mbDescr a)

textEnvelopeToJSON :: HasTextEnvelope a => Maybe TextEnvelopeDescr -> a -> LBS.ByteString
textEnvelopeToJSON mbDescr a =
  serialiseTextEnvelope $ serialiseToTextEnvelope mbDescr a

-- | Serialise text envelope to pretty JSON
serialiseTextEnvelope :: TextEnvelope -> LBS.ByteString
serialiseTextEnvelope te = encodePretty' textEnvelopeJsonConfig te <> "\n"

readFileTextEnvelope
  :: HasTextEnvelope a
  => File content In
  -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope path =
  runExceptT $ do
    content <- fileIOExceptT (unFile path) readFileBlocking
    firstExceptT (FileError (unFile path)) $ hoistEither $ do
      te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
      deserialiseFromTextEnvelope te

readFileTextEnvelopeAnyOf
  :: [FromSomeType HasTextEnvelope b]
  -> File content In
  -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
  runExceptT $ do
    content <- fileIOExceptT (unFile path) readFileBlocking
    firstExceptT (FileError (unFile path)) $ hoistEither $ do
      te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
      deserialiseFromTextEnvelopeAnyOf types te

readTextEnvelopeFromFile
  :: FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- fileIOExceptT path readFileBlocking
    firstExceptT (FileError path . TextEnvelopeAesonDecodeError)
      . hoistEither
      $ Aeson.eitherDecodeStrict' bs

readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $
      hoistEither $
        expectTextEnvelopeOfType expectedType te
    return te
