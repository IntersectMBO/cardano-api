{-# LANGUAGE DeriveDataTypeable #-}

-- | JSON serialisation
module Cardano.Api.Serialise.Json
  ( serialiseToJSON
  , ToJSON (..)
  , ToJSONKey
  , deserialiseFromJSON
  , prettyPrintJSON
  , FromJSON (..)
  , FromJSONKey
  , JsonDecodeError (..)
  , readFileJSON
  , writeFileJSON
  , textWithMaxLength
  , toRationalJSON
  )
where

import Cardano.Api.Error
import Cardano.Api.Pretty

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Data)
import Data.Scientific (fromRationalRepetendLimited)
import Data.Text (Text)
import Data.Text qualified as T

newtype JsonDecodeError = JsonDecodeError String
  deriving (Eq, Show, Data)

instance Error JsonDecodeError where
  prettyError (JsonDecodeError err) =
    pretty err

serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

prettyPrintJSON :: ToJSON a => a -> ByteString
prettyPrintJSON = LBS.toStrict . encodePretty

deserialiseFromJSON
  :: FromJSON a
  => ByteString
  -> Either JsonDecodeError a
deserialiseFromJSON =
  either (Left . JsonDecodeError) Right
    . Aeson.eitherDecodeStrict'

readFileJSON
  :: FromJSON a
  => FilePath
  -> IO (Either (FileError JsonDecodeError) a)
readFileJSON path =
  runExceptT $ do
    content <- fileIOExceptT path BS.readFile
    firstExceptT (FileError path) $
      hoistEither $
        deserialiseFromJSON content

writeFileJSON
  :: ToJSON a
  => FilePath
  -> a
  -> IO (Either (FileError ()) ())
writeFileJSON path x =
  runExceptT $
    handleIOExceptT (FileIOError path) $
      BS.writeFile path (serialiseToJSON x)

-- | Parser for 'Text' that validates that the number of characters is
-- under a given maximum. The 'String' parameter is meant to be the name
-- of the field in order to be able to give context in case of error.
textWithMaxLength :: String -> Int -> Value -> Aeson.Parser Text
textWithMaxLength fieldName maxLen value = do
  txt <- parseJSON value
  if T.length txt <= maxLen
    then pure txt
    else
      fail $
        "key \""
          ++ fieldName
          ++ "\" exceeds maximum length of "
          ++ show maxLen
          ++ " characters. Got length: "
          ++ show (T.length txt)

-- Rationals and JSON are an awkward mix. We cannot convert rationals
-- like @1/3@ to JSON numbers. But _most_ of the numbers we want to use
-- in practice have simple decimal representations. Our solution here is
-- to use simple decimal representations where we can and representation
-- in a @{"numerator": 1, "denominator": 3}@ style otherwise.
--
toRationalJSON :: Rational -> Value
toRationalJSON r =
  case fromRationalRepetendLimited 20 r of
    Right (s, Nothing) -> toJSON s
    _ -> toJSON r
