{-# LANGUAGE DeriveDataTypeable #-}

-- | JSON serialisation
module Cardano.Api.Internal.SerialiseJSON
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
  )
where

import Cardano.Api.Internal.Error
import Cardano.Api.Internal.Pretty

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Data)

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
