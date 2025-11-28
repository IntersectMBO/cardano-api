{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Cardano.Rpc.ProtocolParamsJson where

import Data.Aeson
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics

newtype ProtocolParamsJson = ProtocolParamsJson
  { protocolParamsJsonJson :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProtocolParamsJson where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = map toLower . drop 18}

instance ToJSON ProtocolParamsJson where
  toJSON =
    genericToJSON defaultOptions{fieldLabelModifier = map toLower . drop 18, omitNothingFields = True}