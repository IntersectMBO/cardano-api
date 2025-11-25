{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Cardano.Rpc.ProtocolParamsJson where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype ProtocolParamsJson = ProtocolParamsJson
  { protocolParamsJsonJson :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProtocolParamsJson where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 18}

instance ToJSON ProtocolParamsJson where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 18, omitNothingFields = True}
