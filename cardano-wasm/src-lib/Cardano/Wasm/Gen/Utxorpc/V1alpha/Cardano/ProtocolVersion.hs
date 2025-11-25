{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.ProtocolVersion where

import Data.Aeson
import GHC.Generics

data ProtocolVersion = ProtocolVersion
  { protocolVersionMajor :: Int
  , protocolVersionMinor :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProtocolVersion where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 15}

instance ToJSON ProtocolVersion where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 15, omitNothingFields = True}
