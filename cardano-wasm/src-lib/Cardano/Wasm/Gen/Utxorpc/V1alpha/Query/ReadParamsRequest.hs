{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadParamsRequest where

import Cardano.Wasm.Gen.Google.Protobuf.FieldMask (FieldMask)

import Data.Aeson
import GHC.Generics

newtype ReadParamsRequest = ReadParamsRequest
  { readParamsRequestFieldMask :: Maybe FieldMask
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadParamsRequest where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 17}

instance ToJSON ReadParamsRequest where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 17, omitNothingFields = True}
