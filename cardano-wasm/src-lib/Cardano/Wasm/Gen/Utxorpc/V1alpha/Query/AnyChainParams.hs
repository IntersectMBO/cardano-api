{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.AnyChainParams where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.PParams (PParams)

import Data.Aeson
import GHC.Generics

newtype AnyChainParams = AnyChainParams
  { anyChainParamsCardano :: Maybe PParams
  }
  deriving (Show, Eq, Generic)

instance FromJSON AnyChainParams where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 14}

instance ToJSON AnyChainParams where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 14, omitNothingFields = True}
