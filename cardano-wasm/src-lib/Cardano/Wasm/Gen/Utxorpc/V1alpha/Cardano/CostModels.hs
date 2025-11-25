{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.CostModels where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.CostModel

import Data.Aeson
import GHC.Generics

data CostModels = CostModels
  { costModelsPlutusV1 :: Maybe CostModel
  , costModelsPlutusV2 :: Maybe CostModel
  , costModelsPlutusV3 :: Maybe CostModel
  , costModelsPlutusV4 :: Maybe CostModel
  }
  deriving (Show, Eq, Generic)

instance FromJSON CostModels where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 10}

instance ToJSON CostModels where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True}
