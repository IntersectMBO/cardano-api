{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.CostModel where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype CostModel = CostModel
  { costModelValues :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CostModel where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 9}

instance ToJSON CostModel where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 9, omitNothingFields = True}
