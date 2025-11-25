{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.ExUnits where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data ExUnits = ExUnits
  { exUnitsSteps :: Text
  , exUnitsMemory :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExUnits where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 7}

instance ToJSON ExUnits where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True}
