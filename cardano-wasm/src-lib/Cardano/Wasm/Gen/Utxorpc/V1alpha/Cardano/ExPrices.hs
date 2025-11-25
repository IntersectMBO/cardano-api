{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.ExPrices where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.RationalNumber

import Data.Aeson
import GHC.Generics

data ExPrices = ExPrices
  { exPricesSteps :: Maybe RationalNumber
  , exPricesMemory :: Maybe RationalNumber
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExPrices where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 8}

instance ToJSON ExPrices where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 8, omitNothingFields = True}
