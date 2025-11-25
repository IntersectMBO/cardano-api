{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.RationalNumber where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data RationalNumber = RationalNumber
  { rationalNumberNumerator :: Text
  , rationalNumberDenominator :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RationalNumber where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 14}

instance ToJSON RationalNumber where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 14, omitNothingFields = True}
