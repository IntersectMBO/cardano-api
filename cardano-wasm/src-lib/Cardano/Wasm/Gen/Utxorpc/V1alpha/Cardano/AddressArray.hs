{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.AddressArray where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype AddressArray = AddressArray
  { addressArrayItems :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON AddressArray where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 12}

instance ToJSON AddressArray where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 12, omitNothingFields = True}
