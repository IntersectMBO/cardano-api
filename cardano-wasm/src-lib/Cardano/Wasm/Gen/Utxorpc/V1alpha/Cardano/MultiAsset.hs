{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.MultiAsset where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.Asset

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data MultiAsset = MultiAsset
  { multiAssetPolicyId :: Text
  , multiAssetAssets :: [Maybe Asset]
  }
  deriving (Show, Eq, Generic)

instance FromJSON MultiAsset where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 10}

instance ToJSON MultiAsset where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True}
