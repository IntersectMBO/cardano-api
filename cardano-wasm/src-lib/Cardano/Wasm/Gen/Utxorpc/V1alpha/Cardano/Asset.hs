{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.Asset where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Asset = Asset
  { assetName :: Text
  , assetOutputCoin :: Text
  , assetMintCoin :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Asset where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 5}

instance ToJSON Asset where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True}
