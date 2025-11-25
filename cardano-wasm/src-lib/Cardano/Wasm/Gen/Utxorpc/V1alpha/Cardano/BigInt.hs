{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.BigInt where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data BigInt = BigInt
  { bigIntInt :: Text
  , bigIntBigUInt :: Text
  , bigIntBigNInt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON BigInt where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 6}

instance ToJSON BigInt where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 6, omitNothingFields = True}
