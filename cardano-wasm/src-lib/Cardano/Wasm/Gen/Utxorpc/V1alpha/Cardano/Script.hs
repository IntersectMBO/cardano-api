{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.Script where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.NativeScript

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Script = Script
  { scriptNative :: Maybe NativeScript
  , scriptPlutusV1 :: Text
  , scriptPlutusV2 :: Text
  , scriptPlutusV3 :: Text
  , scriptPlutusV4 :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Script where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 6}

instance ToJSON Script where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 6, omitNothingFields = True}
