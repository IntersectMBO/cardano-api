{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.TxOutput where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.Datum
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.MultiAsset
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.Script

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data TxOutput = TxOutput
  { txOutputAddress :: Text
  , txOutputCoin :: Text
  , txOutputAssets :: [Maybe MultiAsset]
  , txOutputDatum :: Maybe Datum
  , txOutputScript :: Maybe Script
  }
  deriving (Show, Eq, Generic)

instance FromJSON TxOutput where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 8}

instance ToJSON TxOutput where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 8, omitNothingFields = True}
