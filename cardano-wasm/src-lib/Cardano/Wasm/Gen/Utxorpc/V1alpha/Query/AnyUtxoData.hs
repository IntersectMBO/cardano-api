{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.AnyUtxoData where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.TxOutput (TxOutput)
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.TxoRef

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data AnyUtxoData = AnyUtxoData
  { anyUtxoDataNativeBytes :: Text
  , anyUtxoDataTxoRef :: Maybe TxoRef
  , anyUtxoDataCardano :: Maybe TxOutput
  }
  deriving (Show, Eq, Generic)

instance FromJSON AnyUtxoData where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 11}

instance ToJSON AnyUtxoData where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 11, omitNothingFields = True}
