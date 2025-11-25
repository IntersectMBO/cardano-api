{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadUtxosResponse where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.AnyUtxoData
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ChainPoint

import Data.Aeson
import GHC.Generics

data ReadUtxosResponse = ReadUtxosResponse
  { readUtxosResponseItems :: [Maybe AnyUtxoData]
  , readUtxosResponseLedgerTip :: Maybe ChainPoint
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadUtxosResponse where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 17}

instance ToJSON ReadUtxosResponse where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 17, omitNothingFields = True}
