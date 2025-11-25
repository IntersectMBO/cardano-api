{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadParamsResponse where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.AnyChainParams
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ChainPoint

import Data.Aeson
import GHC.Generics

data ReadParamsResponse = ReadParamsResponse
  { readParamsResponseValues :: Maybe AnyChainParams
  , readParamsResponseLedgerTip :: Maybe ChainPoint
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadParamsResponse where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 18}

instance ToJSON ReadParamsResponse where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 18, omitNothingFields = True}
