{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.TxoRefArray where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.TxoRef

import Data.Aeson
import GHC.Generics

newtype TxoRefArray = TxoRefArray
  { txoRefArrayItems :: [Maybe TxoRef]
  }
  deriving (Show, Eq, Generic)

instance FromJSON TxoRefArray where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 11}

instance ToJSON TxoRefArray where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 11, omitNothingFields = True}
