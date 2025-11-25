{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.TxoRef where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data TxoRef = TxoRef
  { txoRefHash :: Text
  , txoRefIndex :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON TxoRef where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 6}

instance ToJSON TxoRef where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 6, omitNothingFields = True}
