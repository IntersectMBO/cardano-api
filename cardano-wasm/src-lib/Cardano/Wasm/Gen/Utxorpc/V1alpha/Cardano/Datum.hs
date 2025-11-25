{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.Datum where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.PlutusData

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Datum = Datum
  { datumHash :: Text
  , datumPayload :: Maybe PlutusData
  , datumOriginalCbor :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Datum where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 5}

instance ToJSON Datum where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True}
