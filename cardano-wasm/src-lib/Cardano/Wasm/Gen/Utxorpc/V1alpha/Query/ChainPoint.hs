{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ChainPoint where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data ChainPoint = ChainPoint
  { chainPointSlot :: Text
  , chainPointHash :: Text
  , chainPointHeight :: Text
  , chainPointTimestamp :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ChainPoint where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 10}

instance ToJSON ChainPoint where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True}
