{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Google.Protobuf.FieldMask where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype FieldMask = FieldMask
  { fieldMaskPaths :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON FieldMask where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 9}

instance ToJSON FieldMask where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 9, omitNothingFields = True}
