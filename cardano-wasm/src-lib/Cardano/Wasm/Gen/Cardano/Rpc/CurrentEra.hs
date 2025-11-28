{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Cardano.Rpc.CurrentEra where

import Cardano.Wasm.Gen.Cardano.Rpc.Era (Era)

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics

newtype CurrentEra = CurrentEra
  { currentEraEra :: Maybe Era
  }
  deriving (Show, Eq, Generic)

instance FromJSON CurrentEra where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = map toLower . drop 10
        , omitNothingFields = True
        }

instance ToJSON CurrentEra where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = map toLower . drop 10
        , omitNothingFields = True
        }