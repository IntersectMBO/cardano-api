{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Cardano.Rpc.Era where

import Data.Aeson
import Data.Char (toLower)
import GHC.Generics

data Era
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway
  deriving (Show, Eq, Enum, Bounded, Generic)

instance FromJSON Era where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = map toLower
        }

instance ToJSON Era where
  toJSON =
    genericToJSON
      defaultOptions
        { constructorTagModifier = map toLower
        }