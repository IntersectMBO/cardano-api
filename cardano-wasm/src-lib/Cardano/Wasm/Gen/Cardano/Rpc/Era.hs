{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Cardano.Rpc.Era where

import Data.Aeson
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

instance FromJSON Era

instance ToJSON Era
