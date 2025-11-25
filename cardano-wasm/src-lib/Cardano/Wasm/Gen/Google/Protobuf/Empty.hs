{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Google.Protobuf.Empty where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import GHC.Generics

data Empty = Empty
  deriving (Show, Eq, Generic)

instance FromJSON Empty where
  parseJSON _ = return Empty

instance ToJSON Empty where
  toJSON _ = emptyObject
