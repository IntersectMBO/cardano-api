{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.AnyChainTx where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype AnyChainTx = AnyChainTx
  { anyChainTxRaw :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON AnyChainTx where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 10}

instance ToJSON AnyChainTx where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True}
