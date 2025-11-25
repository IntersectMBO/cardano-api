{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitTxResponse where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.TxSubmitResult

import Data.Aeson
import GHC.Generics

newtype SubmitTxResponse = SubmitTxResponse
  { submitTxResponseResults :: [Maybe TxSubmitResult]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubmitTxResponse where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 16}

instance ToJSON SubmitTxResponse where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 16, omitNothingFields = True}
