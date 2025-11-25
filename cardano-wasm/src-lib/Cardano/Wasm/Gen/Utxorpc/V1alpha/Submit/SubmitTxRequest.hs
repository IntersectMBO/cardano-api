{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitTxRequest where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.AnyChainTx

import Data.Aeson
import GHC.Generics

newtype SubmitTxRequest = SubmitTxRequest
  { submitTxRequestTx :: [Maybe AnyChainTx]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubmitTxRequest where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 15}

instance ToJSON SubmitTxRequest where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 15, omitNothingFields = True}
