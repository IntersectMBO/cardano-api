{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.TxSubmitResult where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data TxSubmitResult = TxSubmitResult
  { txSubmitResultRef :: Text
  , txSubmitResultErrorMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TxSubmitResult where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 14}

instance ToJSON TxSubmitResult where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 14, omitNothingFields = True}
