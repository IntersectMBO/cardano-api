{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadUtxosRequest where

import Cardano.Wasm.Gen.Google.Protobuf.FieldMask (FieldMask)
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.AddressArray (AddressArray)
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.TxoRefArray

import Data.Aeson
import GHC.Generics

data ReadUtxosRequest = ReadUtxosRequest
  { readUtxosRequestTxoRefs :: Maybe TxoRefArray
  , readUtxosRequestCardanoAddresses :: Maybe AddressArray
  , readUtxosRequestFieldMask :: Maybe FieldMask
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadUtxosRequest where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 16}

instance ToJSON ReadUtxosRequest where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 16, omitNothingFields = True}
