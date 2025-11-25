{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.PlutusData where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.BigInt

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data PlutusData = PlutusData
  { plutusDataConstr :: Maybe Constr
  , plutusDataMap :: Maybe PlutusDataMap
  , plutusDataBigInt :: Maybe BigInt
  , plutusDataBoundedBytes :: Text
  , plutusDataArray :: Maybe PlutusDataArray
  }
  deriving (Show, Eq, Generic)

newtype PlutusDataArray = PlutusDataArray
  { plutusDataArrayItems :: [Maybe PlutusData]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlutusDataArray where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 15}

instance ToJSON PlutusDataArray where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 15, omitNothingFields = True}

instance FromJSON PlutusData where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 10}

instance ToJSON PlutusData where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True}

newtype PlutusDataMap = PlutusDataMap
  { plutusDataMapPairs :: [Maybe PlutusDataPair]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlutusDataMap where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 13}

instance ToJSON PlutusDataMap where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 13, omitNothingFields = True}

data PlutusDataPair = PlutusDataPair
  { plutusDataPairKey :: Maybe PlutusData
  , plutusDataPairValue :: Maybe PlutusData
  }
  deriving (Show, Eq, Generic)

instance FromJSON PlutusDataPair where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 14}

instance ToJSON PlutusDataPair where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 14, omitNothingFields = True}

data Constr = Constr
  { constrTag :: Int
  , constrAnyConstructor :: Text
  , constrFields :: [Maybe PlutusData]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Constr where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 6}

instance ToJSON Constr where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 6, omitNothingFields = True}
