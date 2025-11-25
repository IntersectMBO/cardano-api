{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.NativeScript where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data NativeScript = NativeScript
  { nativeScriptScriptPubkey :: Text
  , nativeScriptScriptAll :: Maybe NativeScriptList
  , nativeScriptScriptAny :: Maybe NativeScriptList
  , nativeScriptScriptNOfK :: Maybe ScriptNOfK
  , nativeScriptInvalidBefore :: Text
  , nativeScriptInvalidHereafter :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON NativeScript where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 12}

instance ToJSON NativeScript where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 12, omitNothingFields = True}

newtype NativeScriptList = NativeScriptList
  { nativeScriptListItems :: [Maybe NativeScript]
  }
  deriving (Show, Eq, Generic)

instance FromJSON NativeScriptList where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 16}

instance ToJSON NativeScriptList where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 16, omitNothingFields = True}

data ScriptNOfK = ScriptNOfK
  { scriptNOfKK :: Int
  , scriptNOfKScripts :: [Maybe NativeScript]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ScriptNOfK where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 10}

instance ToJSON ScriptNOfK where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True}
