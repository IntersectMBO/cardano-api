{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.VotingThresholds where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.RationalNumber

import Data.Aeson
import GHC.Generics

newtype VotingThresholds = VotingThresholds
  { votingThresholdsThresholds :: [Maybe RationalNumber]
  }
  deriving (Show, Eq, Generic)

instance FromJSON VotingThresholds where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 16}

instance ToJSON VotingThresholds where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 16, omitNothingFields = True}
