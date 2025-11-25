{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.PParams where

import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.CostModels
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.ExPrices
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.ExUnits
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.ProtocolVersion
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.RationalNumber
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.VotingThresholds

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data PParams = PParams
  { pParamsCoinsPerUtxoByte :: Text
  , pParamsMaxTxSize :: Text
  , pParamsMinFeeCoefficient :: Text
  , pParamsMinFeeConstant :: Text
  , pParamsMaxBlockBodySize :: Text
  , pParamsMaxBlockHeaderSize :: Text
  , pParamsStakeKeyDeposit :: Text
  , pParamsPoolDeposit :: Text
  , pParamsPoolRetirementEpochBound :: Text
  , pParamsDesiredNumberOfPools :: Text
  , pParamsPoolInfluence :: Maybe RationalNumber
  , pParamsMonetaryExpansion :: Maybe RationalNumber
  , pParamsTreasuryExpansion :: Maybe RationalNumber
  , pParamsMinPoolCost :: Text
  , pParamsProtocolVersion :: Maybe ProtocolVersion
  , pParamsMaxValueSize :: Text
  , pParamsCollateralPercentage :: Text
  , pParamsMaxCollateralInputs :: Text
  , pParamsCostModels :: Maybe CostModels
  , pParamsPrices :: Maybe ExPrices
  , pParamsMaxExecutionUnitsPerTransaction :: Maybe ExUnits
  , pParamsMaxExecutionUnitsPerBlock :: Maybe ExUnits
  , pParamsMinFeeScriptRefCostPerByte :: Maybe RationalNumber
  , pParamsPoolVotingThresholds :: Maybe VotingThresholds
  , pParamsDrepVotingThresholds :: Maybe VotingThresholds
  , pParamsMinCommitteeSize :: Int
  , pParamsCommitteeTermLimit :: Text
  , pParamsGovernanceActionValidityPeriod :: Text
  , pParamsGovernanceActionDeposit :: Text
  , pParamsDrepDeposit :: Text
  , pParamsDrepInactivityPeriod :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PParams where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 7}

instance ToJSON PParams where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True}
