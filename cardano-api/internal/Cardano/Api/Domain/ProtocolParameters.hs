
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This will be removed in the next commit
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- HLINT ignore "Redundant pure" -}

module Cardano.Api.Domain.ProtocolParameters
  ( ProtocolParameters(..)
  , emptyProtocolParameters
  , protocolParametersL
  , protocolParamProtocolVersionL
  , protocolParamDecentralizationL
  , protocolParamExtraPraosEntropyL
  , protocolParamMaxBlockHeaderSizeL
  , protocolParamMaxBlockBodySizeL
  , protocolParamMaxTxSizeL
  , protocolParamTxFeeFixedL
  , protocolParamTxFeePerByteL
  , protocolParamMinUTxOValueL
  , protocolParamStakeAddressDepositL
  , protocolParamStakePoolDepositL
  , protocolParamMinPoolCostL
  , protocolParamPoolRetireMaxEpochL
  , protocolParamStakePoolTargetNumL
  , protocolParamPoolPledgeInfluenceL
  , protocolParamMonetaryExpansionL
  , protocolParamTreasuryCutL
  , protocolParamUTxOCostPerWordL
  , protocolParamCostModelsL
  , protocolParamPricesL
  , protocolParamMaxTxExUnitsL
  , protocolParamMaxBlockExUnitsL
  , protocolParamMaxValueSizeL
  , protocolParamCollateralPercentL
  , protocolParamMaxCollateralInputsL
  , protocolParamUTxOCostPerByteL
  ) where

import           Cardano.Api.Domain.Lovelace
import           Cardano.Api.Domain.PraosNonce
import           Cardano.Api.Domain.ProtVer
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
import           Cardano.Api.Feature.AlonzoEraOnly
import           Cardano.Api.Feature.AlonzoEraOnwards
import           Cardano.Api.Feature.BabbageEraOnwards
import           Cardano.Api.Feature.ShelleyToAllegraEra
import           Cardano.Api.Feature.ShelleyToAlonzoEra

import qualified Cardano.Ledger.Alonzo.Core as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Babbage.Core as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Function ((&))
import           GHC.Natural (Natural)
import           Lens.Micro (Lens', lens, (.~), (^.))

newtype ProtocolParameters era = ProtocolParameters
  { unProtocolParameters :: Ledger.PParams (ShelleyLedgerEra era)
  }

instance IsCardanoEra era => ToJSON (ProtocolParameters era) where
  toJSON pp =
    case cardanoEra @era of
      era ->
        object
          [ "extraPraosEntropy"       .= justInEraFeature era (\w -> pp ^. protocolParamExtraPraosEntropyL w)
          , "stakePoolTargetNum"      .= justInEraFeature era (\w -> pp ^. protocolParamStakePoolTargetNumL w)
          , "minUTxOValue"            .= justInEraFeature era (\w -> pp ^. protocolParamMinUTxOValueL w)
          -- , "poolRetireMaxEpoch"  .= protocolParamPoolRetireMaxEpoch
          -- , "decentralization"    .= (toRationalJSON <$> protocolParamDecentralization)
          , "maxBlockHeaderSize"      .= justInEraFeature era (\w -> pp ^. protocolParamMaxBlockHeaderSizeL w)
          , "maxBlockBodySize"        .= justInEraFeature era (\w -> pp ^. protocolParamMaxBlockBodySizeL w)
          , "maxTxSize"               .= justInEraFeature era (\w -> pp ^. protocolParamMaxTxSizeL w)
          -- , "treasuryCut"         .= toRationalJSON protocolParamTreasuryCut
          , "minPoolCost"             .= justInEraFeature era (\w -> pp ^. protocolParamMinPoolCostL w)
          -- , "monetaryExpansion"   .= toRationalJSON protocolParamMonetaryExpansion
          , "stakeAddressDeposit"     .= justInEraFeature era (\w -> pp ^. protocolParamStakeAddressDepositL w)
          -- , "poolPledgeInfluence" .= toRationalJSON protocolParamPoolPledgeInfluence
          , "protocolVersion"         .= justInEraFeature era (\w -> pp ^. protocolParamProtocolVersionL w)
          , "txFeeFixed"              .= justInEraFeature era (\w -> pp ^. protocolParamTxFeeFixedL w)
          , "txFeePerByte"            .= justInEraFeature era (\w -> pp ^. protocolParamTxFeePerByteL w)
          -- -- Alonzo era:
          -- , "utxoCostPerWord"        .= protocolParamUTxOCostPerWord
          -- , "costModels"             .= CostModels protocolParamCostModels
          -- , "executionUnitPrices"    .= protocolParamPrices
          -- , "maxTxExecutionUnits"    .= protocolParamMaxTxExUnits
          -- , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits
          , "maxValueSize"            .= justInEraFeature era (\w -> pp ^. protocolParamMaxValueSizeL w)
          , "collateralPercentage"    .= justInEraFeature era (\w -> pp ^. protocolParamCollateralPercentL w)
          , "maxCollateralInputs"     .= justInEraFeature era (\w -> pp ^. protocolParamMaxCollateralInputsL w)
          -- -- Babbage era:
          -- , "utxoCostPerByte"        .= protocolParamUTxOCostPerByte
          ]

instance IsCardanoEra era => FromJSON (ProtocolParameters era) where
  parseJSON =
    case cardanoEra @era of
      era ->
        withObject "ProtocolParameters" $ \o -> do
          pp <- inEraFeature era (fail ("Supported Era " <> show era)) (\w -> pure $ shelleyBasedEraConstraints w $ emptyProtocolParameters w)

          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "protocolVersion"
            pure (pp & protocolParamProtocolVersionL w .~ v)
          -- decentralization <- o .:? "decentralization"
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "extraPraosEntropy"
            pure (pp & protocolParamExtraPraosEntropyL w .~ v)
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "maxBlockHeaderSize"
            pure (pp & protocolParamMaxBlockHeaderSizeL w .~ v)
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "maxBlockBodySize"
            pure (pp & protocolParamMaxBlockBodySizeL w .~ v)
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "maxTxSize"
            pure (pp & protocolParamMaxTxSizeL w .~ v)
          -- txFeeFixed <- o .: "txFeeFixed"
          -- txFeePerByte <- o .: "txFeePerByte"
          -- minUTxOValue <- o .: "minUTxOValue"
          -- stakeAddressDeposit <- o .: "stakeAddressDeposit"
          -- stakePoolDeposit <- o .: "stakePoolDeposit"
          -- minPoolCost <- o .: "minPoolCost"
          -- poolRetireMaxEpoch <- o .: "poolRetireMaxEpoch"
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "stakePoolTargetNum"
            pure (pp & protocolParamStakePoolTargetNumL w .~ v)
          -- poolPledgeInfluence <- o .: "poolPledgeInfluence"
          -- monetaryExpansion <- o .: "monetaryExpansion"
          -- treasuryCut <- o .: "treasuryCut"
          -- utxoCostPerWord <- o .:? "utxoCostPerWord"
          -- costModels <- (fmap unCostModels <$> o .:? "costModels") .!= Map.empty
          -- executionUnitPrices <- o .:? "executionUnitPrices"
          -- maxTxExecutionUnits <- o .:? "maxTxExecutionUnits"
          -- maxBlockExecutionUnits <- o .:? "maxBlockExecutionUnits"
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "maxValueSize"
            pure (pp & protocolParamMaxValueSizeL w .~ v)
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "collateralPercentage"
            pure (pp & protocolParamCollateralPercentL w .~ v)
          pp <- inEraFeature era (pure pp) $ \w -> do
            v <- o .: "maxCollateralInputs"
            pure (pp & protocolParamMaxCollateralInputsL w .~ v)
          -- utxoCostPerByte <- o .:? "utxoCostPerByte"

          pure pp

protocolParametersL :: Lens' (ProtocolParameters era) (Ledger.PParams (ShelleyLedgerEra era))
protocolParametersL = lens unProtocolParameters (\_ pp -> ProtocolParameters pp)

emptyProtocolParameters :: ShelleyBasedEra era -> ProtocolParameters era
emptyProtocolParameters w = shelleyBasedEraConstraints w $ ProtocolParameters Ledger.emptyPParams

-- | Protocol version, major and minor. Updating the major version is
-- used to trigger hard forks.
protocolParamProtocolVersionL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) ProtVer
protocolParamProtocolVersionL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppProtocolVersionL . unLedgerProtVerL

-- | The decentralization parameter. This is fraction of slots that
-- belong to the BFT overlay schedule, rather than the Praos schedule.
-- So 1 means fully centralised, while 0 means fully decentralised.
--
-- This is the \"d\" parameter from the design document.
protocolParamDecentralizationL :: ShelleyToAlonzoEra era -> Lens' (ProtocolParameters era) Ledger.UnitInterval
protocolParamDecentralizationL w = shelleyToAlonzoEraConstraints w $ protocolParametersL . Ledger.ppDL

-- | Extra entropy for the Praos per-epoch nonce.
--
-- This can be used to add extra entropy during the decentralisation
-- process. If the extra entropy can be demonstrated to be generated
-- randomly then this method can be used to show that the initial
-- federated operators did not subtly bias the initial schedule so that
-- they retain undue influence after decentralisation.
protocolParamExtraPraosEntropyL :: ShelleyToAlonzoEra era -> Lens' (ProtocolParameters era) (Maybe PraosNonce)
protocolParamExtraPraosEntropyL w = shelleyToAlonzoEraConstraints w $ protocolParametersL . Ledger.ppExtraEntropyL . unLedgerNonceL

-- | The maximum permitted size of a block header.
--
-- This must be at least as big as the largest legitimate block headers
-- but should not be too much larger, to help prevent DoS attacks.
--
-- Caution: setting this to be smaller than legitimate block headers is
-- a sure way to brick the system!
protocolParamMaxBlockHeaderSizeL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Natural
protocolParamMaxBlockHeaderSizeL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppMaxBHSizeL

-- | The maximum permitted size of the block body (that is, the block
-- payload, without the block header).
--
-- This should be picked with the Praos network delta security parameter
-- in mind. Making this too large can severely weaken the Praos
-- consensus properties.
--
-- Caution: setting this to be smaller than a transaction that can
-- change the protocol parameters is a sure way to brick the system!
protocolParamMaxBlockBodySizeL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Natural
protocolParamMaxBlockBodySizeL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppMaxBBSizeL

-- | The maximum permitted size of a transaction.
--
-- Typically this should not be too high a fraction of the block size,
-- otherwise wastage from block fragmentation becomes a problem, and
-- the current implementation does not use any sophisticated box packing
-- algorithm.
protocolParamMaxTxSizeL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Natural
protocolParamMaxTxSizeL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppMaxTxSizeL

-- | The constant factor for the minimum fee calculation.
protocolParamTxFeeFixedL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Lovelace
protocolParamTxFeeFixedL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppMinFeeBL . unLedgerCoinL

-- | Per byte linear factor for the minimum fee calculation.
protocolParamTxFeePerByteL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Lovelace
protocolParamTxFeePerByteL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppMinFeeAL . unLedgerCoinL

-- | The minimum permitted value for new UTxO entries, ie for
-- transaction outputs.
protocolParamMinUTxOValueL :: ShelleyToAllegraEra era -> Lens' (ProtocolParameters era) Lovelace
protocolParamMinUTxOValueL w = shelleyToAllegraEraConstraints w $ protocolParametersL . Ledger.ppMinUTxOValueL . unLedgerCoinL

-- | The deposit required to register a stake address.
protocolParamStakeAddressDepositL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Lovelace
protocolParamStakeAddressDepositL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppKeyDepositL . unLedgerCoinL

-- | The deposit required to register a stake pool.
protocolParamStakePoolDepositL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Lovelace
protocolParamStakePoolDepositL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppPoolDepositL . unLedgerCoinL

-- | The minimum value that stake pools are permitted to declare for
-- their cost parameter.
protocolParamMinPoolCostL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Lovelace
protocolParamMinPoolCostL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppMinPoolCostL . unLedgerCoinL

-- | The maximum number of epochs into the future that stake pools
-- are permitted to schedule a retirement.
protocolParamPoolRetireMaxEpochL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Ledger.EpochNo
protocolParamPoolRetireMaxEpochL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppEMaxL

-- | The equilibrium target number of stake pools.
-- This is the \"k\" incentives parameter from the design document.
protocolParamStakePoolTargetNumL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Natural
protocolParamStakePoolTargetNumL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppNOptL

-- | The influence of the pledge in stake pool rewards.
-- This is the \"a_0\" incentives parameter from the design document.
protocolParamPoolPledgeInfluenceL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Ledger.NonNegativeInterval
protocolParamPoolPledgeInfluenceL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppA0L

-- | The monetary expansion rate. This determines the fraction of the
-- reserves that are added to the fee pot each epoch.
-- This is the \"rho\" incentives parameter from the design document.
protocolParamMonetaryExpansionL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Ledger.UnitInterval
protocolParamMonetaryExpansionL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppRhoL

-- | The fraction of the fee pot each epoch that goes to the treasury.
-- This is the \"tau\" incentives parameter from the design document.
protocolParamTreasuryCutL :: ShelleyBasedEra era -> Lens' (ProtocolParameters era) Ledger.UnitInterval
protocolParamTreasuryCutL w = shelleyBasedEraConstraints w $ protocolParametersL . Ledger.ppTauL

-- | Cost in ada per word of UTxO storage.
protocolParamUTxOCostPerWordL :: AlonzoEraOnly era -> Lens' (ProtocolParameters era) Ledger.CoinPerWord
protocolParamUTxOCostPerWordL w = alonzoEraOnlyConstraints w $ protocolParametersL . Ledger.ppCoinsPerUTxOWordL

-- | Cost models for script languages that use them.
protocolParamCostModelsL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Ledger.CostModels
protocolParamCostModelsL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppCostModelsL

-- | Price of execution units for script languages that use them.
protocolParamPricesL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Ledger.Prices
protocolParamPricesL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppPricesL

-- | Max total script execution resources units allowed per tx
protocolParamMaxTxExUnitsL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Ledger.ExUnits
protocolParamMaxTxExUnitsL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppMaxTxExUnitsL

-- | Max total script execution resources units allowed per block
protocolParamMaxBlockExUnitsL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Ledger.ExUnits
protocolParamMaxBlockExUnitsL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppMaxBlockExUnitsL

-- | Max size of a Value in a tx output.
protocolParamMaxValueSizeL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Natural
protocolParamMaxValueSizeL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppMaxValSizeL

-- | The percentage of the script contribution to the txfee that must be
-- provided as collateral inputs when including Plutus scripts.
protocolParamCollateralPercentL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Natural
protocolParamCollateralPercentL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppCollateralPercentageL

-- | The maximum number of collateral inputs allowed in a transaction.
protocolParamMaxCollateralInputsL :: AlonzoEraOnwards era -> Lens' (ProtocolParameters era) Natural
protocolParamMaxCollateralInputsL w = alonzoEraOnwardsConstraints w $ protocolParametersL . Ledger.ppMaxCollateralInputsL

-- | Cost in ada per byte of UTxO storage.
protocolParamUTxOCostPerByteL :: BabbageEraOnwards era -> Lens' (ProtocolParameters era) Ledger.CoinPerByte
protocolParamUTxOCostPerByteL w = babbageEraOnwardsConstraints w $ protocolParametersL . Ledger.ppCoinsPerUTxOByteL
