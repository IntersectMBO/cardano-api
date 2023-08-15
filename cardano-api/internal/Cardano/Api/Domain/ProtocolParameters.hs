{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Domain.ProtocolParameters
  ( emptyProtocolParameters
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
import qualified Cardano.Ledger.Coin as Ledger

import           GHC.Natural (Natural)
import           Lens.Micro (Lens')

emptyProtocolParameters :: ShelleyBasedEra era -> Ledger.PParams (ShelleyLedgerEra era)
emptyProtocolParameters w = shelleyBasedEraConstraints w Ledger.emptyPParams

-- | Protocol version, major and minor. Updating the major version is
-- used to trigger hard forks.
protocolParamProtocolVersionL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.ProtVer
protocolParamProtocolVersionL w = shelleyBasedEraConstraints w Ledger.ppProtocolVersionL

-- | The decentralization parameter. This is fraction of slots that
-- belong to the BFT overlay schedule, rather than the Praos schedule.
-- So 1 means fully centralised, while 0 means fully decentralised.
--
-- This is the \"d\" parameter from the design document.
protocolParamDecentralizationL :: ShelleyToAlonzoEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.UnitInterval
protocolParamDecentralizationL w = shelleyToAlonzoEraConstraints w Ledger.ppDL

-- | Extra entropy for the Praos per-epoch nonce.
--
-- This can be used to add extra entropy during the decentralisation
-- process. If the extra entropy can be demonstrated to be generated
-- randomly then this method can be used to show that the initial
-- federated operators did not subtly bias the initial schedule so that
-- they retain undue influence after decentralisation.
protocolParamExtraPraosEntropyL :: ShelleyToAlonzoEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Nonce
protocolParamExtraPraosEntropyL w = shelleyToAlonzoEraConstraints w Ledger.ppExtraEntropyL

-- | The maximum permitted size of a block header.
--
-- This must be at least as big as the largest legitimate block headers
-- but should not be too much larger, to help prevent DoS attacks.
--
-- Caution: setting this to be smaller than legitimate block headers is
-- a sure way to brick the system!
protocolParamMaxBlockHeaderSizeL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamMaxBlockHeaderSizeL w = shelleyBasedEraConstraints w Ledger.ppMaxBHSizeL

-- | The maximum permitted size of the block body (that is, the block
-- payload, without the block header).
--
-- This should be picked with the Praos network delta security parameter
-- in mind. Making this too large can severely weaken the Praos
-- consensus properties.
--
-- Caution: setting this to be smaller than a transaction that can
-- change the protocol parameters is a sure way to brick the system!
protocolParamMaxBlockBodySizeL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamMaxBlockBodySizeL w = shelleyBasedEraConstraints w Ledger.ppMaxBBSizeL

-- | The maximum permitted size of a transaction.
--
-- Typically this should not be too high a fraction of the block size,
-- otherwise wastage from block fragmentation becomes a problem, and
-- the current implementation does not use any sophisticated box packing
-- algorithm.
protocolParamMaxTxSizeL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamMaxTxSizeL w = shelleyBasedEraConstraints w Ledger.ppMaxTxSizeL

-- | The constant factor for the minimum fee calculation.
protocolParamTxFeeFixedL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Coin
protocolParamTxFeeFixedL w = shelleyBasedEraConstraints w Ledger.ppMinFeeBL

-- | Per byte linear factor for the minimum fee calculation.
protocolParamTxFeePerByteL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Coin
protocolParamTxFeePerByteL w = shelleyBasedEraConstraints w Ledger.ppMinFeeAL

-- | The minimum permitted value for new UTxO entries, ie for
-- transaction outputs.
protocolParamMinUTxOValueL :: ShelleyToAllegraEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Coin
protocolParamMinUTxOValueL w = shelleyToAllegraEraConstraints w Ledger.ppMinUTxOValueL

-- | The deposit required to register a stake address.
protocolParamStakeAddressDepositL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Coin
protocolParamStakeAddressDepositL w = shelleyBasedEraConstraints w Ledger.ppKeyDepositL

-- | The deposit required to register a stake pool.
protocolParamStakePoolDepositL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Coin
protocolParamStakePoolDepositL w = shelleyBasedEraConstraints w Ledger.ppPoolDepositL

-- | The minimum value that stake pools are permitted to declare for
-- their cost parameter.
protocolParamMinPoolCostL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Coin
protocolParamMinPoolCostL w = shelleyBasedEraConstraints w Ledger.ppMinPoolCostL

-- | The maximum number of epochs into the future that stake pools
-- are permitted to schedule a retirement.
protocolParamPoolRetireMaxEpochL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.EpochNo
protocolParamPoolRetireMaxEpochL w = shelleyBasedEraConstraints w Ledger.ppEMaxL

-- | The equilibrium target number of stake pools.
-- This is the \"k\" incentives parameter from the design document.
protocolParamStakePoolTargetNumL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamStakePoolTargetNumL w = shelleyBasedEraConstraints w Ledger.ppNOptL

-- | The influence of the pledge in stake pool rewards.
-- This is the \"a_0\" incentives parameter from the design document.
protocolParamPoolPledgeInfluenceL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.NonNegativeInterval
protocolParamPoolPledgeInfluenceL w = shelleyBasedEraConstraints w Ledger.ppA0L

-- | The monetary expansion rate. This determines the fraction of the
-- reserves that are added to the fee pot each epoch.
-- This is the \"rho\" incentives parameter from the design document.
protocolParamMonetaryExpansionL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.UnitInterval
protocolParamMonetaryExpansionL w = shelleyBasedEraConstraints w Ledger.ppRhoL

-- | The fraction of the fee pot each epoch that goes to the treasury.
-- This is the \"tau\" incentives parameter from the design document.
protocolParamTreasuryCutL :: ShelleyBasedEra era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.UnitInterval
protocolParamTreasuryCutL w = shelleyBasedEraConstraints w Ledger.ppTauL

-- | Cost in ada per word of UTxO storage.
protocolParamUTxOCostPerWordL :: AlonzoEraOnly era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.CoinPerWord
protocolParamUTxOCostPerWordL w = alonzoEraOnlyConstraints w Ledger.ppCoinsPerUTxOWordL

-- | Cost models for script languages that use them.
protocolParamCostModelsL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.CostModels
protocolParamCostModelsL w = alonzoEraOnwardsConstraints w Ledger.ppCostModelsL

-- | Price of execution units for script languages that use them.
protocolParamPricesL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.Prices
protocolParamPricesL w = alonzoEraOnwardsConstraints w Ledger.ppPricesL

-- | Max total script execution resources units allowed per tx
protocolParamMaxTxExUnitsL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.ExUnits
protocolParamMaxTxExUnitsL w = alonzoEraOnwardsConstraints w Ledger.ppMaxTxExUnitsL

-- | Max total script execution resources units allowed per block
protocolParamMaxBlockExUnitsL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.ExUnits
protocolParamMaxBlockExUnitsL w = alonzoEraOnwardsConstraints w Ledger.ppMaxBlockExUnitsL

-- | Max size of a Value in a tx output.
protocolParamMaxValueSizeL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamMaxValueSizeL w = alonzoEraOnwardsConstraints w Ledger.ppMaxValSizeL

-- | The percentage of the script contribution to the txfee that must be
-- provided as collateral inputs when including Plutus scripts.
protocolParamCollateralPercentL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamCollateralPercentL w = alonzoEraOnwardsConstraints w Ledger.ppCollateralPercentageL

-- | The maximum number of collateral inputs allowed in a transaction.
protocolParamMaxCollateralInputsL :: AlonzoEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Natural
protocolParamMaxCollateralInputsL w = alonzoEraOnwardsConstraints w Ledger.ppMaxCollateralInputsL

-- | Cost in ada per byte of UTxO storage.
protocolParamUTxOCostPerByteL :: BabbageEraOnwards era -> Lens' (Ledger.PParams (ShelleyLedgerEra era)) Ledger.CoinPerByte
protocolParamUTxOCostPerByteL w = babbageEraOnwardsConstraints w Ledger.ppCoinsPerUTxOByteL
