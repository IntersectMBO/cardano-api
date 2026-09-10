{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Era-based protocol parameter updates, and the era-sliced record types
-- that back them.
module Cardano.Api.Compatible.ProtocolParametersUpdate
  ( EraBasedProtocolParametersUpdate (..)
  , AlonzoOnwardsPParams (..)
  , CommonProtocolParametersUpdate (..)
  , DeprecatedAfterBabbagePParams (..)
  , DeprecatedAfterMaryPParams (..)
  , ShelleyToAlonzoPParams (..)
  , IntroducedInBabbagePParams (..)
  , IntroducedInConwayPParams (..)
  , IntroducedInDijkstraPParams (..)
  , createEraBasedProtocolParamUpdate
  , createPParams

    -- * Update proposals to change the protocol parameters
  , UpdateProposal (..)
  , makeShelleyUpdateProposal

    -- * Internal conversion functions
  , toLedgerUpdate
  , fromLedgerUpdate
  , toLedgerProposedPPUpdates
  , fromLedgerProposedPPUpdates
  , fromLedgerPParamsUpdate

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Key.Internal
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api.Era qualified as Ledger
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.Babbage.Core qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Dijkstra.PParams qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.Slot (EpochNo (..))

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Typeable
import Data.Word
import GHC.Exts (IsList (..))
import Lens.Micro

createPParams
  :: ShelleyBasedEra era
  -> EraBasedProtocolParametersUpdate era
  -> Ledger.PParams (ShelleyLedgerEra era)
createPParams sbe ebPParamsUpdate =
  shelleyBasedEraConstraints sbe $
    let ppUp = createEraBasedProtocolParamUpdate sbe ebPParamsUpdate
     in Ledger.applyPPUpdates emptyPParams ppUp

-- -----------------------------------------------------------------------------
-- Era based Ledger protocol parameters update
--

-- | Each constructor corresponds to the set of protocol parameters available
-- in a given era.
data EraBasedProtocolParametersUpdate era where
  ShelleyEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> DeprecatedAfterMaryPParams ShelleyEra
    -> DeprecatedAfterBabbagePParams ShelleyEra
    -> ShelleyToAlonzoPParams ShelleyEra
    -> EraBasedProtocolParametersUpdate ShelleyEra
  AllegraEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> DeprecatedAfterMaryPParams AllegraEra
    -> ShelleyToAlonzoPParams AllegraEra
    -> DeprecatedAfterBabbagePParams ShelleyEra
    -> EraBasedProtocolParametersUpdate AllegraEra
  MaryEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> DeprecatedAfterMaryPParams MaryEra
    -> ShelleyToAlonzoPParams MaryEra
    -> DeprecatedAfterBabbagePParams ShelleyEra
    -> EraBasedProtocolParametersUpdate MaryEra
  AlonzoEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> ShelleyToAlonzoPParams AlonzoEra
    -> AlonzoOnwardsPParams AlonzoEra
    -> DeprecatedAfterBabbagePParams ShelleyEra
    -> EraBasedProtocolParametersUpdate AlonzoEra
  BabbageEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> AlonzoOnwardsPParams BabbageEra
    -> DeprecatedAfterBabbagePParams ShelleyEra
    -> IntroducedInBabbagePParams BabbageEra
    -> EraBasedProtocolParametersUpdate BabbageEra
  ConwayEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> AlonzoOnwardsPParams ConwayEra
    -> IntroducedInBabbagePParams ConwayEra
    -> IntroducedInConwayPParams (ShelleyLedgerEra ConwayEra)
    -> EraBasedProtocolParametersUpdate ConwayEra
  DijkstraEraBasedProtocolParametersUpdate
    :: CommonProtocolParametersUpdate
    -> AlonzoOnwardsPParams DijkstraEra
    -> IntroducedInBabbagePParams DijkstraEra
    -> IntroducedInConwayPParams (ShelleyLedgerEra DijkstraEra)
    -> IntroducedInDijkstraPParams (ShelleyLedgerEra DijkstraEra)
    -> EraBasedProtocolParametersUpdate DijkstraEra

deriving instance Show (EraBasedProtocolParametersUpdate era)

deriving instance Eq (EraBasedProtocolParametersUpdate era)

instance IsShelleyBasedEra era => ToCBOR (EraBasedProtocolParametersUpdate era) where
  toCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      toCBOR . createEraBasedProtocolParamUpdate shelleyBasedEra

instance IsShelleyBasedEra era => FromCBOR (EraBasedProtocolParametersUpdate era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      fromLedgerPParamsUpdate shelleyBasedEra <$> fromCBOR

data IntroducedInConwayPParams era
  = IntroducedInConwayPParams
  { icPoolVotingThresholds :: StrictMaybe Ledger.PoolVotingThresholds
  , icDRepVotingThresholds :: StrictMaybe Ledger.DRepVotingThresholds
  , icMinCommitteeSize :: StrictMaybe Word16
  , icCommitteeTermLength :: StrictMaybe Ledger.EpochInterval
  , icGovActionLifetime :: StrictMaybe Ledger.EpochInterval
  , icGovActionDeposit :: StrictMaybe Ledger.Coin
  , icDRepDeposit :: StrictMaybe Ledger.Coin
  , icDRepActivity :: StrictMaybe Ledger.EpochInterval
  , icMinFeeRefScriptCostPerByte :: StrictMaybe Ledger.NonNegativeInterval
  }
  deriving (Eq, Show)

createIntroducedInConwayPParams
  :: Ledger.ConwayEraPParams ledgerera
  => IntroducedInConwayPParams ledgerera
  -> Ledger.PParamsUpdate ledgerera
createIntroducedInConwayPParams IntroducedInConwayPParams{..} =
  Ledger.emptyPParamsUpdate
    & Ledger.ppuPoolVotingThresholdsL .~ icPoolVotingThresholds
    & Ledger.ppuDRepVotingThresholdsL .~ icDRepVotingThresholds
    & Ledger.ppuCommitteeMinSizeL .~ icMinCommitteeSize
    & Ledger.ppuCommitteeMaxTermLengthL .~ icCommitteeTermLength
    & Ledger.ppuGovActionLifetimeL .~ icGovActionLifetime
    & Ledger.ppuGovActionDepositL .~ icGovActionDeposit
    & Ledger.ppuDRepDepositL .~ icDRepDeposit
    & Ledger.ppuDRepActivityL .~ icDRepActivity
    & Ledger.ppuMinFeeRefScriptCostPerByteL .~ icMinFeeRefScriptCostPerByte

pparamsUpdateToIntroducedInConwayPParams
  :: Ledger.ConwayEraPParams ledgerera
  => Ledger.PParamsUpdate ledgerera
  -> IntroducedInConwayPParams ledgerera
pparamsUpdateToIntroducedInConwayPParams ppupdate =
  IntroducedInConwayPParams
    { icPoolVotingThresholds = ppupdate ^. Ledger.ppuPoolVotingThresholdsL
    , icDRepVotingThresholds = ppupdate ^. Ledger.ppuDRepVotingThresholdsL
    , icMinCommitteeSize = ppupdate ^. Ledger.ppuCommitteeMinSizeL
    , icCommitteeTermLength = ppupdate ^. Ledger.ppuCommitteeMaxTermLengthL
    , icGovActionLifetime = ppupdate ^. Ledger.ppuGovActionLifetimeL
    , icGovActionDeposit = ppupdate ^. Ledger.ppuGovActionDepositL
    , icDRepDeposit = ppupdate ^. Ledger.ppuDRepDepositL
    , icDRepActivity = ppupdate ^. Ledger.ppuDRepActivityL
    , icMinFeeRefScriptCostPerByte = ppupdate ^. Ledger.ppuMinFeeRefScriptCostPerByteL
    }

data IntroducedInDijkstraPParams era
  = IntroducedInDijkstraPParams
  { idMaxRefScriptSizePerBlock :: StrictMaybe Word32
  , idMaxRefScriptSizePerTx :: StrictMaybe Word32
  , idRefScriptCostStride :: StrictMaybe (Ledger.NonZero Word32)
  , idRefScriptCostMultiplier :: StrictMaybe Ledger.PositiveInterval
  }
  deriving (Eq, Show)

createIntroducedInDijkstraPParams
  :: (Ledger.ConwayEraPParams ledgerera, Ledger.DijkstraEraPParams ledgerera)
  => IntroducedInDijkstraPParams ledgerera
  -> Ledger.PParamsUpdate ledgerera
createIntroducedInDijkstraPParams IntroducedInDijkstraPParams{..} =
  Ledger.emptyPParamsUpdate
    & Ledger.ppuMaxRefScriptSizePerBlockL .~ idMaxRefScriptSizePerBlock
    & Ledger.ppuMaxRefScriptSizePerTxL .~ idMaxRefScriptSizePerTx
    & Ledger.ppuRefScriptCostStrideL .~ idRefScriptCostStride
    & Ledger.ppuRefScriptCostMultiplierL .~ idRefScriptCostMultiplier

pparamsUpdateToIntroducedInDijkstraPParams
  :: Ledger.DijkstraEraPParams ledgerera
  => Ledger.PParamsUpdate ledgerera
  -> IntroducedInDijkstraPParams ledgerera
pparamsUpdateToIntroducedInDijkstraPParams ppupdate =
  IntroducedInDijkstraPParams
    { idMaxRefScriptSizePerBlock = ppupdate ^. Ledger.ppuMaxRefScriptSizePerBlockL
    , idMaxRefScriptSizePerTx = ppupdate ^. Ledger.ppuMaxRefScriptSizePerTxL
    , idRefScriptCostStride = ppupdate ^. Ledger.ppuRefScriptCostStrideL
    , idRefScriptCostMultiplier = ppupdate ^. Ledger.ppuRefScriptCostMultiplierL
    }

createEraBasedProtocolParamUpdate
  :: ShelleyBasedEra era
  -> EraBasedProtocolParametersUpdate era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createEraBasedProtocolParamUpdate sbe eraPParamsUpdate =
  case eraPParamsUpdate of
    ShelleyEraBasedProtocolParametersUpdate c depAfterMary depAfterBabbage depAfterAlonzo ->
      let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
          Ledger.PParamsUpdate withProtVer = createPreConwayProtocolVersionUpdate depAfterBabbage
          Ledger.PParamsUpdate depAfterMary' = createDeprecatedAfterMaryPParams sbe depAfterMary
          Ledger.PParamsUpdate depAfterAlonzo' = createDeprecatedAfterAlonzoPParams sbe depAfterAlonzo
       in Ledger.PParamsUpdate $ common <> withProtVer <> depAfterMary' <> depAfterAlonzo'
    AllegraEraBasedProtocolParametersUpdate c depAfterMary depAfterAlonzo depAfterBabbage ->
      let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
          Ledger.PParamsUpdate withProtVer = createPreConwayProtocolVersionUpdate depAfterBabbage
          Ledger.PParamsUpdate depAfterMary' = createDeprecatedAfterMaryPParams sbe depAfterMary
          Ledger.PParamsUpdate depAfterAlonzo' = createDeprecatedAfterAlonzoPParams sbe depAfterAlonzo
       in Ledger.PParamsUpdate $ common <> withProtVer <> depAfterMary' <> depAfterAlonzo'
    MaryEraBasedProtocolParametersUpdate c depAfterMary depAfterAlonzo depAfterBabbage ->
      let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
          Ledger.PParamsUpdate withProtVer = createPreConwayProtocolVersionUpdate depAfterBabbage
          Ledger.PParamsUpdate depAfterMary' = createDeprecatedAfterMaryPParams sbe depAfterMary
          Ledger.PParamsUpdate depAfterAlonzo' = createDeprecatedAfterAlonzoPParams sbe depAfterAlonzo
       in Ledger.PParamsUpdate $ common <> withProtVer <> depAfterMary' <> depAfterAlonzo'
    AlonzoEraBasedProtocolParametersUpdate c depAfterAlonzoA introInAlon depAfterBabbage ->
      let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
          Ledger.PParamsUpdate withProtVer = createPreConwayProtocolVersionUpdate depAfterBabbage
          Ledger.PParamsUpdate preAl' = createPParamsUpdateIntroducedInAlonzo AlonzoEraOnwardsAlonzo introInAlon
          Ledger.PParamsUpdate depAfterAlonzoA' = createDeprecatedAfterAlonzoPParams sbe depAfterAlonzoA
       in Ledger.PParamsUpdate $ common <> withProtVer <> preAl' <> depAfterAlonzoA'
    BabbageEraBasedProtocolParametersUpdate c introInAlonzo depAfterBabbage introInBabbage ->
      let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
          Ledger.PParamsUpdate withProtVer = createPreConwayProtocolVersionUpdate depAfterBabbage
          Ledger.PParamsUpdate inAlonzoPParams = createPParamsUpdateIntroducedInAlonzo AlonzoEraOnwardsBabbage introInAlonzo
          Ledger.PParamsUpdate inBAb = createIntroducedInBabbagePParams BabbageEraOnwardsBabbage introInBabbage
       in Ledger.PParamsUpdate $ common <> withProtVer <> inAlonzoPParams <> inBAb
    ConwayEraBasedProtocolParametersUpdate c introInAlonzo introInBabbage introInConway ->
      let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
          Ledger.PParamsUpdate inAlonzoPParams = createPParamsUpdateIntroducedInAlonzo AlonzoEraOnwardsConway introInAlonzo
          Ledger.PParamsUpdate inBab = createIntroducedInBabbagePParams BabbageEraOnwardsConway introInBabbage
          Ledger.PParamsUpdate inCon = createIntroducedInConwayPParams introInConway
       in Ledger.PParamsUpdate $ common <> inAlonzoPParams <> inBab <> inCon
    DijkstraEraBasedProtocolParametersUpdate
      c
      introInAlonzo
      introInBabbage
      introInConway
      introInDijkstra ->
        let Ledger.PParamsUpdate common = createCommonPParamsUpdate c
            Ledger.PParamsUpdate inAlonzoPParams = createPParamsUpdateIntroducedInAlonzo AlonzoEraOnwardsDijkstra introInAlonzo
            Ledger.PParamsUpdate inBab = createIntroducedInBabbagePParams BabbageEraOnwardsDijkstra introInBabbage
            Ledger.PParamsUpdate inCon = createIntroducedInConwayPParams introInConway
            Ledger.PParamsUpdate inDij = createIntroducedInDijkstraPParams introInDijkstra
         in Ledger.PParamsUpdate $ common <> inAlonzoPParams <> inBab <> inCon <> inDij

-- | Protocol parameters common to each era. This can only ever be reduced
-- if parameters are deprecated.
data CommonProtocolParametersUpdate
  = CommonProtocolParametersUpdate
  { cppTxFeePerByteL :: StrictMaybe Ledger.CoinPerByte
  , cppTxFeeFixedL :: StrictMaybe Ledger.Coin
  , cppMaxBlockBodySize :: StrictMaybe Word32
  , cppMaxTxSize :: StrictMaybe Word32
  , cppMaxBlockHeaderSize :: StrictMaybe Word16
  , cppKeyDeposit :: StrictMaybe Ledger.Coin
  , cppPoolDeposit :: StrictMaybe Ledger.Coin
  , cppPoolRetireMaxEpoch :: StrictMaybe Ledger.EpochInterval
  , cppStakePoolTargetNum :: StrictMaybe Word16
  , cppPoolPledgeInfluence :: StrictMaybe Ledger.NonNegativeInterval
  , cppTreasuryExpansion :: StrictMaybe Ledger.UnitInterval
  , cppMonetaryExpansion :: StrictMaybe Ledger.UnitInterval
  , cppMinPoolCost :: StrictMaybe Ledger.Coin
  }
  deriving (Eq, Show)

-- | Create a protocol parameters update with parameters common to all eras
createCommonPParamsUpdate
  :: EraPParams ledgerera => CommonProtocolParametersUpdate -> Ledger.PParamsUpdate ledgerera
createCommonPParamsUpdate CommonProtocolParametersUpdate{..} =
  emptyPParamsUpdate
    & Ledger.ppuTxFeePerByteL .~ cppTxFeePerByteL
    & Ledger.ppuTxFeeFixedL .~ cppTxFeeFixedL
    & Ledger.ppuMaxBBSizeL .~ cppMaxBlockBodySize
    & Ledger.ppuMaxTxSizeL .~ cppMaxTxSize
    & Ledger.ppuMaxBHSizeL .~ cppMaxBlockHeaderSize
    & Ledger.ppuKeyDepositL .~ cppKeyDeposit
    & Ledger.ppuPoolDepositL .~ cppPoolDeposit
    & Ledger.ppuEMaxL .~ cppPoolRetireMaxEpoch
    & Ledger.ppuNOptL .~ cppStakePoolTargetNum
    & Ledger.ppuA0L .~ cppPoolPledgeInfluence
    & Ledger.ppuTauL .~ cppTreasuryExpansion
    & Ledger.ppuRhoL .~ cppMonetaryExpansion
    & Ledger.ppuMinPoolCostL .~ cppMinPoolCost

pparamsUpdateToCommonParametersUpdate
  :: ShelleyBasedEra era -> Ledger.PParamsUpdate (ShelleyLedgerEra era) -> CommonProtocolParametersUpdate
pparamsUpdateToCommonParametersUpdate sbe pparamsUpdate =
  shelleyBasedEraConstraints sbe $
    CommonProtocolParametersUpdate
      { cppTxFeePerByteL = pparamsUpdate ^. Ledger.ppuTxFeePerByteL
      , cppTxFeeFixedL = pparamsUpdate ^. Ledger.ppuTxFeeFixedL
      , cppMaxBlockBodySize = pparamsUpdate ^. Ledger.ppuMaxBBSizeL
      , cppMaxTxSize = pparamsUpdate ^. Ledger.ppuMaxTxSizeL
      , cppMaxBlockHeaderSize = pparamsUpdate ^. Ledger.ppuMaxBHSizeL
      , cppKeyDeposit = pparamsUpdate ^. Ledger.ppuKeyDepositL
      , cppPoolDeposit = pparamsUpdate ^. Ledger.ppuPoolDepositL
      , cppPoolRetireMaxEpoch = pparamsUpdate ^. Ledger.ppuEMaxL
      , cppStakePoolTargetNum = pparamsUpdate ^. Ledger.ppuNOptL
      , cppPoolPledgeInfluence = pparamsUpdate ^. Ledger.ppuA0L
      , cppTreasuryExpansion = pparamsUpdate ^. Ledger.ppuTauL
      , cppMonetaryExpansion = pparamsUpdate ^. Ledger.ppuRhoL
      , cppMinPoolCost = pparamsUpdate ^. Ledger.ppuMinPoolCostL
      }

-- | Updating protocol version with PParamUpdate is being prevented in Conway
-- (via the `ProtVerAtMost era 8` constraint in `ppuProtocolVersionL`).
-- As a consequence, ppuProtocolVersionL cannot be used in `createCommonPParamsUpdate`,
-- as was the case pre-Conway.
-- Here we isolate the usage of the lens, so that it can be used in each pre-conway era
-- when creating `Ledger.PParamsUpdate` within `createEraBasedProtocolParamUpdate`.
createPreConwayProtocolVersionUpdate
  :: (EraPParams ledgerera, Ledger.ProtVerAtMost ledgerera 8)
  => DeprecatedAfterBabbagePParams cppProtocolVersion
  -> Ledger.PParamsUpdate ledgerera
createPreConwayProtocolVersionUpdate (DeprecatedAfterBabbagePParams cppProtocolVersion) =
  Ledger.emptyPParamsUpdate & Ledger.ppuProtocolVersionL .~ cppProtocolVersion

newtype DeprecatedAfterBabbagePParams ledgerera
  = DeprecatedAfterBabbagePParams (StrictMaybe Ledger.ProtVer)
  deriving (Eq, Show)

type MaxBabbageEra ledgerera = Ledger.ProtVerAtMost ledgerera 8

pparamsUpdateToDeprecatedAfterBabbagePParams
  :: MaxBabbageEra (ShelleyLedgerEra era)
  => EraPParams (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
  -> DeprecatedAfterBabbagePParams ShelleyEra
pparamsUpdateToDeprecatedAfterBabbagePParams _ ppupdate =
  DeprecatedAfterBabbagePParams $
    ppupdate ^. Ledger.ppuProtocolVersionL

type MaxMaryEra ledgerera = Ledger.ProtVerAtMost ledgerera 4

newtype DeprecatedAfterMaryPParams ledgerera
  = DeprecatedAfterMaryPParams (StrictMaybe Ledger.Coin) -- Minimum UTxO value
  deriving (Eq, Show)

createDeprecatedAfterMaryPParams
  :: EraPParams (ShelleyLedgerEra era)
  => MaxMaryEra (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> DeprecatedAfterMaryPParams era -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createDeprecatedAfterMaryPParams _ (DeprecatedAfterMaryPParams minUtxoVal) =
  Ledger.emptyPParamsUpdate & Ledger.ppuMinUTxOValueL .~ minUtxoVal

pparamsUpdateToDeprecatedAfterMaryPParams
  :: MaxMaryEra (ShelleyLedgerEra era)
  => EraPParams (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> Ledger.PParamsUpdate (ShelleyLedgerEra era) -> DeprecatedAfterMaryPParams era
pparamsUpdateToDeprecatedAfterMaryPParams _ ppupdate =
  DeprecatedAfterMaryPParams $
    ppupdate ^. Ledger.ppuMinUTxOValueL

data ShelleyToAlonzoPParams ledgerera
  = ShelleyToAlonzoPParams
      (StrictMaybe Ledger.Nonce)
      -- ^ Extra entropy
      (StrictMaybe Ledger.UnitInterval)
      -- ^ Decentralization parameter
  deriving (Eq, Show)

type MaxAlonzoEra ledgerera = Ledger.ProtVerAtMost ledgerera 6

createDeprecatedAfterAlonzoPParams
  :: EraPParams (ShelleyLedgerEra era)
  => MaxAlonzoEra (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> ShelleyToAlonzoPParams era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createDeprecatedAfterAlonzoPParams _ (ShelleyToAlonzoPParams extraEntropy decentralization) =
  Ledger.emptyPParamsUpdate
    & Ledger.ppuExtraEntropyL .~ extraEntropy
    & Ledger.ppuDL .~ decentralization

pparamsUpdateToShelleyToAlonzoPParams
  :: EraPParams (ShelleyLedgerEra era)
  => MaxAlonzoEra (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
  -> ShelleyToAlonzoPParams era
pparamsUpdateToShelleyToAlonzoPParams _ pparamUpdate =
  ShelleyToAlonzoPParams
    (pparamUpdate ^. Ledger.ppuExtraEntropyL)
    (pparamUpdate ^. Ledger.ppuDL)

data AlonzoOnwardsPParams ledgerera
  = AlonzoOnwardsPParams
  { alCostModels :: StrictMaybe Alonzo.CostModels
  , alPrices :: StrictMaybe Alonzo.Prices
  , alMaxTxExUnits :: StrictMaybe Alonzo.ExUnits
  , alMaxBlockExUnits :: StrictMaybe Alonzo.ExUnits
  , alMaxValSize :: StrictMaybe Word32
  , alCollateralPercentage :: StrictMaybe Word16
  , alMaxCollateralInputs :: StrictMaybe Word16
  }
  deriving (Eq, Show)

createPParamsUpdateIntroducedInAlonzo
  :: ()
  => AlonzoEraOnwards era
  -> AlonzoOnwardsPParams era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createPParamsUpdateIntroducedInAlonzo w (AlonzoOnwardsPParams{..}) =
  alonzoEraOnwardsConstraints w $
    Ledger.emptyPParamsUpdate
      & Ledger.ppuCostModelsL .~ alCostModels
      & Ledger.ppuPricesL .~ alPrices
      & Ledger.ppuMaxTxExUnitsL .~ alMaxTxExUnits
      & Ledger.ppuMaxBlockExUnitsL .~ alMaxBlockExUnits
      & Ledger.ppuMaxValSizeL .~ alMaxValSize
      & Ledger.ppuCollateralPercentageL .~ alCollateralPercentage
      & Ledger.ppuMaxCollateralInputsL .~ alMaxCollateralInputs

pparamsUpdateToAlonzoOnwardsPParams
  :: AlonzoEraOnwards era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
  -> AlonzoOnwardsPParams era
pparamsUpdateToAlonzoOnwardsPParams w ppupdate =
  alonzoEraOnwardsConstraints w $
    AlonzoOnwardsPParams
      { alCostModels = ppupdate ^. Ledger.ppuCostModelsL
      , alPrices = ppupdate ^. Ledger.ppuPricesL
      , alMaxTxExUnits = ppupdate ^. Ledger.ppuMaxTxExUnitsL
      , alMaxBlockExUnits = ppupdate ^. Ledger.ppuMaxBlockExUnitsL
      , alMaxValSize = ppupdate ^. Ledger.ppuMaxValSizeL
      , alCollateralPercentage = ppupdate ^. Ledger.ppuCollateralPercentageL
      , alMaxCollateralInputs = ppupdate ^. Ledger.ppuMaxCollateralInputsL
      }

newtype IntroducedInBabbagePParams era
  = -- | Coins per UTxO byte
    IntroducedInBabbagePParams
      (StrictMaybe CoinPerByte)
  deriving (Eq, Show)

createIntroducedInBabbagePParams
  :: ()
  => BabbageEraOnwards era
  -> IntroducedInBabbagePParams era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createIntroducedInBabbagePParams w (IntroducedInBabbagePParams coinsPerUTxOByte) =
  babbageEraOnwardsConstraints w $
    Ledger.emptyPParamsUpdate & Ledger.ppuCoinsPerUTxOByteL .~ coinsPerUTxOByte

pparamsUpdateToIntroducedInBabbagePParams
  :: BabbageEraOnwards era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
  -> IntroducedInBabbagePParams era
pparamsUpdateToIntroducedInBabbagePParams w ppupdate =
  babbageEraOnwardsConstraints w $
    IntroducedInBabbagePParams (ppupdate ^. Ledger.ppuCoinsPerUTxOByteL)

-- ----------------------------------------------------------------------------
-- Proposals embedded in transactions to update protocol parameters
--

data UpdateProposal era
  = UpdateProposal
      !(Map (Hash GenesisKey) (EraBasedProtocolParametersUpdate era))
      !EpochNo
  deriving stock (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance Typeable era => HasTypeProxy (UpdateProposal era) where
  data AsType (UpdateProposal era) = AsUpdateProposal
  proxyToAsType _ = AsUpdateProposal

instance IsShelleyBasedEra era => HasTextEnvelope (UpdateProposal era) where
  textEnvelopeType _ = "UpdateProposalShelley"

instance IsShelleyBasedEra era => ToCBOR (UpdateProposal era) where
  toCBOR (UpdateProposal ppup epochno) =
    CBOR.encodeListLen 2
      <> toCBOR ppup
      <> toCBOR epochno

instance IsShelleyBasedEra era => FromCBOR (UpdateProposal era) where
  fromCBOR = do
    CBOR.enforceSize "UpdateProposal" 2
    UpdateProposal
      <$> fromCBOR
      <*> fromCBOR

makeShelleyUpdateProposal
  :: EraBasedProtocolParametersUpdate era
  -> [Hash GenesisKey]
  -> EpochNo
  -> UpdateProposal era
makeShelleyUpdateProposal params genesisKeyHashes =
  -- TODO decide how to handle parameter validation
  --     for example we need to validate the Rational values can convert
  --     into the UnitInterval type ok.
  UpdateProposal (fromList [(kh, params) | kh <- genesisKeyHashes])

-- ----------------------------------------------------------------------------
-- Conversion functions: updates to ledger types
--

toLedgerUpdate
  :: ()
  => ShelleyBasedEra era
  -> UpdateProposal era
  -> Ledger.Update (ShelleyLedgerEra era)
toLedgerUpdate sbe (UpdateProposal ppup epochno) =
  Ledger.Update (toLedgerProposedPPUpdates sbe ppup) epochno

toLedgerProposedPPUpdates
  :: ()
  => ShelleyBasedEra era
  -> Map (Hash GenesisKey) (EraBasedProtocolParametersUpdate era)
  -> Ledger.ProposedPPUpdates (ShelleyLedgerEra era)
toLedgerProposedPPUpdates sbe m =
  Ledger.ProposedPPUpdates $
    Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh) $
      Map.map (createEraBasedProtocolParamUpdate sbe) m

-- ----------------------------------------------------------------------------
-- Conversion functions: updates from ledger types
--

fromLedgerUpdate
  :: forall era ledgerera
   . ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Ledger.Update ledgerera
  -> UpdateProposal era
fromLedgerUpdate sbe (Ledger.Update ppup epochno) =
  UpdateProposal (fromLedgerProposedPPUpdates sbe ppup) epochno

fromLedgerProposedPPUpdates
  :: forall era ledgerera
   . ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Ledger.ProposedPPUpdates ledgerera
  -> Map (Hash GenesisKey) (EraBasedProtocolParametersUpdate era)
fromLedgerProposedPPUpdates sbe =
  Map.map (fromLedgerPParamsUpdate sbe)
    . Map.mapKeysMonotonic GenesisKeyHash
    . (\(Ledger.ProposedPPUpdates ppup) -> ppup)

fromLedgerPParamsUpdate
  :: ShelleyBasedEra era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
  -> EraBasedProtocolParametersUpdate era
fromLedgerPParamsUpdate sbe ppup =
  let common = pparamsUpdateToCommonParametersUpdate sbe ppup
   in case sbe of
        ShelleyBasedEraShelley ->
          let depAfterMary = pparamsUpdateToDeprecatedAfterMaryPParams sbe ppup
              sToAPParamsUpdate = pparamsUpdateToShelleyToAlonzoPParams sbe ppup
              depAfterBabbage = pparamsUpdateToDeprecatedAfterBabbagePParams sbe ppup
           in ShelleyEraBasedProtocolParametersUpdate common depAfterMary depAfterBabbage sToAPParamsUpdate
        ShelleyBasedEraAllegra ->
          let depAfterMary = pparamsUpdateToDeprecatedAfterMaryPParams sbe ppup
              sToAPParamsUpdate = pparamsUpdateToShelleyToAlonzoPParams sbe ppup
              depAfterBabbage = pparamsUpdateToDeprecatedAfterBabbagePParams sbe ppup
           in AllegraEraBasedProtocolParametersUpdate common depAfterMary sToAPParamsUpdate depAfterBabbage
        ShelleyBasedEraMary ->
          let depAfterMary = pparamsUpdateToDeprecatedAfterMaryPParams sbe ppup
              sToAPParamsUpdate = pparamsUpdateToShelleyToAlonzoPParams sbe ppup
              depAfterBabbage = pparamsUpdateToDeprecatedAfterBabbagePParams sbe ppup
           in MaryEraBasedProtocolParametersUpdate common depAfterMary sToAPParamsUpdate depAfterBabbage
        ShelleyBasedEraAlonzo ->
          let sToAPParamsUpdate = pparamsUpdateToShelleyToAlonzoPParams sbe ppup
              depAfterBabbage = pparamsUpdateToDeprecatedAfterBabbagePParams sbe ppup
              introInAlonzo = pparamsUpdateToAlonzoOnwardsPParams AlonzoEraOnwardsAlonzo ppup
           in AlonzoEraBasedProtocolParametersUpdate common sToAPParamsUpdate introInAlonzo depAfterBabbage
        ShelleyBasedEraBabbage ->
          let depAfterBabbage = pparamsUpdateToDeprecatedAfterBabbagePParams sbe ppup
              introInAlonzo = pparamsUpdateToAlonzoOnwardsPParams AlonzoEraOnwardsBabbage ppup
              introInBabbage = pparamsUpdateToIntroducedInBabbagePParams BabbageEraOnwardsBabbage ppup
           in BabbageEraBasedProtocolParametersUpdate common introInAlonzo depAfterBabbage introInBabbage
        ShelleyBasedEraConway ->
          let introInAlonzo = pparamsUpdateToAlonzoOnwardsPParams AlonzoEraOnwardsConway ppup
              introInBabbage = pparamsUpdateToIntroducedInBabbagePParams BabbageEraOnwardsConway ppup
              introInConway = pparamsUpdateToIntroducedInConwayPParams ppup
           in ConwayEraBasedProtocolParametersUpdate common introInAlonzo introInBabbage introInConway
        ShelleyBasedEraDijkstra ->
          let introInAlonzo = pparamsUpdateToAlonzoOnwardsPParams AlonzoEraOnwardsDijkstra ppup
              introInBabbage = pparamsUpdateToIntroducedInBabbagePParams BabbageEraOnwardsDijkstra ppup
              introInConway = pparamsUpdateToIntroducedInConwayPParams ppup
              introInDijkstra = pparamsUpdateToIntroducedInDijkstraPParams ppup
           in DijkstraEraBasedProtocolParametersUpdate
                common
                introInAlonzo
                introInBabbage
                introInConway
                introInDijkstra
