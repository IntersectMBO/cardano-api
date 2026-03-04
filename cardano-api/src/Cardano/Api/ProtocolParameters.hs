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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | The various Cardano protocol parameters, including:
--
-- * the current values of updatable protocol parameters: 'ProtocolParameters'
-- * updates to protocol parameters: 'ProtocolParametersUpdate'
-- * update proposals that can be embedded in transactions: 'UpdateProposal'
-- * parameters fixed in the genesis file: 'GenesisParameters'
module Cardano.Api.ProtocolParameters
  ( -- * The updatable protocol parameters
    EpochNo

    -- * The updatable protocol parameters
  , LedgerProtocolParameters (..)
  , EraBasedProtocolParametersUpdate (..)
  , AlonzoOnwardsPParams (..)
  , CommonProtocolParametersUpdate (..)
  , DeprecatedAfterBabbagePParams (..)
  , DeprecatedAfterMaryPParams (..)
  , ShelleyToAlonzoPParams (..)
  , IntroducedInBabbagePParams (..)
  , IntroducedInConwayPParams (..)
  , createEraBasedProtocolParamUpdate
  , createPParams

    -- * Errors
  , ProtocolParametersError (..)
  , ProtocolParametersConversionError (..)

    -- * PraosNonce
  , PraosNonce
  , makePraosNonce

    -- * Execution units, prices and cost models,
  , ExecutionUnits (..)
  , ExecutionUnitPrices (..)
  , CostModels (..)
  , CostModel (..)
  , fromAlonzoCostModels

    -- * Update proposals to change the protocol parameters
  , UpdateProposal (..)
  , makeShelleyUpdateProposal

    -- * Internal conversion functions
  , toLedgerNonce
  , toAlonzoPrices
  , fromAlonzoPrices
  , toAlonzoScriptLanguage
  , fromAlonzoScriptLanguage
  , toAlonzoCostModel
  , fromAlonzoCostModel
  , toAlonzoCostModels
  , fromLedgerUpdate

    -- * Data family instances
  , AsType (..)

    -- ** Era-dependent protocol features
  )
where

import Cardano.Api.Address
import Cardano.Api.Byron.Internal.Key
import Cardano.Api.Certificate.Internal.StakePoolMetadata
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Key.Internal
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Json (toRationalJSON)
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing
import Cardano.Api.Serialise.TextEnvelope.Internal
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal

import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api.Era qualified as Ledger
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.Babbage.Core qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Plutus.CostModels qualified as Plutus
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.Slot (EpochNo (..))
import PlutusLedgerApi.Common (CostModelApplyError)

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Either.Combinators (maybeToRight)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Typeable
import Data.Word
import GHC.Exts (IsList (..))
import GHC.Generics
import Lens.Micro
import Numeric.Natural
import Text.PrettyBy.Default (display)

-- -----------------------------------------------------------------------------
-- Era based ledger protocol parameters
--
newtype LedgerProtocolParameters era = LedgerProtocolParameters
  { unLedgerProtocolParameters :: Ledger.PParams (ShelleyLedgerEra era)
  }

instance IsShelleyBasedEra era => Show (LedgerProtocolParameters era) where
  show (LedgerProtocolParameters pp) =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      show pp

instance IsShelleyBasedEra era => Eq (LedgerProtocolParameters era) where
  LedgerProtocolParameters a == LedgerProtocolParameters b =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      a == b

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

deriving instance Show (EraBasedProtocolParametersUpdate era)

deriving instance Eq (EraBasedProtocolParametersUpdate era)

instance Typeable era => ToCBOR (EraBasedProtocolParametersUpdate era) where
  toCBOR = error "toCBOR not implemented for EraBasedProtocolParametersUpdate"

instance Typeable era => FromCBOR (EraBasedProtocolParametersUpdate era) where
  fromCBOR = error "fromCBOR not implemented for EraBasedProtocolParametersUpdate"

data IntroducedInConwayPParams era
  = IntroducedInConwayPParams
  { icPoolVotingThresholds :: StrictMaybe Ledger.PoolVotingThresholds
  , icDRepVotingThresholds :: StrictMaybe Ledger.DRepVotingThresholds
  , icMinCommitteeSize :: StrictMaybe Natural
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

-- | Protocol parameters common to each era. This can only ever be reduced
-- if parameters are deprecated.
data CommonProtocolParametersUpdate
  = CommonProtocolParametersUpdate
  { cppMinFeeA :: StrictMaybe Ledger.Coin
  , cppMinFeeB :: StrictMaybe Ledger.Coin
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
    & Ledger.ppuMinFeeAL .~ cppMinFeeA
    & Ledger.ppuMinFeeBL .~ cppMinFeeB
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
      { cppMinFeeA = pparamsUpdate ^. Ledger.ppuMinFeeAL
      , cppMinFeeB = pparamsUpdate ^. Ledger.ppuMinFeeBL
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
  , alMaxValSize :: StrictMaybe Natural
  , alCollateralPercentage :: StrictMaybe Natural
  , alMaxCollateralInputs :: StrictMaybe Natural
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

-- | The values of the set of /updatable/ protocol parameters. At any
-- particular point on the chain there is a current set of parameters in use.
--
-- These parameters can be updated (at epoch boundaries) via an
-- 'UpdateProposal', which contains a 'ProtocolParametersUpdate'.
--
-- The 'ProtocolParametersUpdate' is essentially a diff for the
-- 'ProtocolParameters'.
--
-- There are also parameters fixed in the Genesis file. See 'GenesisParameters'.
{-# DEPRECATED
  ProtocolParameters
  "Use the ledger's PParams (from module Cardano.Api.Ledger) type instead of ProtocolParameters. The type will be removed after Chang hard fork."
  #-}

data ProtocolParameters
  = ProtocolParameters
  { protocolParamProtocolVersion :: (Natural, Natural)
  -- ^ Protocol version, major and minor. Updating the major version is
  -- used to trigger hard forks.
  --                              (Major  , Minor  )
  , protocolParamDecentralization :: Maybe Rational
  -- ^ The decentralization parameter. This is fraction of slots that
  -- belong to the BFT overlay schedule, rather than the Praos schedule.
  -- So 1 means fully centralised, while 0 means fully decentralised.
  --
  -- This is the \"d\" parameter from the design document.
  --
  -- /Deprecated in Babbage/
  , protocolParamExtraPraosEntropy :: Maybe PraosNonce
  -- ^ Extra entropy for the Praos per-epoch nonce.
  --
  -- This can be used to add extra entropy during the decentralisation
  -- process. If the extra entropy can be demonstrated to be generated
  -- randomly then this method can be used to show that the initial
  -- federated operators did not subtly bias the initial schedule so that
  -- they retain undue influence after decentralisation.
  , protocolParamMaxBlockHeaderSize :: Natural
  -- ^ The maximum permitted size of a block header.
  --
  -- This must be at least as big as the largest legitimate block headers
  -- but should not be too much larger, to help prevent DoS attacks.
  --
  -- Caution: setting this to be smaller than legitimate block headers is
  -- a sure way to brick the system!
  , protocolParamMaxBlockBodySize :: Natural
  -- ^ The maximum permitted size of the block body (that is, the block
  -- payload, without the block header).
  --
  -- This should be picked with the Praos network delta security parameter
  -- in mind. Making this too large can severely weaken the Praos
  -- consensus properties.
  --
  -- Caution: setting this to be smaller than a transaction that can
  -- change the protocol parameters is a sure way to brick the system!
  , protocolParamMaxTxSize :: Natural
  -- ^ The maximum permitted size of a transaction.
  --
  -- Typically this should not be too high a fraction of the block size,
  -- otherwise wastage from block fragmentation becomes a problem, and
  -- the current implementation does not use any sophisticated box packing
  -- algorithm.
  , protocolParamTxFeeFixed :: L.Coin
  -- ^ The constant factor for the minimum fee calculation.
  , protocolParamTxFeePerByte :: L.Coin
  -- ^ Per byte linear factor for the minimum fee calculation.
  , protocolParamMinUTxOValue :: Maybe L.Coin
  -- ^ The minimum permitted value for new UTxO entries, ie for
  -- transaction outputs.
  , protocolParamStakeAddressDeposit :: L.Coin
  -- ^ The deposit required to register a stake address.
  , protocolParamStakePoolDeposit :: L.Coin
  -- ^ The deposit required to register a stake pool.
  , protocolParamMinPoolCost :: L.Coin
  -- ^ The minimum value that stake pools are permitted to declare for
  -- their cost parameter.
  , protocolParamPoolRetireMaxEpoch :: Ledger.EpochInterval
  -- ^ The maximum number of epochs into the future that stake pools
  -- are permitted to schedule a retirement.
  , protocolParamStakePoolTargetNum :: Word16
  -- ^ The equilibrium target number of stake pools.
  --
  -- This is the \"k\" incentives parameter from the design document.
  , protocolParamPoolPledgeInfluence :: Rational
  -- ^ The influence of the pledge in stake pool rewards.
  --
  -- This is the \"a_0\" incentives parameter from the design document.
  , protocolParamMonetaryExpansion :: Rational
  -- ^ The monetary expansion rate. This determines the fraction of the
  -- reserves that are added to the fee pot each epoch.
  --
  -- This is the \"rho\" incentives parameter from the design document.
  , protocolParamTreasuryCut :: Rational
  -- ^ The fraction of the fee pot each epoch that goes to the treasury.
  --
  -- This is the \"tau\" incentives parameter from the design document.
  , protocolParamCostModels :: Map AnyPlutusScriptVersion CostModel
  -- ^ Cost models for script languages that use them.
  --
  -- /Introduced in Alonzo/
  , protocolParamPrices :: Maybe ExecutionUnitPrices
  -- ^ Price of execution units for script languages that use them.
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxTxExUnits :: Maybe ExecutionUnits
  -- ^ Max total script execution resources units allowed per tx
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxBlockExUnits :: Maybe ExecutionUnits
  -- ^ Max total script execution resources units allowed per block
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxValueSize :: Maybe Natural
  -- ^ Max size of a Value in a tx output.
  --
  -- /Introduced in Alonzo/
  , protocolParamCollateralPercent :: Maybe Natural
  -- ^ The percentage of the script contribution to the txfee that must be
  -- provided as collateral inputs when including Plutus scripts.
  --
  -- /Introduced in Alonzo/
  , protocolParamMaxCollateralInputs :: Maybe Natural
  -- ^ The maximum number of collateral inputs allowed in a transaction.
  --
  -- /Introduced in Alonzo/
  , protocolParamUTxOCostPerByte :: Maybe L.Coin
  -- ^ Cost in ada per byte of UTxO storage.
  --
  -- /Introduced in Babbage/
  }
  deriving (Eq, Generic, Show)

instance FromJSON ProtocolParameters where
  parseJSON =
    withObject "ProtocolParameters" $ \o -> do
      v <- o .: "protocolVersion"
      ProtocolParameters
        <$> ((,) <$> v .: "major" <*> v .: "minor")
        <*> o .:? "decentralization"
        <*> o .: "extraPraosEntropy"
        <*> o .: "maxBlockHeaderSize"
        <*> o .: "maxBlockBodySize"
        <*> o .: "maxTxSize"
        <*> o .: "txFeeFixed"
        <*> o .: "txFeePerByte"
        <*> o .: "minUTxOValue"
        <*> o .: "stakeAddressDeposit"
        <*> o .: "stakePoolDeposit"
        <*> o .: "minPoolCost"
        <*> o .: "poolRetireMaxEpoch"
        <*> o .: "stakePoolTargetNum"
        <*> o .: "poolPledgeInfluence"
        <*> o .: "monetaryExpansion"
        <*> o .: "treasuryCut"
        <*> (fmap unCostModels <$> o .:? "costModels") .!= Map.empty
        <*> o .:? "executionUnitPrices"
        <*> o .:? "maxTxExecutionUnits"
        <*> o .:? "maxBlockExecutionUnits"
        <*> o .:? "maxValueSize"
        <*> o .:? "collateralPercentage"
        <*> o .:? "maxCollateralInputs"
        <*> o .:? "utxoCostPerByte"

instance ToJSON ProtocolParameters where
  toJSON ProtocolParameters{..} =
    object
      [ "extraPraosEntropy" .= protocolParamExtraPraosEntropy
      , "stakePoolTargetNum" .= protocolParamStakePoolTargetNum
      , "minUTxOValue" .= protocolParamMinUTxOValue
      , "poolRetireMaxEpoch" .= protocolParamPoolRetireMaxEpoch
      , "decentralization" .= (toRationalJSON <$> protocolParamDecentralization)
      , "stakePoolDeposit" .= protocolParamStakePoolDeposit
      , "maxBlockHeaderSize" .= protocolParamMaxBlockHeaderSize
      , "maxBlockBodySize" .= protocolParamMaxBlockBodySize
      , "maxTxSize" .= protocolParamMaxTxSize
      , "treasuryCut" .= toRationalJSON protocolParamTreasuryCut
      , "minPoolCost" .= protocolParamMinPoolCost
      , "monetaryExpansion" .= toRationalJSON protocolParamMonetaryExpansion
      , "stakeAddressDeposit" .= protocolParamStakeAddressDeposit
      , "poolPledgeInfluence" .= toRationalJSON protocolParamPoolPledgeInfluence
      , "protocolVersion"
          .= let (major, minor) = protocolParamProtocolVersion
              in object ["major" .= major, "minor" .= minor]
      , "txFeeFixed" .= protocolParamTxFeeFixed
      , "txFeePerByte" .= protocolParamTxFeePerByte
      , -- Alonzo era:
        "costModels" .= CostModels protocolParamCostModels
      , "executionUnitPrices" .= protocolParamPrices
      , "maxTxExecutionUnits" .= protocolParamMaxTxExUnits
      , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits
      , "maxValueSize" .= protocolParamMaxValueSize
      , "collateralPercentage" .= protocolParamCollateralPercent
      , "maxCollateralInputs" .= protocolParamMaxCollateralInputs
      , -- Babbage era:
        "utxoCostPerByte" .= protocolParamUTxOCostPerByte
      ]

-- ----------------------------------------------------------------------------
-- Praos nonce
--

newtype PraosNonce = PraosNonce {unPraosNonce :: Hash.Hash HASH ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving (Show, Pretty) via UsingRawBytesHex PraosNonce
  deriving (ToJSON, FromJSON) via UsingRawBytesHex PraosNonce
  deriving (ToCBOR, FromCBOR) via UsingRawBytes PraosNonce

instance HasTypeProxy PraosNonce where
  data AsType PraosNonce = AsPraosNonce
  proxyToAsType _ = AsPraosNonce

instance SerialiseAsRawBytes PraosNonce where
  serialiseToRawBytes (PraosNonce h) =
    Crypto.hashToBytes h

  deserialiseFromRawBytes AsPraosNonce bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise PraosNonce") $
      PraosNonce <$> Crypto.hashFromBytes bs

makePraosNonce :: ByteString -> PraosNonce
makePraosNonce = PraosNonce . Crypto.hashWith id

toLedgerNonce :: Maybe PraosNonce -> Ledger.Nonce
toLedgerNonce Nothing = Ledger.NeutralNonce
toLedgerNonce (Just (PraosNonce h)) = Ledger.Nonce (Crypto.castHash h)

-- ----------------------------------------------------------------------------
-- Script execution unit prices and cost models
--

-- | The prices for 'ExecutionUnits' as a fraction of a 'L.Coin'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
data ExecutionUnitPrices
  = ExecutionUnitPrices
  { priceExecutionSteps :: Rational
  , priceExecutionMemory :: Rational
  }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnitPrices where
  toCBOR ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    CBOR.encodeListLen 2
      <> toCBOR priceExecutionSteps
      <> toCBOR priceExecutionMemory

instance FromCBOR ExecutionUnitPrices where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnitPrices" 2
    ExecutionUnitPrices
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnitPrices where
  toJSON ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    object
      [ "priceSteps" .= toRationalJSON priceExecutionSteps
      , "priceMemory" .= toRationalJSON priceExecutionMemory
      ]

instance FromJSON ExecutionUnitPrices where
  parseJSON =
    withObject "ExecutionUnitPrices" $ \o ->
      ExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"

toAlonzoPrices :: ExecutionUnitPrices -> Either ProtocolParametersConversionError Alonzo.Prices
toAlonzoPrices
  ExecutionUnitPrices
    { priceExecutionSteps
    , priceExecutionMemory
    } = do
    prSteps <- boundRationalEither "Steps" priceExecutionSteps
    prMem <- boundRationalEither "Mem" priceExecutionMemory
    return
      Alonzo.Prices
        { Alonzo.prSteps
        , Alonzo.prMem
        }

fromAlonzoPrices :: Alonzo.Prices -> ExecutionUnitPrices
fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} =
  ExecutionUnitPrices
    { priceExecutionSteps = Ledger.unboundRational prSteps
    , priceExecutionMemory = Ledger.unboundRational prMem
    }

-- ----------------------------------------------------------------------------
-- Script cost models
--

newtype CostModel = CostModel [Int64]
  deriving (Eq, Show, Data)
  deriving newtype (ToCBOR, FromCBOR)

newtype CostModels = CostModels {unCostModels :: Map AnyPlutusScriptVersion CostModel}
  deriving (Eq, Show)

instance FromJSON CostModels where
  parseJSON v = CostModels . fromAlonzoCostModels <$> parseJSON v

instance ToJSON CostModels where
  toJSON (CostModels costModels) =
    case toAlonzoCostModels costModels of
      Left err -> error $ displayError err
      Right ledgerCostModels -> toJSON ledgerCostModels

toAlonzoCostModels
  :: Map AnyPlutusScriptVersion CostModel
  -> Either ProtocolParametersConversionError Alonzo.CostModels
toAlonzoCostModels m = do
  f <- mapM conv $ toList m
  Right $ Plutus.mkCostModels $ fromList f
 where
  conv
    :: (AnyPlutusScriptVersion, CostModel)
    -> Either ProtocolParametersConversionError (Plutus.Language, Alonzo.CostModel)
  conv (anySVer, cModel) = do
    alonzoCostModel <- toAlonzoCostModel cModel (toAlonzoScriptLanguage anySVer)
    Right (toAlonzoScriptLanguage anySVer, alonzoCostModel)

fromAlonzoCostModels
  :: Plutus.CostModels
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels cModels =
  fromList
    . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
    $ toList
    $ Plutus.costModelsValid cModels

toAlonzoScriptLanguage :: AnyPlutusScriptVersion -> Plutus.Language
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Plutus.PlutusV1
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Plutus.PlutusV2
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV3) = Plutus.PlutusV3
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV4) = Plutus.PlutusV4

fromAlonzoScriptLanguage :: Plutus.Language -> AnyPlutusScriptVersion
fromAlonzoScriptLanguage Plutus.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoScriptLanguage Plutus.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2
fromAlonzoScriptLanguage Plutus.PlutusV3 = AnyPlutusScriptVersion PlutusScriptV3
fromAlonzoScriptLanguage Plutus.PlutusV4 = AnyPlutusScriptVersion PlutusScriptV4

toAlonzoCostModel
  :: CostModel -> Plutus.Language -> Either ProtocolParametersConversionError Alonzo.CostModel
toAlonzoCostModel (CostModel m) l = first (PpceInvalidCostModel (CostModel m)) $ Alonzo.mkCostModel l m

fromAlonzoCostModel :: Alonzo.CostModel -> CostModel
fromAlonzoCostModel m = CostModel $ Alonzo.getCostModelParams m

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

instance Typeable era => HasTextEnvelope (UpdateProposal era) where
  textEnvelopeType _ = "UpdateProposalShelley"

instance Typeable era => ToCBOR (UpdateProposal era) where
  toCBOR (UpdateProposal ppup epochno) =
    CBOR.encodeListLen 2
      <> toCBOR ppup
      <> toCBOR epochno

instance Typeable era => FromCBOR (UpdateProposal era) where
  fromCBOR = do
    CBOR.enforceSize "ProtocolParametersUpdate" 2
    UpdateProposal
      <$> fromCBOR
      <*> fromCBOR

-- TODO: LLeft off here. You need to implement the relevant FromCBOR instance
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

boundRationalEither
  :: Ledger.BoundedRational b
  => String
  -> Rational
  -> Either ProtocolParametersConversionError b
boundRationalEither name r = maybeToRight (PpceOutOfBounds name r) $ Ledger.boundRational r

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
          error "Dijkstra era should never be used with fromLedgerPParamsUpdate"

data ProtocolParametersError
  = PParamsErrorMissingMinUTxoValue !AnyCardanoEra
  | PParamsErrorMissingAlonzoProtocolParameter
  deriving Show

instance Error ProtocolParametersError where
  prettyError = \case
    PParamsErrorMissingMinUTxoValue (AnyCardanoEra era) ->
      mconcat
        [ "The " <> pretty era <> " protocol parameters value is missing the following "
        , "field: MinUTxoValue. Did you intend to use a " <> pretty era <> " protocol "
        , "parameters value?"
        ]
    PParamsErrorMissingAlonzoProtocolParameter ->
      mconcat
        [ "The Alonzo era protocol parameters in use is missing one or more of the "
        , "following fields: UTxOCostPerWord, CostModels, Prices, MaxTxExUnits, "
        , "MaxBlockExUnits, MaxValueSize, CollateralPercent, MaxCollateralInputs. Did "
        , "you intend to use an Alonzo era protocol parameters value?"
        ]

data ProtocolParametersConversionError
  = PpceOutOfBounds !ProtocolParameterName !Rational
  | PpceVersionInvalid !ProtocolParameterVersion
  | PpceInvalidCostModel !CostModel !CostModelApplyError
  | PpceMissingParameter !ProtocolParameterName
  deriving (Eq, Show, Data)

type ProtocolParameterName = String

type ProtocolParameterVersion = Natural

instance Error ProtocolParametersConversionError where
  prettyError = \case
    PpceOutOfBounds name r ->
      "Value for '" <> pretty name <> "' is outside of bounds: " <> pretty (fromRational r :: Double)
    PpceVersionInvalid majorProtVer ->
      "Major protocol version is invalid: " <> pretty majorProtVer
    PpceInvalidCostModel cm err ->
      "Invalid cost model: " <> pretty @Text (display err) <> " Cost model: " <> pshow cm
    PpceMissingParameter name ->
      "Missing parameter: " <> pretty name
