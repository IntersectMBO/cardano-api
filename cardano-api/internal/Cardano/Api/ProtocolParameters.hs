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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Redundant ==" -}
{- HLINT ignore "Use mapM" -}

-- | The various Cardano protocol parameters, including:
--
-- * the current values of updatable protocol parameters: 'ProtocolParameters'
-- * updates to protocol parameters: 'ProtocolParametersUpdate'
-- * update proposals that can be embedded in transactions: 'UpdateProposal'
-- * parameters fixed in the genesis file: 'GenesisParameters'
--
module Cardano.Api.ProtocolParameters (
    -- * The updatable protocol parameters
    ProtocolParameters(..),
    checkProtocolParameters,
    EpochNo,

    -- * The updatable protocol parameters
    LedgerProtocolParameters(..),
    EraBasedProtocolParametersUpdate(..),
    AlonzoOnwardsPParams(..),
    CommonProtocolParametersUpdate(..),
    DeprecatedAfterBabbagePParams(..),
    DeprecatedAfterMaryPParams(..),
    ShelleyToAlonzoPParams(..),
    IntroducedInBabbagePParams(..),
    IntroducedInConwayPParams(..),
    createEraBasedProtocolParamUpdate,
    convertToLedgerProtocolParameters,
    createPParams,

    -- * Deprecated
    ProtocolParametersUpdate(..),

    -- * Errors
    ProtocolParametersError(..),
    ProtocolParametersConversionError(..),

    -- * PraosNonce
    PraosNonce,
    makePraosNonce,

    -- * Execution units, prices and cost models,
    ExecutionUnits(..),
    ExecutionUnitPrices(..),
    CostModels(..),
    CostModel(..),
    fromAlonzoCostModels,

    -- * Update proposals to change the protocol parameters
    UpdateProposal(..),
    makeShelleyUpdateProposal,

    -- * Internal conversion functions
    toLedgerNonce,
    toLedgerUpdate,
    fromLedgerUpdate,
    toLedgerProposedPPUpdates,
    fromLedgerProposedPPUpdates,
    toLedgerPParams,
    toLedgerPParamsUpdate,
    fromLedgerPParams,
    fromLedgerPParamsUpdate,
    toAlonzoPrices,
    fromAlonzoPrices,
    toAlonzoScriptLanguage,
    fromAlonzoScriptLanguage,
    toAlonzoCostModel,
    fromAlonzoCostModel,
    toAlonzoCostModels,

    -- * Data family instances
    AsType(..),

    -- ** Era-dependent protocol features
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Json (toRationalJSON)
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Orphans ()
import           Cardano.Api.Pretty
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api.Era as Ledger
import           Cardano.Ledger.Api.PParams
import qualified Cardano.Ledger.Babbage.Core as Ledger
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.PParams as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Plutus.CostModels as Plutus
import qualified Cardano.Ledger.Plutus.Language as Plutus
import qualified Cardano.Ledger.Shelley.API as Ledger
import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?),
                   (.=))
import           Data.Bifunctor (bimap, first)
import           Data.ByteString (ByteString)
import           Data.Data (Data)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.String (IsString)
import           Data.Text (Text)
import           Data.Word
import           GHC.Generics
import           Lens.Micro
import           Numeric.Natural
import           Text.PrettyBy.Default (display)

-- -----------------------------------------------------------------------------
-- Era based ledger protocol parameters
--
newtype LedgerProtocolParameters era = LedgerProtocolParameters
  { unLedgerProtocolParameters :: Ledger.PParams (ShelleyLedgerEra era)
  }

instance IsShelleyBasedEra era => Show (LedgerProtocolParameters era) where
  show (LedgerProtocolParameters pp) =
    shelleyBasedEraConstraints (shelleyBasedEra @era)
      $ show pp

instance IsShelleyBasedEra era => Eq (LedgerProtocolParameters era) where
  LedgerProtocolParameters a == LedgerProtocolParameters b =
    shelleyBasedEraConstraints (shelleyBasedEra @era)
      $ a == b

-- TODO: Conway era - remove me when we begin relying on the JSON
-- instances of Ledger.PParams
convertToLedgerProtocolParameters
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersConversionError (LedgerProtocolParameters era)
convertToLedgerProtocolParameters sbe pp =
  LedgerProtocolParameters <$> toLedgerPParams sbe pp

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


data IntroducedInConwayPParams era
  = IntroducedInConwayPParams
    { icPoolVotingThresholds :: StrictMaybe Ledger.PoolVotingThresholds
    , icDRepVotingThresholds :: StrictMaybe Ledger.DRepVotingThresholds
    , icMinCommitteeSize     :: StrictMaybe Natural
    , icCommitteeTermLength  :: StrictMaybe Ledger.EpochInterval
    , icGovActionLifetime    :: StrictMaybe Ledger.EpochInterval
    , icGovActionDeposit     :: StrictMaybe Ledger.Coin
    , icDRepDeposit          :: StrictMaybe Ledger.Coin
    , icDRepActivity         :: StrictMaybe Ledger.EpochInterval
    } deriving Show


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


createEraBasedProtocolParamUpdate
  :: ShelleyBasedEra era
  -> EraBasedProtocolParametersUpdate era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createEraBasedProtocolParamUpdate sbe eraPParamsUpdate =
  case eraPParamsUpdate  of
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
    , cppMaxBlockBodySize ::  StrictMaybe Word32
    , cppMaxTxSize :: StrictMaybe Word32
    , cppMaxBlockHeaderSize :: StrictMaybe Word16
    , cppKeyDeposit :: StrictMaybe Ledger.Coin
    , cppPoolDeposit :: StrictMaybe Ledger.Coin
    , cppPoolRetireMaxEpoch :: StrictMaybe Ledger.EpochInterval
    , cppStakePoolTargetNum :: StrictMaybe Natural
    , cppPoolPledgeInfluence :: StrictMaybe Ledger.NonNegativeInterval
    , cppTreasuryExpansion :: StrictMaybe Ledger.UnitInterval
    , cppMonetaryExpansion :: StrictMaybe Ledger.UnitInterval
    , cppMinPoolCost :: StrictMaybe Ledger.Coin
    } deriving Show

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

newtype DeprecatedAfterMaryPParams ledgerera
  = DeprecatedAfterMaryPParams (StrictMaybe Ledger.Coin) -- Minimum UTxO value
  deriving Show

newtype DeprecatedAfterBabbagePParams ledgerera
  = DeprecatedAfterBabbagePParams (StrictMaybe Ledger.ProtVer)
  deriving Show

type MaxMaryEra ledgerera = Ledger.ProtVerAtMost ledgerera 4

createDeprecatedAfterMaryPParams
  :: EraPParams (ShelleyLedgerEra era)
  => MaxMaryEra (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> DeprecatedAfterMaryPParams era -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createDeprecatedAfterMaryPParams _ (DeprecatedAfterMaryPParams minUtxoVal) =
  Ledger.emptyPParamsUpdate & Ledger.ppuMinUTxOValueL .~ minUtxoVal

data ShelleyToAlonzoPParams ledgerera
  = ShelleyToAlonzoPParams
      (StrictMaybe Ledger.Nonce) -- ^ Extra entropy
      (StrictMaybe Ledger.UnitInterval) -- ^ Decentralization parameter
  deriving Show


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
    deriving Show

createPParamsUpdateIntroducedInAlonzo :: ()
  => AlonzoEraOnwards era
  -> AlonzoOnwardsPParams era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createPParamsUpdateIntroducedInAlonzo w (AlonzoOnwardsPParams {..})=
  alonzoEraOnwardsConstraints w $
    Ledger.emptyPParamsUpdate
      & Ledger.ppuCostModelsL .~ alCostModels
      & Ledger.ppuPricesL .~ alPrices
      & Ledger.ppuMaxTxExUnitsL .~ alMaxTxExUnits
      & Ledger.ppuMaxBlockExUnitsL .~ alMaxBlockExUnits
      & Ledger.ppuMaxValSizeL .~ alMaxValSize
      & Ledger.ppuCollateralPercentageL .~ alCollateralPercentage
      & Ledger.ppuMaxCollateralInputsL .~ alMaxCollateralInputs

newtype IntroducedInBabbagePParams era
  = IntroducedInBabbagePParams
      (StrictMaybe CoinPerByte) -- ^ Coins per UTxO byte
  deriving Show

createIntroducedInBabbagePParams :: ()
  => BabbageEraOnwards era
  -> IntroducedInBabbagePParams era
  -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
createIntroducedInBabbagePParams w (IntroducedInBabbagePParams coinsPerUTxOByte) =
  babbageEraOnwardsConstraints w $
    Ledger.emptyPParamsUpdate & Ledger.ppuCoinsPerUTxOByteL .~ coinsPerUTxOByte

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
--
data ProtocolParameters =
     ProtocolParameters {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --                              (Major  , Minor  )
       protocolParamProtocolVersion :: (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Praos schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       -- /Deprecated in Babbage/
       protocolParamDecentralization :: Maybe Rational,

       -- | Extra entropy for the Praos per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolParamExtraPraosEntropy :: Maybe PraosNonce,

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolParamMaxBlockHeaderSize :: Natural,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Praos network delta security parameter
       -- in mind. Making this too large can severely weaken the Praos
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolParamMaxBlockBodySize :: Natural,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolParamMaxTxSize :: Natural,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolParamTxFeeFixed :: L.Coin,

       -- | Per byte linear factor for the minimum fee calculation.
       --
       protocolParamTxFeePerByte :: L.Coin,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolParamMinUTxOValue :: Maybe L.Coin,

       -- | The deposit required to register a stake address.
       --
       protocolParamStakeAddressDeposit :: L.Coin,

       -- | The deposit required to register a stake pool.
       --
       protocolParamStakePoolDeposit :: L.Coin,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolParamMinPoolCost :: L.Coin,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolParamPoolRetireMaxEpoch :: Ledger.EpochInterval,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolParamStakePoolTargetNum :: Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolParamPoolPledgeInfluence :: Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolParamMonetaryExpansion :: Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolParamTreasuryCut :: Rational,

       -- | Cost models for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolParamCostModels :: Map AnyPlutusScriptVersion CostModel,

       -- | Price of execution units for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolParamPrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       --
       -- /Introduced in Alonzo/
       protocolParamMaxTxExUnits :: Maybe ExecutionUnits,

       -- | Max total script execution resources units allowed per block
       --
       -- /Introduced in Alonzo/
       protocolParamMaxBlockExUnits :: Maybe ExecutionUnits,

       -- | Max size of a Value in a tx output.
       --
       -- /Introduced in Alonzo/
       protocolParamMaxValueSize :: Maybe Natural,

       -- | The percentage of the script contribution to the txfee that must be
       -- provided as collateral inputs when including Plutus scripts.
       --
       -- /Introduced in Alonzo/
       protocolParamCollateralPercent :: Maybe Natural,

       -- | The maximum number of collateral inputs allowed in a transaction.
       --
       -- /Introduced in Alonzo/
       protocolParamMaxCollateralInputs :: Maybe Natural,

       -- | Cost in ada per byte of UTxO storage.
       --
       -- /Introduced in Babbage/
       protocolParamUTxOCostPerByte :: Maybe L.Coin

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
      [ "extraPraosEntropy"   .= protocolParamExtraPraosEntropy
      , "stakePoolTargetNum"  .= protocolParamStakePoolTargetNum
      , "minUTxOValue"        .= protocolParamMinUTxOValue
      , "poolRetireMaxEpoch"  .= protocolParamPoolRetireMaxEpoch
      , "decentralization"    .= (toRationalJSON <$> protocolParamDecentralization)
      , "stakePoolDeposit"    .= protocolParamStakePoolDeposit
      , "maxBlockHeaderSize"  .= protocolParamMaxBlockHeaderSize
      , "maxBlockBodySize"    .= protocolParamMaxBlockBodySize
      , "maxTxSize"           .= protocolParamMaxTxSize
      , "treasuryCut"         .= toRationalJSON protocolParamTreasuryCut
      , "minPoolCost"         .= protocolParamMinPoolCost
      , "monetaryExpansion"   .= toRationalJSON protocolParamMonetaryExpansion
      , "stakeAddressDeposit" .= protocolParamStakeAddressDeposit
      , "poolPledgeInfluence" .= toRationalJSON protocolParamPoolPledgeInfluence
      , "protocolVersion"     .= let (major, minor) = protocolParamProtocolVersion
                                  in object ["major" .= major, "minor" .= minor]
      , "txFeeFixed"          .= protocolParamTxFeeFixed
      , "txFeePerByte"        .= protocolParamTxFeePerByte
      -- Alonzo era:
      , "costModels"             .= CostModels protocolParamCostModels
      , "executionUnitPrices"    .= protocolParamPrices
      , "maxTxExecutionUnits"    .= protocolParamMaxTxExUnits
      , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits
      , "maxValueSize"           .= protocolParamMaxValueSize
      , "collateralPercentage"   .= protocolParamCollateralPercent
      , "maxCollateralInputs"    .= protocolParamMaxCollateralInputs
      -- Babbage era:
      , "utxoCostPerByte"        .= protocolParamUTxOCostPerByte
      ]


-- ----------------------------------------------------------------------------
-- Updates to the protocol parameters
--

-- | The representation of a change in the 'ProtocolParameters'.
--
data ProtocolParametersUpdate =
     ProtocolParametersUpdate {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --
       protocolUpdateProtocolVersion :: Maybe (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Praos schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       protocolUpdateDecentralization :: Maybe Rational,

       -- | Extra entropy for the Praos per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolUpdateExtraPraosEntropy :: Maybe (Maybe PraosNonce),

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolUpdateMaxBlockHeaderSize :: Maybe Word16,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Praos network delta security parameter
       -- in mind. Making this too large can severely weaken the Praos
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolUpdateMaxBlockBodySize :: Maybe Word32,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolUpdateMaxTxSize :: Maybe Word32,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolUpdateTxFeeFixed :: Maybe L.Coin,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolUpdateTxFeePerByte :: Maybe L.Coin,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolUpdateMinUTxOValue :: Maybe L.Coin,

       -- | The deposit required to register a stake address.
       --
       protocolUpdateStakeAddressDeposit :: Maybe L.Coin,

       -- | The deposit required to register a stake pool.
       --
       protocolUpdateStakePoolDeposit :: Maybe L.Coin,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolUpdateMinPoolCost :: Maybe L.Coin,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolUpdatePoolRetireMaxEpoch :: Maybe Ledger.EpochInterval,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolUpdateStakePoolTargetNum :: Maybe Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolUpdatePoolPledgeInfluence :: Maybe Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolUpdateMonetaryExpansion :: Maybe Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolUpdateTreasuryCut :: Maybe Rational,

       -- Introduced in Alonzo,

       -- | Cost models for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolUpdateCostModels :: Map AnyPlutusScriptVersion CostModel,

       -- | Price of execution units for script languages that use them.
       --
       -- /Introduced in Alonzo/
       protocolUpdatePrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxTxExUnits :: Maybe ExecutionUnits,

       -- | Max total script execution resources units allowed per block
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxBlockExUnits :: Maybe ExecutionUnits,

       -- | Max size of a 'Value' in a tx output.
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxValueSize :: Maybe Natural,

       -- | The percentage of the script contribution to the txfee that must be
       -- provided as collateral inputs when including Plutus scripts.
       --
       -- /Introduced in Alonzo/
       protocolUpdateCollateralPercent :: Maybe Natural,

       -- | The maximum number of collateral inputs allowed in a transaction.
       --
       -- /Introduced in Alonzo/
       protocolUpdateMaxCollateralInputs :: Maybe Natural,

       -- | Cost in ada per byte of UTxO storage.
       --
       -- /Introduced in Babbage/
       protocolUpdateUTxOCostPerByte :: Maybe L.Coin
    }
  deriving (Eq, Show)

instance Semigroup ProtocolParametersUpdate where
    ppu1 <> ppu2 =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = merge protocolUpdateProtocolVersion
      , protocolUpdateDecentralization    = merge protocolUpdateDecentralization
      , protocolUpdateExtraPraosEntropy   = merge protocolUpdateExtraPraosEntropy
      , protocolUpdateMaxBlockHeaderSize  = merge protocolUpdateMaxBlockHeaderSize
      , protocolUpdateMaxBlockBodySize    = merge protocolUpdateMaxBlockBodySize
      , protocolUpdateMaxTxSize           = merge protocolUpdateMaxTxSize
      , protocolUpdateTxFeeFixed          = merge protocolUpdateTxFeeFixed
      , protocolUpdateTxFeePerByte        = merge protocolUpdateTxFeePerByte
      , protocolUpdateMinUTxOValue        = merge protocolUpdateMinUTxOValue
      , protocolUpdateStakeAddressDeposit = merge protocolUpdateStakeAddressDeposit
      , protocolUpdateStakePoolDeposit    = merge protocolUpdateStakePoolDeposit
      , protocolUpdateMinPoolCost         = merge protocolUpdateMinPoolCost
      , protocolUpdatePoolRetireMaxEpoch  = merge protocolUpdatePoolRetireMaxEpoch
      , protocolUpdateStakePoolTargetNum  = merge protocolUpdateStakePoolTargetNum
      , protocolUpdatePoolPledgeInfluence = merge protocolUpdatePoolPledgeInfluence
      , protocolUpdateMonetaryExpansion   = merge protocolUpdateMonetaryExpansion
      , protocolUpdateTreasuryCut         = merge protocolUpdateTreasuryCut
      -- Introduced in Alonzo below.
      , protocolUpdateCostModels          = mergeMap protocolUpdateCostModels
      , protocolUpdatePrices              = merge protocolUpdatePrices
      , protocolUpdateMaxTxExUnits        = merge protocolUpdateMaxTxExUnits
      , protocolUpdateMaxBlockExUnits     = merge protocolUpdateMaxBlockExUnits
      , protocolUpdateMaxValueSize        = merge protocolUpdateMaxValueSize
      , protocolUpdateCollateralPercent   = merge protocolUpdateCollateralPercent
      , protocolUpdateMaxCollateralInputs = merge protocolUpdateMaxCollateralInputs
      -- Introduced in Babbage below.
      , protocolUpdateUTxOCostPerByte     = merge protocolUpdateUTxOCostPerByte
      }
      where
        -- prefer the right hand side:
        merge :: (ProtocolParametersUpdate -> Maybe a) -> Maybe a
        merge f = f ppu2 `mplus` f ppu1

        -- prefer the right hand side:
        mergeMap :: Ord k => (ProtocolParametersUpdate -> Map k a) -> Map k a
        mergeMap f = f ppu2 `Map.union` f ppu1

instance Monoid ProtocolParametersUpdate where
    mempty =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = Nothing
      , protocolUpdateDecentralization    = Nothing
      , protocolUpdateExtraPraosEntropy   = Nothing
      , protocolUpdateMaxBlockHeaderSize  = Nothing
      , protocolUpdateMaxBlockBodySize    = Nothing
      , protocolUpdateMaxTxSize           = Nothing
      , protocolUpdateTxFeeFixed          = Nothing
      , protocolUpdateTxFeePerByte        = Nothing
      , protocolUpdateMinUTxOValue        = Nothing
      , protocolUpdateStakeAddressDeposit = Nothing
      , protocolUpdateStakePoolDeposit    = Nothing
      , protocolUpdateMinPoolCost         = Nothing
      , protocolUpdatePoolRetireMaxEpoch  = Nothing
      , protocolUpdateStakePoolTargetNum  = Nothing
      , protocolUpdatePoolPledgeInfluence = Nothing
      , protocolUpdateMonetaryExpansion   = Nothing
      , protocolUpdateTreasuryCut         = Nothing
      , protocolUpdateCostModels          = mempty
      , protocolUpdatePrices              = Nothing
      , protocolUpdateMaxTxExUnits        = Nothing
      , protocolUpdateMaxBlockExUnits     = Nothing
      , protocolUpdateMaxValueSize        = Nothing
      , protocolUpdateCollateralPercent   = Nothing
      , protocolUpdateMaxCollateralInputs = Nothing
      , protocolUpdateUTxOCostPerByte     = Nothing
      }

instance ToCBOR ProtocolParametersUpdate where
    toCBOR :: ProtocolParametersUpdate -> CBOR.Encoding
    toCBOR ProtocolParametersUpdate{..} =
        CBOR.encodeListLen 26
     <> toCBOR protocolUpdateProtocolVersion
     <> toCBOR protocolUpdateDecentralization
     <> toCBOR protocolUpdateExtraPraosEntropy
     <> toCBOR protocolUpdateMaxBlockHeaderSize
     <> toCBOR protocolUpdateMaxBlockBodySize
     <> toCBOR protocolUpdateMaxTxSize
     <> toCBOR protocolUpdateTxFeeFixed
     <> toCBOR protocolUpdateTxFeePerByte
     <> toCBOR protocolUpdateMinUTxOValue
     <> toCBOR protocolUpdateStakeAddressDeposit
     <> toCBOR protocolUpdateStakePoolDeposit
     <> toCBOR protocolUpdateMinPoolCost
     <> toCBOR protocolUpdatePoolRetireMaxEpoch
     <> toCBOR protocolUpdateStakePoolTargetNum
     <> toCBOR protocolUpdatePoolPledgeInfluence
     <> toCBOR protocolUpdateMonetaryExpansion
     <> toCBOR protocolUpdateTreasuryCut
     <> toCBOR protocolUpdateCostModels
     <> toCBOR protocolUpdatePrices
     <> toCBOR protocolUpdateMaxTxExUnits
     <> toCBOR protocolUpdateMaxBlockExUnits
     <> toCBOR protocolUpdateMaxValueSize
     <> toCBOR protocolUpdateCollateralPercent
     <> toCBOR protocolUpdateMaxCollateralInputs
     <> toCBOR protocolUpdateUTxOCostPerByte
instance FromCBOR ProtocolParametersUpdate where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 26
      ProtocolParametersUpdate
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR

-- ----------------------------------------------------------------------------
-- Praos nonce
--

newtype PraosNonce = PraosNonce { unPraosNonce :: Ledger.Hash StandardCrypto ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving (Show, IsString)   via UsingRawBytesHex PraosNonce
  deriving (ToJSON, FromJSON) via UsingRawBytesHex PraosNonce
  deriving (ToCBOR, FromCBOR) via UsingRawBytes    PraosNonce

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
toLedgerNonce Nothing               = Ledger.NeutralNonce
toLedgerNonce (Just (PraosNonce h)) = Ledger.Nonce (Crypto.castHash h)

fromLedgerNonce :: Ledger.Nonce -> Maybe PraosNonce
fromLedgerNonce Ledger.NeutralNonce = Nothing
fromLedgerNonce (Ledger.Nonce h)    = Just (PraosNonce (Crypto.castHash h))


-- ----------------------------------------------------------------------------
-- Script execution unit prices and cost models
--

-- | The prices for 'ExecutionUnits' as a fraction of a 'L.Coin'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
--
data ExecutionUnitPrices =
     ExecutionUnitPrices {
       priceExecutionSteps  :: Rational,
       priceExecutionMemory :: Rational
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
    object [ "priceSteps"  .= toRationalJSON priceExecutionSteps
           , "priceMemory" .= toRationalJSON priceExecutionMemory
           ]

instance FromJSON ExecutionUnitPrices where
  parseJSON =
    withObject "ExecutionUnitPrices" $ \o ->
      ExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"


toAlonzoPrices :: ExecutionUnitPrices -> Either ProtocolParametersConversionError Alonzo.Prices
toAlonzoPrices ExecutionUnitPrices {
                 priceExecutionSteps,
                 priceExecutionMemory
               } = do
  prSteps <- boundRationalEither "Steps" priceExecutionSteps
  prMem   <- boundRationalEither "Mem" priceExecutionMemory
  return Alonzo.Prices {
    Alonzo.prSteps,
    Alonzo.prMem
  }

fromAlonzoPrices :: Alonzo.Prices -> ExecutionUnitPrices
fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} =
  ExecutionUnitPrices {
    priceExecutionSteps  = Ledger.unboundRational prSteps,
    priceExecutionMemory = Ledger.unboundRational prMem
  }


-- ----------------------------------------------------------------------------
-- Script cost models
--

newtype CostModel = CostModel [Integer]
  deriving (Eq, Show, Data)
  deriving newtype (ToCBOR, FromCBOR)

newtype CostModels = CostModels { unCostModels :: Map AnyPlutusScriptVersion CostModel }
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
  f <- mapM conv $ Map.toList m
  Right $ Plutus.mkCostModels $ Map.fromList f
 where
  conv :: (AnyPlutusScriptVersion, CostModel) -> Either ProtocolParametersConversionError (Plutus.Language, Alonzo.CostModel)
  conv (anySVer, cModel) = do
    alonzoCostModel <- toAlonzoCostModel cModel (toAlonzoScriptLanguage anySVer)
    Right (toAlonzoScriptLanguage anySVer, alonzoCostModel)

fromAlonzoCostModels
  :: Plutus.CostModels
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels cModels =
    Map.fromList
  . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
  $ Map.toList $ Plutus.costModelsValid cModels

toAlonzoScriptLanguage :: AnyPlutusScriptVersion -> Plutus.Language
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Plutus.PlutusV1
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Plutus.PlutusV2
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV3) = Plutus.PlutusV3

fromAlonzoScriptLanguage :: Plutus.Language -> AnyPlutusScriptVersion
fromAlonzoScriptLanguage Plutus.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoScriptLanguage Plutus.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2
fromAlonzoScriptLanguage Plutus.PlutusV3 = AnyPlutusScriptVersion PlutusScriptV3

toAlonzoCostModel :: CostModel -> Plutus.Language -> Either ProtocolParametersConversionError Alonzo.CostModel
toAlonzoCostModel (CostModel m) l = first (PpceInvalidCostModel (CostModel m)) $ Alonzo.mkCostModel l m

fromAlonzoCostModel :: Alonzo.CostModel -> CostModel
fromAlonzoCostModel m = CostModel $ Alonzo.getCostModelParams m


-- ----------------------------------------------------------------------------
-- Proposals embedded in transactions to update protocol parameters
--

data UpdateProposal =
     UpdateProposal
       !(Map (Hash GenesisKey) ProtocolParametersUpdate)
       !EpochNo
    deriving stock (Eq, Show)
    deriving anyclass SerialiseAsCBOR

instance HasTypeProxy UpdateProposal where
    data AsType UpdateProposal = AsUpdateProposal
    proxyToAsType _ = AsUpdateProposal

instance HasTextEnvelope UpdateProposal where
    textEnvelopeType _ = "UpdateProposalShelley"

instance ToCBOR UpdateProposal where
    toCBOR (UpdateProposal ppup epochno) =
        CBOR.encodeListLen 2
     <> toCBOR ppup
     <> toCBOR epochno

instance FromCBOR UpdateProposal where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 2
      UpdateProposal
        <$> fromCBOR
        <*> fromCBOR

makeShelleyUpdateProposal :: ProtocolParametersUpdate
                          -> [Hash GenesisKey]
                          -> EpochNo
                          -> UpdateProposal
makeShelleyUpdateProposal params genesisKeyHashes =
    --TODO decide how to handle parameter validation
    --     for example we need to validate the Rational values can convert
    --     into the UnitInterval type ok.
    UpdateProposal (Map.fromList [ (kh, params) | kh <- genesisKeyHashes ])


-- ----------------------------------------------------------------------------
-- Conversion functions: updates to ledger types
--

toLedgerUpdate :: ()
  => ShelleyBasedEra era
  -> UpdateProposal
  -> Either ProtocolParametersConversionError (Ledger.Update (ShelleyLedgerEra era))
toLedgerUpdate sbe (UpdateProposal ppup epochno) =
  (`Ledger.Update` epochno) <$> toLedgerProposedPPUpdates sbe ppup

toLedgerProposedPPUpdates :: ()
  => ShelleyBasedEra era
  -> Map (Hash GenesisKey) ProtocolParametersUpdate
  -> Either ProtocolParametersConversionError (Ledger.ProposedPPUpdates (ShelleyLedgerEra era))
toLedgerProposedPPUpdates sbe m =
  shelleyBasedEraConstraints sbe $
    Ledger.ProposedPPUpdates . Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh) <$> traverse (toLedgerPParamsUpdate sbe) m

toLedgerPParamsUpdate :: ShelleyBasedEra era
                      -> ProtocolParametersUpdate
                      -> Either ProtocolParametersConversionError (PParamsUpdate (ShelleyLedgerEra era))
toLedgerPParamsUpdate ShelleyBasedEraShelley = toShelleyPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraAllegra = toShelleyPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraMary    = toShelleyPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraAlonzo  = toAlonzoPParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraBabbage = toBabbagePParamsUpdate
toLedgerPParamsUpdate ShelleyBasedEraConway  = toConwayPParamsUpdate


toShelleyCommonPParamsUpdate :: EraPParams ledgerera
                             => ProtocolParametersUpdate
                             -> Either ProtocolParametersConversionError (PParamsUpdate ledgerera)
toShelleyCommonPParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    } = do
  a0 <- mapM (boundRationalEither "A0") protocolUpdatePoolPledgeInfluence
  rho <- mapM (boundRationalEither "Rho") protocolUpdateMonetaryExpansion
  tau <- mapM (boundRationalEither "Tau") protocolUpdateTreasuryCut
  let ppuCommon =
        emptyPParamsUpdate
        & ppuMinFeeAL     .~ noInlineMaybeToStrictMaybe protocolUpdateTxFeePerByte
        & ppuMinFeeBL     .~ noInlineMaybeToStrictMaybe protocolUpdateTxFeeFixed
        & ppuMaxBBSizeL   .~ noInlineMaybeToStrictMaybe protocolUpdateMaxBlockBodySize
        & ppuMaxTxSizeL   .~ noInlineMaybeToStrictMaybe protocolUpdateMaxTxSize
        & ppuMaxBHSizeL   .~ noInlineMaybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
        & ppuKeyDepositL  .~ noInlineMaybeToStrictMaybe protocolUpdateStakeAddressDeposit
        & ppuPoolDepositL .~ noInlineMaybeToStrictMaybe protocolUpdateStakePoolDeposit
        & ppuEMaxL        .~ noInlineMaybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
        & ppuNOptL        .~ noInlineMaybeToStrictMaybe protocolUpdateStakePoolTargetNum
        & ppuA0L          .~ noInlineMaybeToStrictMaybe a0
        & ppuRhoL         .~ noInlineMaybeToStrictMaybe rho
        & ppuTauL         .~ noInlineMaybeToStrictMaybe tau
        & ppuMinPoolCostL .~ noInlineMaybeToStrictMaybe protocolUpdateMinPoolCost
  pure ppuCommon

toShelleyPParamsUpdate :: ( EraPParams ledgerera
                          , Ledger.AtMostEra Ledger.MaryEra ledgerera
                          , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                          , Ledger.AtMostEra Ledger.BabbageEra ledgerera
                          )
                       => ProtocolParametersUpdate
                       -> Either ProtocolParametersConversionError (PParamsUpdate ledgerera)
toShelleyPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMinUTxOValue
    } = do
  ppuCommon <- toShelleyCommonPParamsUpdate protocolParametersUpdate
  d <- mapM (boundRationalEither "D") protocolUpdateDecentralization
  protVer <- mapM mkProtVer protocolUpdateProtocolVersion
  let ppuShelley =
        ppuCommon
        & ppuDL               .~ noInlineMaybeToStrictMaybe d
        & ppuExtraEntropyL    .~ (toLedgerNonce <$> noInlineMaybeToStrictMaybe protocolUpdateExtraPraosEntropy)
        & ppuMinUTxOValueL    .~ noInlineMaybeToStrictMaybe protocolUpdateMinUTxOValue
        & ppuProtocolVersionL .~ noInlineMaybeToStrictMaybe protVer
  pure ppuShelley


toAlonzoCommonPParamsUpdate :: AlonzoEraPParams ledgerera
                            => ProtocolParametersUpdate
                            -> Either ProtocolParametersConversionError (PParamsUpdate ledgerera)
toAlonzoCommonPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateCostModels
    , protocolUpdatePrices
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxCollateralInputs
    } = do
  ppuShelleyCommon <- toShelleyCommonPParamsUpdate protocolParametersUpdate
  costModels <-
    if Map.null protocolUpdateCostModels
    then pure SNothing
    else SJust <$> toAlonzoCostModels protocolUpdateCostModels
  prices <- mapM toAlonzoPrices protocolUpdatePrices
  let ppuAlonzoCommon =
        ppuShelleyCommon
        & ppuCostModelsL           .~ costModels
        & ppuPricesL               .~ noInlineMaybeToStrictMaybe prices
        & ppuMaxTxExUnitsL         .~
          (toAlonzoExUnits <$> noInlineMaybeToStrictMaybe protocolUpdateMaxTxExUnits)
        & ppuMaxBlockExUnitsL      .~
          (toAlonzoExUnits <$> noInlineMaybeToStrictMaybe protocolUpdateMaxBlockExUnits)
        & ppuMaxValSizeL           .~ noInlineMaybeToStrictMaybe protocolUpdateMaxValueSize
        & ppuCollateralPercentageL .~ noInlineMaybeToStrictMaybe protocolUpdateCollateralPercent
        & ppuMaxCollateralInputsL  .~ noInlineMaybeToStrictMaybe protocolUpdateMaxCollateralInputs
  pure ppuAlonzoCommon


toAlonzoPParamsUpdate :: Ledger.Crypto crypto
                      => ProtocolParametersUpdate
                      -> Either ProtocolParametersConversionError (PParamsUpdate (Ledger.AlonzoEra crypto))
toAlonzoPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    } = do
  ppuAlonzoCommon <- toAlonzoCommonPParamsUpdate protocolParametersUpdate
  d <- mapM (boundRationalEither "D") protocolUpdateDecentralization
  protVer <- mapM mkProtVer protocolUpdateProtocolVersion
  let ppuAlonzo =
        ppuAlonzoCommon
        & ppuDL .~ noInlineMaybeToStrictMaybe d
        & ppuProtocolVersionL .~ noInlineMaybeToStrictMaybe protVer
  pure ppuAlonzo

toBabbageCommonPParamsUpdate :: BabbageEraPParams ledgerera
                            => ProtocolParametersUpdate
                            -> Either ProtocolParametersConversionError (Ledger.PParamsUpdate ledgerera)
toBabbageCommonPParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateUTxOCostPerByte
    } = do
  ppuAlonzoCommon <- toAlonzoCommonPParamsUpdate protocolParametersUpdate
  let ppuBabbage =
        ppuAlonzoCommon
          & ppuCoinsPerUTxOByteL .~ fmap CoinPerByte (noInlineMaybeToStrictMaybe protocolUpdateUTxOCostPerByte)
  pure ppuBabbage

toBabbagePParamsUpdate :: Ledger.Crypto crypto
                       => ProtocolParametersUpdate
                       -> Either ProtocolParametersConversionError (Ledger.PParamsUpdate (Ledger.BabbageEra crypto))
toBabbagePParamsUpdate
    protocolParametersUpdate@ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    } = do
  ppuBabbageCommon <- toBabbageCommonPParamsUpdate protocolParametersUpdate
  protVer <- mapM mkProtVer protocolUpdateProtocolVersion
  let ppuBabbage =
        ppuBabbageCommon
        & ppuProtocolVersionL .~ noInlineMaybeToStrictMaybe protVer
  pure ppuBabbage

requireParam :: String -> (a -> Either ProtocolParametersConversionError b) -> Maybe a -> Either ProtocolParametersConversionError b
requireParam paramName = maybe (Left $ PpceMissingParameter paramName)

mkProtVer :: (Natural, Natural) -> Either ProtocolParametersConversionError Ledger.ProtVer
mkProtVer (majorProtVer, minorProtVer) = maybeToRight (PpceVersionInvalid majorProtVer) $
  (`Ledger.ProtVer` minorProtVer) <$> Ledger.mkVersion majorProtVer

boundRationalEither :: Ledger.BoundedRational b
                    => String
                    -> Rational
                    -> Either ProtocolParametersConversionError b
boundRationalEither name r = maybeToRight (PpceOutOfBounds name r) $ Ledger.boundRational r

-- Conway uses the same PParams as Babbage for now.
toConwayPParamsUpdate :: BabbageEraPParams ledgerera
                      => ProtocolParametersUpdate
                      -> Either ProtocolParametersConversionError (PParamsUpdate ledgerera)
toConwayPParamsUpdate = toBabbageCommonPParamsUpdate

-- ----------------------------------------------------------------------------
-- Conversion functions: updates from ledger types
--

fromLedgerUpdate :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => Ledger.EraCrypto ledgerera ~ StandardCrypto
                 => ShelleyBasedEra era
                 -> Ledger.Update ledgerera
                 -> UpdateProposal
fromLedgerUpdate sbe (Ledger.Update ppup epochno) =
    UpdateProposal (fromLedgerProposedPPUpdates sbe ppup) epochno


fromLedgerProposedPPUpdates :: forall era ledgerera.
                               ShelleyLedgerEra era ~ ledgerera
                            => Ledger.EraCrypto ledgerera ~ StandardCrypto
                            => ShelleyBasedEra era
                            -> Ledger.ProposedPPUpdates ledgerera
                            -> Map (Hash GenesisKey) ProtocolParametersUpdate
fromLedgerProposedPPUpdates sbe =
    Map.map (fromLedgerPParamsUpdate sbe)
  . Map.mapKeysMonotonic GenesisKeyHash
  . (\(Ledger.ProposedPPUpdates ppup) -> ppup)


fromLedgerPParamsUpdate :: ShelleyBasedEra era
                        -> Ledger.PParamsUpdate (ShelleyLedgerEra era)
                        -> ProtocolParametersUpdate
fromLedgerPParamsUpdate ShelleyBasedEraShelley = fromShelleyPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraAllegra = fromShelleyPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraMary    = fromShelleyPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraAlonzo  = fromAlonzoPParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraBabbage = fromBabbagePParamsUpdate
fromLedgerPParamsUpdate ShelleyBasedEraConway  = fromConwayPParamsUpdate



fromShelleyCommonPParamsUpdate :: EraPParams ledgerera
                               => PParamsUpdate ledgerera
                               -> ProtocolParametersUpdate
fromShelleyCommonPParamsUpdate ppu =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = Nothing
    , protocolUpdateMaxBlockHeaderSize  = strictMaybeToMaybe (ppu ^. ppuMaxBHSizeL)
    , protocolUpdateMaxBlockBodySize    = strictMaybeToMaybe (ppu ^. ppuMaxBBSizeL)
    , protocolUpdateMaxTxSize           = strictMaybeToMaybe (ppu ^. ppuMaxTxSizeL)
    , protocolUpdateTxFeeFixed          = strictMaybeToMaybe (ppu ^. ppuMinFeeBL)
    , protocolUpdateTxFeePerByte        = strictMaybeToMaybe (ppu ^. ppuMinFeeAL)
    , protocolUpdateStakeAddressDeposit = strictMaybeToMaybe (ppu ^. ppuKeyDepositL)
    , protocolUpdateStakePoolDeposit    = strictMaybeToMaybe (ppu ^. ppuPoolDepositL)
    , protocolUpdateMinPoolCost         = strictMaybeToMaybe (ppu ^. ppuMinPoolCostL)
    , protocolUpdatePoolRetireMaxEpoch  = strictMaybeToMaybe (ppu ^. ppuEMaxL)
    , protocolUpdateStakePoolTargetNum  = strictMaybeToMaybe (ppu ^. ppuNOptL)
    , protocolUpdatePoolPledgeInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (ppu ^. ppuA0L)
    , protocolUpdateMonetaryExpansion   = Ledger.unboundRational <$> strictMaybeToMaybe (ppu ^. ppuRhoL)
    , protocolUpdateTreasuryCut         = Ledger.unboundRational <$> strictMaybeToMaybe (ppu ^. ppuTauL)
    , protocolUpdateCostModels          = mempty
    , protocolUpdatePrices              = Nothing
    , protocolUpdateMaxTxExUnits        = Nothing
    , protocolUpdateMaxBlockExUnits     = Nothing
    , protocolUpdateMaxValueSize        = Nothing
    , protocolUpdateCollateralPercent   = Nothing
    , protocolUpdateMaxCollateralInputs = Nothing
    , protocolUpdateUTxOCostPerByte     = Nothing
    , protocolUpdateDecentralization    = Nothing
    , protocolUpdateExtraPraosEntropy   = Nothing
    , protocolUpdateMinUTxOValue        = Nothing
    }

fromShelleyPParamsUpdate :: ( EraPParams ledgerera
                            , Ledger.AtMostEra Ledger.MaryEra ledgerera
                            , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                            , Ledger.AtMostEra Ledger.BabbageEra ledgerera
                            )
                         => PParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromShelleyPParamsUpdate ppu =
  (fromShelleyCommonPParamsUpdate ppu) {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (Ledger.getVersion a,b)) <$>
                                           strictMaybeToMaybe (ppu ^. ppuProtocolVersionL)
    , protocolUpdateDecentralization    = Ledger.unboundRational <$>
                                            strictMaybeToMaybe (ppu ^. ppuDL)
    , protocolUpdateExtraPraosEntropy   = fromLedgerNonce <$>
                                            strictMaybeToMaybe (ppu ^. ppuExtraEntropyL)
    , protocolUpdateMinUTxOValue        = strictMaybeToMaybe (ppu ^. ppuMinUTxOValueL)
    }

fromAlonzoCommonPParamsUpdate :: AlonzoEraPParams ledgerera
                              => PParamsUpdate ledgerera
                              -> ProtocolParametersUpdate
fromAlonzoCommonPParamsUpdate ppu =
  (fromShelleyCommonPParamsUpdate ppu) {
      protocolUpdateCostModels          = maybe mempty fromAlonzoCostModels
                                               (strictMaybeToMaybe (ppu ^. ppuCostModelsL))
    , protocolUpdatePrices              = fromAlonzoPrices <$>
                                            strictMaybeToMaybe (ppu ^. ppuPricesL)
    , protocolUpdateMaxTxExUnits        = fromAlonzoExUnits <$>
                                            strictMaybeToMaybe (ppu ^. ppuMaxTxExUnitsL)
    , protocolUpdateMaxBlockExUnits     = fromAlonzoExUnits <$>
                                            strictMaybeToMaybe (ppu ^. ppuMaxBlockExUnitsL)
    , protocolUpdateMaxValueSize        = strictMaybeToMaybe (ppu ^. ppuMaxValSizeL)
    , protocolUpdateCollateralPercent   = strictMaybeToMaybe (ppu ^. ppuCollateralPercentageL)
    , protocolUpdateMaxCollateralInputs = strictMaybeToMaybe (ppu ^. ppuMaxCollateralInputsL)
    , protocolUpdateUTxOCostPerByte     = Nothing
    }


fromAlonzoPParamsUpdate :: Ledger.Crypto crypto
                        => PParamsUpdate (Ledger.AlonzoEra crypto)
                        -> ProtocolParametersUpdate
fromAlonzoPParamsUpdate ppu =
  (fromAlonzoCommonPParamsUpdate ppu) {
      protocolUpdateProtocolVersion    = (\(Ledger.ProtVer a b) -> (Ledger.getVersion a,b)) <$>
                                           strictMaybeToMaybe (ppu ^. ppuProtocolVersionL)
    }

fromBabbageCommonPParamsUpdate :: BabbageEraPParams ledgerera
                               => PParamsUpdate ledgerera
                               -> ProtocolParametersUpdate
fromBabbageCommonPParamsUpdate ppu =
  (fromAlonzoCommonPParamsUpdate ppu)
    { protocolUpdateUTxOCostPerByte = unCoinPerByte <$> strictMaybeToMaybe (ppu ^. ppuCoinsPerUTxOByteL)
    }

fromBabbagePParamsUpdate :: Ledger.Crypto crypto
                         => PParamsUpdate (Ledger.BabbageEra crypto)
                         -> ProtocolParametersUpdate
fromBabbagePParamsUpdate ppu =
  (fromBabbageCommonPParamsUpdate ppu) {
      protocolUpdateProtocolVersion    = (\(Ledger.ProtVer a b) -> (Ledger.getVersion a,b)) <$>
                                           strictMaybeToMaybe (ppu ^. ppuProtocolVersionL)
    }

fromConwayPParamsUpdate :: BabbageEraPParams ledgerera
                        => PParamsUpdate ledgerera
                        -> ProtocolParametersUpdate
fromConwayPParamsUpdate = fromBabbageCommonPParamsUpdate


-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters to ledger types
--

toLedgerPParams
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersConversionError (Ledger.PParams (ShelleyLedgerEra era))
toLedgerPParams ShelleyBasedEraShelley = toShelleyPParams
toLedgerPParams ShelleyBasedEraAllegra = toShelleyPParams
toLedgerPParams ShelleyBasedEraMary    = toShelleyPParams
toLedgerPParams ShelleyBasedEraAlonzo  = toAlonzoPParams
toLedgerPParams ShelleyBasedEraBabbage = toBabbagePParams
toLedgerPParams ShelleyBasedEraConway  = toConwayPParams


toShelleyCommonPParams :: EraPParams ledgerera
                       => ProtocolParameters
                       -> Either ProtocolParametersConversionError (PParams ledgerera)
toShelleyCommonPParams
    ProtocolParameters {
      protocolParamProtocolVersion
    , protocolParamMaxBlockHeaderSize
    , protocolParamMaxBlockBodySize
    , protocolParamMaxTxSize
    , protocolParamTxFeeFixed
    , protocolParamTxFeePerByte
    , protocolParamStakeAddressDeposit
    , protocolParamStakePoolDeposit
    , protocolParamMinPoolCost
    , protocolParamPoolRetireMaxEpoch
    , protocolParamStakePoolTargetNum
    , protocolParamPoolPledgeInfluence
    , protocolParamMonetaryExpansion
    , protocolParamTreasuryCut
    } = do
  a0 <- boundRationalEither "A0" protocolParamPoolPledgeInfluence
  rho <- boundRationalEither "Rho" protocolParamMonetaryExpansion
  tau <- boundRationalEither "Tau" protocolParamTreasuryCut
  protVer <- mkProtVer protocolParamProtocolVersion
  let ppCommon =
        emptyPParams
        & ppMinFeeAL         .~ protocolParamTxFeePerByte
        & ppMinFeeBL         .~ protocolParamTxFeeFixed
        & ppMaxBBSizeL       .~ fromIntegral protocolParamMaxBlockBodySize
        & ppMaxTxSizeL       .~ fromIntegral protocolParamMaxTxSize
        & ppMaxBHSizeL       .~ fromIntegral protocolParamMaxBlockHeaderSize
        & ppKeyDepositL      .~ protocolParamStakeAddressDeposit
        & ppPoolDepositL     .~ protocolParamStakePoolDeposit
        & ppEMaxL            .~ protocolParamPoolRetireMaxEpoch
        & ppNOptL            .~ protocolParamStakePoolTargetNum
        & ppA0L              .~ a0
        & ppRhoL             .~ rho
        & ppTauL             .~ tau
        & ppProtocolVersionL .~ protVer
        & ppMinPoolCostL     .~ protocolParamMinPoolCost
  pure ppCommon

toShelleyPParams :: ( EraPParams ledgerera
                    , Ledger.AtMostEra Ledger.MaryEra ledgerera
                    , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                    )
                 => ProtocolParameters
                 -> Either ProtocolParametersConversionError (PParams ledgerera)
toShelleyPParams
    protocolParameters@ProtocolParameters {
      protocolParamDecentralization
    , protocolParamExtraPraosEntropy
    , protocolParamMinUTxOValue
    } = do
  ppCommon <- toShelleyCommonPParams protocolParameters
  d <- boundRationalEither "D" =<< maybeToRight (PpceMissingParameter "decentralization") protocolParamDecentralization
  minUTxOValue <- maybeToRight (PpceMissingParameter "protocolParamMinUTxOValue") protocolParamMinUTxOValue
  let ppShelley =
        ppCommon
        & ppDL            .~ d
        & ppExtraEntropyL .~ toLedgerNonce protocolParamExtraPraosEntropy
        & ppMinUTxOValueL .~ minUTxOValue
  pure ppShelley


toAlonzoCommonPParams :: AlonzoEraPParams ledgerera
                      => ProtocolParameters
                      -> Either ProtocolParametersConversionError (PParams ledgerera)
toAlonzoCommonPParams
    protocolParameters@ProtocolParameters {
      protocolParamCostModels
    , protocolParamPrices
    , protocolParamMaxTxExUnits
    , protocolParamMaxBlockExUnits
    , protocolParamMaxValueSize
    , protocolParamCollateralPercent
    , protocolParamMaxCollateralInputs
    } = do
  ppShelleyCommon <- toShelleyCommonPParams protocolParameters
  costModels <- toAlonzoCostModels protocolParamCostModels
  prices <-
    requireParam "protocolParamPrices" toAlonzoPrices protocolParamPrices
  maxTxExUnits <-
    requireParam "protocolParamMaxTxExUnits" Right protocolParamMaxTxExUnits
  maxBlockExUnits <-
    requireParam "protocolParamMaxBlockExUnits" Right protocolParamMaxBlockExUnits
  maxValueSize <-
    requireParam "protocolParamMaxBlockExUnits" Right  protocolParamMaxValueSize
  collateralPercent <-
    requireParam "protocolParamCollateralPercent" Right protocolParamCollateralPercent
  maxCollateralInputs <-
    requireParam "protocolParamMaxCollateralInputs" Right protocolParamMaxCollateralInputs
  let ppAlonzoCommon =
        ppShelleyCommon
        & ppCostModelsL           .~ costModels
        & ppPricesL               .~ prices
        & ppMaxTxExUnitsL         .~ toAlonzoExUnits maxTxExUnits
        & ppMaxBlockExUnitsL      .~ toAlonzoExUnits maxBlockExUnits
        & ppMaxValSizeL           .~ maxValueSize
        & ppCollateralPercentageL .~ collateralPercent
        & ppMaxCollateralInputsL  .~ maxCollateralInputs
  pure ppAlonzoCommon

toAlonzoPParams :: Ledger.Crypto crypto
                => ProtocolParameters
                -> Either ProtocolParametersConversionError (PParams (Ledger.AlonzoEra crypto))
toAlonzoPParams
    protocolParameters@ProtocolParameters {
      protocolParamDecentralization
    } = do
  ppAlonzoCommon <- toAlonzoCommonPParams protocolParameters
  d <- requireParam "protocolParamDecentralization"
                    (boundRationalEither "D")
                    protocolParamDecentralization
  let ppAlonzo =
        ppAlonzoCommon
        & ppDL .~ d
  pure ppAlonzo


toBabbagePParams :: BabbageEraPParams ledgerera
                 => ProtocolParameters
                 -> Either ProtocolParametersConversionError (PParams ledgerera)
toBabbagePParams
    protocolParameters@ProtocolParameters {
      protocolParamUTxOCostPerByte
    } = do
  ppAlonzoCommon <- toAlonzoCommonPParams protocolParameters
  utxoCostPerByte <-
    requireParam "protocolParamUTxOCostPerByte" Right protocolParamUTxOCostPerByte
  let ppBabbage =
        ppAlonzoCommon
          & ppCoinsPerUTxOByteL .~ CoinPerByte utxoCostPerByte
  pure ppBabbage

toConwayPParams :: BabbageEraPParams ledgerera
                => ProtocolParameters
                -> Either ProtocolParametersConversionError (PParams ledgerera)
toConwayPParams = toBabbagePParams

-- ----------------------------------------------------------------------------
-- Conversion functions: protocol parameters from ledger types
--

fromLedgerPParams
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> ProtocolParameters
fromLedgerPParams ShelleyBasedEraShelley = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraAllegra = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraMary    = fromShelleyPParams
fromLedgerPParams ShelleyBasedEraAlonzo  = fromExactlyAlonzoPParams
fromLedgerPParams ShelleyBasedEraBabbage = fromBabbagePParams
fromLedgerPParams ShelleyBasedEraConway  = fromConwayPParams


fromShelleyCommonPParams :: EraPParams ledgerera
                         => PParams ledgerera
                         -> ProtocolParameters
fromShelleyCommonPParams pp =
    ProtocolParameters {
      protocolParamProtocolVersion     = case pp ^. ppProtocolVersionL of
                                           Ledger.ProtVer a b -> (Ledger.getVersion a, b)
    , protocolParamMaxBlockHeaderSize  = fromIntegral $ pp ^. ppMaxBHSizeL
    , protocolParamMaxBlockBodySize    = fromIntegral $ pp ^. ppMaxBBSizeL
    , protocolParamMaxTxSize           = fromIntegral $ pp ^. ppMaxTxSizeL
    , protocolParamTxFeeFixed          = pp ^. ppMinFeeBL
    , protocolParamTxFeePerByte        = pp ^. ppMinFeeAL
    , protocolParamStakeAddressDeposit = pp ^. ppKeyDepositL
    , protocolParamStakePoolDeposit    = pp ^. ppPoolDepositL
    , protocolParamMinPoolCost         = pp ^. ppMinPoolCostL
    , protocolParamPoolRetireMaxEpoch  = pp ^. ppEMaxL
    , protocolParamStakePoolTargetNum  = pp ^. ppNOptL
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational (pp ^. ppA0L)
    , protocolParamMonetaryExpansion   = Ledger.unboundRational (pp ^. ppRhoL)
    , protocolParamTreasuryCut         = Ledger.unboundRational (pp ^. ppTauL)
    , protocolParamCostModels          = mempty  -- Only from Alonzo onwards
    , protocolParamPrices              = Nothing -- Only from Alonzo onwards
    , protocolParamMaxTxExUnits        = Nothing -- Only from Alonzo onwards
    , protocolParamMaxBlockExUnits     = Nothing -- Only from Alonzo onwards
    , protocolParamMaxValueSize        = Nothing -- Only from Alonzo onwards
    , protocolParamCollateralPercent   = Nothing -- Only from Alonzo onwards
    , protocolParamMaxCollateralInputs = Nothing -- Only from Alonzo onwards
    , protocolParamUTxOCostPerByte     = Nothing -- Only from Babbage onwards
    , protocolParamDecentralization    = Nothing -- Obsolete from Babbage onwards
    , protocolParamExtraPraosEntropy   = Nothing -- Obsolete from Alonzo onwards
    , protocolParamMinUTxOValue        = Nothing -- Obsolete from Alonzo onwards
    }

fromShelleyPParams :: ( EraPParams ledgerera
                      , Ledger.AtMostEra Ledger.MaryEra ledgerera
                      , Ledger.AtMostEra Ledger.AlonzoEra ledgerera
                      )
                   => PParams ledgerera
                   -> ProtocolParameters
fromShelleyPParams pp =
  (fromShelleyCommonPParams pp) {
      protocolParamDecentralization    = Just . Ledger.unboundRational $ pp ^. ppDL
    , protocolParamExtraPraosEntropy   = fromLedgerNonce $ pp ^. ppExtraEntropyL
    , protocolParamMinUTxOValue        = Just $ pp ^. ppMinUTxOValueL
    }


fromAlonzoPParams :: AlonzoEraPParams ledgerera
                        => PParams ledgerera
                        -> ProtocolParameters
fromAlonzoPParams pp =
  (fromShelleyCommonPParams pp) {
      protocolParamCostModels          = fromAlonzoCostModels $ pp ^. ppCostModelsL
    , protocolParamDecentralization    = Just . Ledger.unboundRational $ pp ^. ppDG
    , protocolParamPrices              = Just . fromAlonzoPrices $ pp ^. ppPricesL
    , protocolParamMaxTxExUnits        = Just . fromAlonzoExUnits $ pp ^. ppMaxTxExUnitsL
    , protocolParamMaxBlockExUnits     = Just . fromAlonzoExUnits $ pp ^. ppMaxBlockExUnitsL
    , protocolParamMaxValueSize        = Just $ pp ^. ppMaxValSizeL
    , protocolParamCollateralPercent   = Just $ pp ^. ppCollateralPercentageL
    , protocolParamMaxCollateralInputs = Just $ pp ^. ppMaxCollateralInputsL
    }

fromExactlyAlonzoPParams :: (AlonzoEraPParams ledgerera, Ledger.ExactEra Ledger.AlonzoEra ledgerera)
                        => PParams ledgerera
                        -> ProtocolParameters
fromExactlyAlonzoPParams pp =
  (fromAlonzoPParams pp) {
      protocolParamUTxOCostPerByte = Just . unCoinPerWord $ pp ^. ppCoinsPerUTxOWordL
  }

fromBabbagePParams :: BabbageEraPParams ledgerera
                   => PParams ledgerera
                   -> ProtocolParameters
fromBabbagePParams pp =
  (fromAlonzoPParams pp)
    { protocolParamUTxOCostPerByte = Just . unCoinPerByte $ pp ^. ppCoinsPerUTxOByteL
  , protocolParamDecentralization = Nothing
  }

fromConwayPParams :: BabbageEraPParams ledgerera
                  => PParams ledgerera
                  -> ProtocolParameters
fromConwayPParams = fromBabbagePParams

checkProtocolParameters :: ()
  => ShelleyBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersError ()
checkProtocolParameters sbe ProtocolParameters{..} =
  case sbe of
    ShelleyBasedEraShelley -> checkMinUTxOVal
    ShelleyBasedEraAllegra -> checkMinUTxOVal
    ShelleyBasedEraMary -> checkMinUTxOVal
    ShelleyBasedEraAlonzo -> checkAlonzoParams
    ShelleyBasedEraBabbage -> checkBabbageParams
    ShelleyBasedEraConway -> checkBabbageParams
 where
   era = shelleyBasedToCardanoEra sbe

   cModel = not $ Map.null protocolParamCostModels
   prices = isJust protocolParamPrices
   maxTxUnits = isJust protocolParamMaxTxExUnits
   maxBlockExUnits = isJust protocolParamMaxBlockExUnits
   maxValueSize = isJust protocolParamMaxValueSize
   collateralPercent = isJust protocolParamCollateralPercent
   maxCollateralInputs = isJust protocolParamMaxCollateralInputs
   costPerByte = isJust protocolParamUTxOCostPerByte
   decentralization = isJust protocolParamDecentralization
   extraPraosEntropy = isJust protocolParamExtraPraosEntropy

   alonzoPParamFieldsRequirements :: [Bool]
   alonzoPParamFieldsRequirements =
     [     cModel
     ,     prices
     ,     maxTxUnits
     ,     maxBlockExUnits
     ,     maxValueSize
     ,     collateralPercent
     ,     maxCollateralInputs
     , not costPerByte
     ]

   babbagePParamFieldsRequirements :: [Bool]
   babbagePParamFieldsRequirements =
     [     cModel
     ,     prices
     ,     maxTxUnits
     ,     maxBlockExUnits
     ,     maxValueSize
     ,     collateralPercent
     ,     maxCollateralInputs
     ,     costPerByte
     , not decentralization
     , not extraPraosEntropy
     ]

   checkAlonzoParams :: Either ProtocolParametersError ()
   checkAlonzoParams = do
     if all (== True) alonzoPParamFieldsRequirements
     then return ()
     else Left PParamsErrorMissingAlonzoProtocolParameter

   checkBabbageParams :: Either ProtocolParametersError ()
   checkBabbageParams =
     if all (== True) babbagePParamFieldsRequirements
     then return ()
     else Left PParamsErrorMissingAlonzoProtocolParameter

   checkMinUTxOVal :: Either ProtocolParametersError ()
   checkMinUTxOVal =
     if isJust protocolParamMinUTxOValue
      then return ()
      else Left . PParamsErrorMissingMinUTxoValue $ cardanoEraConstraints era $ AnyCardanoEra era


data ProtocolParametersError
  = PParamsErrorMissingMinUTxoValue !AnyCardanoEra
  | PParamsErrorMissingAlonzoProtocolParameter
  deriving (Show)

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
  | PpceInvalidCostModel !CostModel !Alonzo.CostModelApplyError
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
