{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.Genesis
  ( ShelleyGenesis(..)
  , shelleyGenesisDefaults
  , alonzoGenesisDefaults
  , conwayGenesisDefaults

  -- ** Configuration
  , ByronGenesisConfig
  , ShelleyGenesisConfig
  , AlonzoGenesisConfig
  , ConwayGenesisConfig

  , ShelleyConfig(..)
  , GenesisHashByron(..)
  , GenesisHashShelley(..)
  , GenesisHashAlonzo(..)
  , GenesisHashConway(..)

  -- ** Files
  , ByronGenesisFile
  , ShelleyGenesisFile
  , AlonzoGenesisFile
  , ConwayGenesisFile
  ) where

import           Cardano.Api.IO
import           Cardano.Api.Utils (unsafeBoundedRational)

import qualified Cardano.Chain.Genesis
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..))
import           Cardano.Ledger.Api (CoinPerWord (..), ConwayEra)
import           Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import           Cardano.Ledger.Conway.Governance (Committee (..))
import           Cardano.Ledger.Conway.PParams (DRepVotingThresholds (..),
                   PoolVotingThresholds (..), UpgradeConwayPParams (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Plutus (Language (..))
import           Cardano.Ledger.Plutus.CostModels (mkCostModelsLenient)
import           Cardano.Ledger.Shelley.Core
import           Cardano.Ledger.Shelley.Genesis (NominalDiffTimeMicro, ShelleyGenesis (..),
                   emptyGenesisStaking)
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley

import           Data.Aeson (decode)
import           Data.ByteString (ByteString)
import           Data.Functor.Identity (Identity)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Time as Time
import           Data.Typeable
import           GHC.Stack (HasCallStack)
import           Lens.Micro

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Ledger.ShelleyGenesis Shelley.StandardCrypto)
  , scGenesisHash :: !GenesisHashShelley
  }

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashConway = GenesisHashConway
  { unGenesisHashConway :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

type ByronGenesisConfig = Cardano.Chain.Genesis.Config
type ShelleyGenesisConfig = ShelleyConfig
type AlonzoGenesisConfig = AlonzoGenesis
type ConwayGenesisConfig = ConwayGenesis Shelley.StandardCrypto

type ByronGenesisFile = File ByronGenesisConfig
type ShelleyGenesisFile = File ShelleyGenesisConfig
type AlonzoGenesisFile = File AlonzoGenesisConfig
type ConwayGenesisFile = File ConwayGenesisConfig

-- | Some reasonable starting defaults for constructing a 'ShelleyGenesis'.
--
-- You must override at least the following fields for this to be useful:
--
-- * 'sgSystemStart' the time of the first block
-- * 'sgNetworkMagic' to a suitable testnet or mainnet network magic number.
-- * 'sgGenDelegs' to have some initial nodes
-- * 'sgInitialFunds' to have any money in the system
-- * 'sgMaxLovelaceSupply' must be at least the sum of the 'sgInitialFunds'
--   but more if you want to allow for rewards.
--
shelleyGenesisDefaults :: ShelleyGenesis StandardCrypto
shelleyGenesisDefaults =
  ShelleyGenesis
    {
      -- parameters for this specific chain
      sgSystemStart           = zeroTime
    , sgNetworkMagic          = 42
    , sgNetworkId             = Ledger.Testnet

      -- consensus protocol parameters
    , sgSlotLength            = 1.0 :: NominalDiffTimeMicro -- 1s slots
    , sgActiveSlotsCoeff      = unsafeBR (1 % 20)   -- f ; 1/f = 20s block times on average
    , sgSecurityParam         = k
    , sgEpochLength           = Ledger.EpochSize (k * 10 * 20) -- 10k/f
    , sgSlotsPerKESPeriod     = 60 * 60 * 36        -- 1.5 days with 1s slots
    , sgMaxKESEvolutions      = 60                  -- 90 days
    , sgUpdateQuorum          = 5                   -- assuming 7 genesis keys

    -- ledger protocol parameters
    , sgProtocolParams        =
        emptyPParams
        & ppDL         .~ maxBound
        & ppMaxBHSizeL .~ 1100                  -- TODO: compute from crypto
        & ppMaxBBSizeL .~ 64 * 1024             -- max 64kb blocks
        & ppMaxTxSizeL .~ 16 * 1024             -- max 16kb txs
        & ppEMaxL      .~ EpochInterval 18
        & ppMinFeeAL   .~ Coin 1                -- The linear factor for the minimum fee calculation
        & ppMinFeeBL   .~ Coin 0                -- The constant factor for the minimum fee calculation
                                                -- pot = tx_fees + ρ * remaining_reserves
        & ppRhoL       .~ unsafeBR (1 % 10)     -- How much of reserves goes into pot
        & ppTauL       .~ unsafeBR (1 % 10)     -- τ * remaining_reserves is sent to treasury every epoch

      -- genesis keys and initial funds
    , sgGenDelegs             = Map.empty
    , sgStaking               = emptyGenesisStaking
    , sgInitialFunds          = ListMap.empty
    , sgMaxLovelaceSupply     = 0
    }
  where
    k = 2160
    zeroTime = Time.UTCTime (Time.fromGregorian 1970 1 1) 0 -- tradition
    unsafeBR :: (HasCallStack, Typeable r, BoundedRational r) => Rational -> r
    unsafeBR = unsafeBoundedRational

-- | Some reasonable starting defaults for constructing a 'ConwayGenesis'.
conwayGenesisDefaults :: ConwayGenesis StandardCrypto
conwayGenesisDefaults = ConwayGenesis { cgUpgradePParams = defaultUpgradeConwayParams
                                      , cgConstitution = blankConstitution
                                      , cgCommittee = emptyComittee
                                      , cgDelegs = mempty
                                      , cgInitialDReps = mempty
                                      }
  where
  defaultUpgradeConwayParams :: UpgradeConwayPParams Identity
  defaultUpgradeConwayParams = UpgradeConwayPParams { ucppPoolVotingThresholds = defaultPoolVotingThresholds
                                                    , ucppGovActionLifetime = EpochInterval 10
                                                    , ucppGovActionDeposit = Coin 1000000000
                                                    , ucppDRepVotingThresholds = defaultDRepVotingThresholds
                                                    , ucppDRepDeposit = Coin 2000000
                                                    , ucppDRepActivity = EpochInterval 20
                                                    , ucppCommitteeMinSize = 0
                                                    , ucppCommitteeMaxTermLength = EpochInterval 200
                                                    }
    where
    defaultPoolVotingThresholds :: PoolVotingThresholds
    defaultPoolVotingThresholds = PoolVotingThresholds { pvtPPSecurityGroup = fromJust $ boundRational 0.51
                                                       , pvtMotionNoConfidence = fromJust $ boundRational 0.51
                                                       , pvtHardForkInitiation = fromJust $ boundRational 0.51
                                                       , pvtCommitteeNormal = fromJust $ boundRational 0.51
                                                       , pvtCommitteeNoConfidence = fromJust $ boundRational 0.51
                                                       }

    defaultDRepVotingThresholds :: DRepVotingThresholds
    defaultDRepVotingThresholds = DRepVotingThresholds { dvtUpdateToConstitution = fromJust $ boundRational 0.51
                                                       , dvtTreasuryWithdrawal = fromJust $ boundRational 0.51
                                                       , dvtPPTechnicalGroup = fromJust $ boundRational 0.51
                                                       , dvtPPNetworkGroup = fromJust $ boundRational 0.51
                                                       , dvtPPGovGroup = fromJust $ boundRational 0.51
                                                       , dvtPPEconomicGroup = fromJust $ boundRational 0.51
                                                       , dvtMotionNoConfidence = fromJust $ boundRational 0.51
                                                       , dvtHardForkInitiation = fromJust $ boundRational 0.51
                                                       , dvtCommitteeNormal = fromJust $ boundRational 0.51
                                                       , dvtCommitteeNoConfidence = fromJust $ boundRational 0.51
                                                       }

  blankConstitution :: Constitution (ConwayEra StandardCrypto)
  blankConstitution = Constitution { constitutionAnchor = emptyAnchor
                                   , constitutionScript = SNothing
                                   }
    where
    emptyAnchor :: Anchor StandardCrypto
    emptyAnchor = Anchor { anchorUrl = fromJust $ textToUrl 0 ""
                         , anchorDataHash = fromJust $ decode "\"0000000000000000000000000000000000000000000000000000000000000000\""
                         }

  emptyComittee :: Committee (ConwayEra StandardCrypto)
  emptyComittee = Committee { committeeQuorum = fromJust $ boundRational 0
                            , committeeMembers = mempty
                            }

-- | Some reasonable starting defaults for constructing a 'AlonzoGenesis'.
alonzoGenesisDefaults :: AlonzoGenesis
alonzoGenesisDefaults = AlonzoGenesis { agPrices = Prices { prSteps = fromJust $ boundRational 721
                                                          , prMem = fromJust $ boundRational 10000000
                                                          }
                                     , agMaxValSize = 5000
                                     , agMaxTxExUnits = ExUnits { exUnitsMem = 10000000
                                                                , exUnitsSteps = 10000000000
                                                                }
                                     , agMaxCollateralInputs = 3
                                     , agMaxBlockExUnits =  ExUnits { exUnitsMem = 50000000
                                                                    , exUnitsSteps = 40000000000
                                                                    }
                                     , agCostModels = apiCostModels
                                     , agCollateralPercentage = 150
                                     , agCoinsPerUTxOWord = CoinPerWord $ Coin 34482
                                     }
  where
    -- Based on cardano-node/cardano-testnet/src/Testnet/Defaults.hs
    apiCostModels = mkCostModelsLenient $ Map.fromList [ (fromIntegral $ fromEnum PlutusV1, defaultV1CostModel)
                                                       , (fromIntegral $ fromEnum PlutusV2, defaultV2CostModel)
                                                       , (fromIntegral $ fromEnum PlutusV3, defaultV3CostModel)
                                                       ]
      where
        defaultV1CostModel = [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
                             , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
                             , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
                             , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
                             , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
                             , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473
                             , 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
                             , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0
                             , 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0
                             , 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357
                             , 32, 32247, 32, 38314, 32, 57996947, 18975, 10
                             ]
        defaultV2CostModel = [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
                             , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
                             , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
                             , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
                             , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
                             , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473
                             , 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
                             , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0
                             , 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990, 30482, 4, 1927926
                             , 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220
                             , 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10, 9462713, 1021, 10, 38887044
                             , 32947, 10
                             ]
        defaultV3CostModel = [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 117366, 10475, 4, 832808, 18
                             , 3209094, 6, 331451, 1, 65990684, 23097, 18, 114242, 18, 94393407
                             , 87060, 18, 16420089, 18, 2145798, 36, 3795345, 12, 889023, 1, 204237282, 23271, 36, 129165, 36, 189977790
                             , 85902, 36, 33012864, 36, 388443360, 1, 401885761, 72, 2331379, 72, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000
                             , 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100
                             , 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662
                             , 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32
                             , 43249, 1000, 32, 32, 80556, 1, 57667, 4, 1927926, 82523, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896
                             , 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0
                             , 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990
                             , 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182
                             , 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35190005, 10, 57996947, 18975, 10, 39121781, 32260, 10
                             ]
