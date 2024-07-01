{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Genesis
  ( ShelleyGenesis(..)
  , shelleyGenesisDefaults
  , alonzoGenesisDefaults
  , decodeAlonzoGenesis
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

import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eras.Core
import           Cardano.Api.IO
import           Cardano.Api.Monad.Error
import           Cardano.Api.Utils (unsafeBoundedRational)

import qualified Cardano.Chain.Genesis
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..))
import           Cardano.Ledger.Api (CoinPerWord (..))
import           Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import           Cardano.Ledger.Conway.PParams (DRepVotingThresholds (..),
                   PoolVotingThresholds (..), UpgradeConwayPParams (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Plutus (Language (..))
import qualified Cardano.Ledger.Plutus as L
import           Cardano.Ledger.Plutus.CostModels (mkCostModelsLenient)
import           Cardano.Ledger.Shelley.Core
import           Cardano.Ledger.Shelley.Genesis (NominalDiffTimeMicro, ShelleyGenesis (..),
                   emptyGenesisStaking)
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley
import qualified PlutusLedgerApi.Common as V2
import qualified PlutusLedgerApi.V2 as V2

import           Control.Monad.Trans.Fail.String (errorFail)
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Default.Class as DefaultClass
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.List (sortOn)
import qualified Data.ListMap as ListMap
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Time as Time
import           Data.Typeable
import qualified Data.Vector as V
import           GHC.Exts (IsList (..))
import           GHC.Stack (HasCallStack)
import           Lens.Micro
import qualified Lens.Micro.Aeson as AL

import           Test.Cardano.Ledger.Core.Rational ((%!))
import           Test.Cardano.Ledger.Plutus (testingCostModelV3)

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
    , sgGenDelegs             = M.empty
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
-- Based on https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
conwayGenesisDefaults :: ConwayGenesis StandardCrypto
conwayGenesisDefaults = ConwayGenesis { cgUpgradePParams = defaultUpgradeConwayParams
                                      , cgConstitution = DefaultClass.def
                                      , cgCommittee = DefaultClass.def
                                      , cgDelegs = mempty
                                      , cgInitialDReps = mempty
                                      }
  where
  defaultUpgradeConwayParams :: UpgradeConwayPParams Identity
  defaultUpgradeConwayParams = UpgradeConwayPParams { ucppPoolVotingThresholds = defaultPoolVotingThresholds
                                                    , ucppGovActionLifetime = EpochInterval 1
                                                    , ucppGovActionDeposit = Coin 1000000
                                                    , ucppDRepVotingThresholds = defaultDRepVotingThresholds
                                                    , ucppDRepDeposit = Coin 1000000
                                                    , ucppDRepActivity = EpochInterval 100
                                                    , ucppCommitteeMinSize = 0
                                                    , ucppCommitteeMaxTermLength = EpochInterval 200
                                                    , ucppMinFeeRefScriptCostPerByte = 0 %! 1 -- TODO: set to correct value after benchmarking
                                                    , ucppPlutusV3CostModel = testingCostModelV3
                                                    }
    where
    defaultPoolVotingThresholds :: PoolVotingThresholds
    defaultPoolVotingThresholds = PoolVotingThresholds { pvtPPSecurityGroup = 1 %! 2
                                                       , pvtMotionNoConfidence = 1 %! 2
                                                       , pvtHardForkInitiation = 1 %! 2
                                                       , pvtCommitteeNormal = 1 %! 2
                                                       , pvtCommitteeNoConfidence = 1 %! 2
                                                       }

    defaultDRepVotingThresholds :: DRepVotingThresholds
    defaultDRepVotingThresholds = DRepVotingThresholds { dvtUpdateToConstitution = 0 %! 1
                                                       , dvtTreasuryWithdrawal = 1 %! 2
                                                       , dvtPPTechnicalGroup = 1 %! 2
                                                       , dvtPPNetworkGroup = 1 %! 2
                                                       , dvtPPGovGroup = 1 %! 2
                                                       , dvtPPEconomicGroup = 1 %! 2
                                                       , dvtMotionNoConfidence = 0 %! 1
                                                       , dvtHardForkInitiation = 1 %! 2
                                                       , dvtCommitteeNormal = 1 %! 2
                                                       , dvtCommitteeNoConfidence = 0 %! 1
                                                       }

decodeAlonzoGenesis :: forall era t m. MonadTransError String t m
                    => AlonzoEraOnwards era
                    -> LBS.ByteString
                    -> t m AlonzoGenesis
decodeAlonzoGenesis aeo genesisBs = modifyError ("Cannot decode Alonzo genesis: " <>) $ do
  genesisValue :: A.Value <- liftEither $ A.eitherDecode genesisBs
  -- Making a fixup of a costmodel is easier before JSON deserialization. This also saves us from building
  -- plutus' EvaluationContext one more time after cost model update.
  genesisValue' <- (AL.key "costModels" . AL.key "PlutusV2" . AL._Value) setCostModelDefaultValues genesisValue
  fromJsonE genesisValue'
  where
    setCostModelDefaultValues :: A.Value -> ExceptT String m A.Value
    setCostModelDefaultValues = \case

      obj@(A.Object _) -> do
        -- decode cost model into a map first
        costModel :: Map V2.ParamName Int64 <- modifyError ("Decoding cost model object: " <> ) $ fromJsonE obj
        setCostModelDefaultValues
          . A.toJSON -- convert to an array representation of Int64 values
          . fmap snd
          . sortOn fst -- ensure proper order of params in the list
          . toList
          . (`M.union` costModelDefaultValues) -- add default values of missing params
          $ costModel

      A.Array vec
        | V.length vec < costModelExpectedLength -> pure . A.Array . V.take costModelExpectedLength $ vec <> (A.toJSON <$> optionalCostModelDefaultValues)
        | V.length vec > costModelExpectedLength -> pure . A.Array $ V.take costModelExpectedLength vec

      other -> pure other

    costModelExpectedLength :: Int
    costModelExpectedLength
      | isConwayOnwards = length allCostModelParams
      | otherwise = L.costModelParamsCount L.PlutusV2 -- Babbage

    optionalCostModelDefaultValues :: (Item l ~ Int64, IsList l) => l
    optionalCostModelDefaultValues = fromList $ replicate (length optionalV2costModelParams) maxBound

    costModelDefaultValues :: Map V2.ParamName Int64
    costModelDefaultValues = fromList $ map (, maxBound) allCostModelParams

    allCostModelParams :: [V2.ParamName]
    allCostModelParams = [minBound..maxBound]

    optionalV2costModelParams :: [Text]
    optionalV2costModelParams = map V2.showParamName
      [ V2.IntegerToByteString'cpu'arguments'c0
      , V2.IntegerToByteString'cpu'arguments'c1
      , V2.IntegerToByteString'cpu'arguments'c2
      , V2.IntegerToByteString'memory'arguments'intercept
      , V2.IntegerToByteString'memory'arguments'slope
      , V2.ByteStringToInteger'cpu'arguments'c0
      , V2.ByteStringToInteger'cpu'arguments'c1
      , V2.ByteStringToInteger'cpu'arguments'c2
      , V2.ByteStringToInteger'memory'arguments'intercept
      , V2.ByteStringToInteger'memory'arguments'slope
      ]

    fromJsonE :: A.FromJSON a => A.Value -> ExceptT String m a
    fromJsonE v =
      case A.fromJSON v of
        A.Success a -> pure a
        A.Error e -> throwError e

    isConwayOnwards = isJust $ forEraMaybeEon @ConwayEraOnwards (toCardanoEra aeo)

-- | Some reasonable starting defaults for constructing a 'AlonzoGenesis'.
-- Based on https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
alonzoGenesisDefaults :: AlonzoGenesis
alonzoGenesisDefaults = AlonzoGenesis { agPrices = Prices { prSteps = 721 %! 10000000
                                                          , prMem = 577 %! 10000
                                                          }
                                     , agMaxValSize = 5000
                                     , agMaxTxExUnits = ExUnits { exUnitsMem = 140000000
                                                                , exUnitsSteps = 10000000000
                                                                }
                                     , agMaxCollateralInputs = 3
                                     , agMaxBlockExUnits =  ExUnits { exUnitsMem = 62000000
                                                                    , exUnitsSteps = 20000000000
                                                                    }
                                     , agCostModels = errorFail apiCostModels
                                     , agCollateralPercentage = 150
                                     , agCoinsPerUTxOWord = CoinPerWord $ Coin 34482
                                     }
  where
    apiCostModels = mkCostModelsLenient $ fromList [ (fromIntegral $ fromEnum PlutusV1, defaultV1CostModel)
                                                       , (fromIntegral $ fromEnum PlutusV2, defaultV2CostModel)
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
                             -- TODO add here those new alonzo cost parametes in conway era
                             , 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 -- FIXME: REMOVEME
                             ]
