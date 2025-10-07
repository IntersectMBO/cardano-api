{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Genesis.Internal
  ( ShelleyGenesis (..)
  , shelleyGenesisDefaults
  , alonzoGenesisDefaults
  , conwayGenesisDefaults
  , dijkstraGenesisDefaults

    -- ** Configuration
  , ByronGenesisConfig
  , ShelleyGenesisConfig
  , AlonzoGenesisConfig
  , ConwayGenesisConfig
  , ShelleyConfig (..)
  , GenesisHashByron (..)
  , GenesisHashShelley (..)
  , GenesisHashAlonzo (..)
  , GenesisHashConway (..)

    -- ** Files
  , ByronGenesisFile
  , ShelleyGenesisFile
  , AlonzoGenesisFile
  , ConwayGenesisFile

    -- * Utilities
  , unsafeBoundedRational
  )
where

import Cardano.Api.IO

import Cardano.Chain.Genesis qualified
import Cardano.Crypto.Hash.Blake2b qualified
import Cardano.Crypto.Hash.Class qualified
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..))
import Cardano.Ledger.Api (CoinPerWord (..))
import Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.PParams
  ( DRepVotingThresholds (..)
  , PoolVotingThresholds (..)
  , UpgradeConwayPParams (..)
  )
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Plutus qualified as L
import Cardano.Ledger.Plutus.CostModels (mkCostModelsLenient)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Genesis
  ( NominalDiffTimeMicro
  , ShelleyGenesis (..)
  , emptyGenesisStaking
  )
import Cardano.Ledger.Shelley.Genesis qualified as Ledger
import PlutusCore.Evaluation.Machine.BuiltinCostModel
import PlutusCore.Evaluation.Machine.CostModelInterface
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults
import PlutusCore.Evaluation.Machine.MachineParameters
import PlutusLedgerApi.Common (IsParamName, readParamName)
import PlutusLedgerApi.V3 qualified as V3

import Control.Monad
import Control.Monad.Trans.Fail.String (errorFail)
import Data.ByteString (ByteString)
import Data.Default.Class qualified as DefaultClass
import Data.Functor.Identity
import Data.Int (Int64)
import Data.ListMap qualified as ListMap
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import Data.Time qualified as Time
import Data.Typeable
import GHC.Exts (IsList (..))
import GHC.Stack (HasCallStack)
import Lens.Micro

import Barbies (bmap)
import UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts

data ShelleyConfig = ShelleyConfig
  { scConfig :: !Ledger.ShelleyGenesis
  , scGenesisHash :: !GenesisHashShelley
  }

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  }
  deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley
      :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo
      :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show)

newtype GenesisHashConway = GenesisHashConway
  { unGenesisHashConway
      :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show)

type ByronGenesisConfig = Cardano.Chain.Genesis.Config

type ShelleyGenesisConfig = ShelleyConfig

type AlonzoGenesisConfig = AlonzoGenesis

type ConwayGenesisConfig = ConwayGenesis

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
shelleyGenesisDefaults :: ShelleyGenesis
shelleyGenesisDefaults =
  ShelleyGenesis
    { -- parameters for this specific chain
      sgSystemStart = zeroTime
    , sgNetworkMagic = 42
    , sgNetworkId = Ledger.Testnet
    , -- consensus protocol parameters
      sgSlotLength = 1.0 :: NominalDiffTimeMicro -- 1s slots
    , sgActiveSlotsCoeff = unsafeBR (1 % 20) -- f ; 1/f = 20s block times on average
    , sgSecurityParam = k
    , sgEpochLength = Ledger.EpochSize (unNonZero k * 10 * 20) -- 10k/f
    , sgSlotsPerKESPeriod = 60 * 60 * 36 -- 1.5 days with 1s slots
    , sgMaxKESEvolutions = 60 -- 90 days
    , sgUpdateQuorum = 5 -- assuming 7 genesis keys
    , -- ledger protocol parameters
      sgProtocolParams =
        emptyPParams
          & ppDL .~ maxBound
          & ppMaxBHSizeL .~ 1100 -- TODO: compute from crypto
          & ppMaxBBSizeL .~ 64 * 1024 -- max 64kb blocks
          & ppMaxTxSizeL .~ 16 * 1024 -- max 16kb txs
          & ppEMaxL .~ EpochInterval 18
          & ppMinFeeAL .~ Coin 1 -- The linear factor for the minimum fee calculation
          & ppMinFeeBL .~ Coin 0 -- The constant factor for the minimum fee calculation
          -- pot = tx_fees + ρ * remaining_reserves
          & ppRhoL .~ unsafeBR (1 % 10) -- How much of reserves goes into pot
          & ppTauL .~ unsafeBR (1 % 10) -- τ * remaining_reserves is sent to treasury every epoch
          & ppKeyDepositL .~ L.Coin 400000 -- require a non-zero deposit when registering keys
    , -- genesis keys and initial funds
      sgGenDelegs = M.empty
    , sgStaking = emptyGenesisStaking
    , sgInitialFunds = ListMap.empty
    , sgMaxLovelaceSupply = 0
    }
 where
  k = knownNonZeroBounded @2160
  zeroTime = Time.UTCTime (Time.fromGregorian 1970 1 1) 0 -- tradition
  unsafeBR :: (HasCallStack, Typeable r, BoundedRational r) => Rational -> r
  unsafeBR = unsafeBoundedRational

dijkstraGenesisDefaults :: DijkstraGenesis
dijkstraGenesisDefaults =
  -- copied from: https://github.com/IntersectMBO/cardano-ledger/blob/232511b0fa01cd848cd7a569d1acc322124cf9b8/eras/dijkstra/impl/testlib/Test/Cardano/Ledger/Dijkstra/ImpTest.hs#L121
  DijkstraGenesis
    { dgUpgradePParams =
        UpgradeDijkstraPParams
          { udppMaxRefScriptSizePerBlock = 1024 * 1024 -- 1MiB
          , udppMaxRefScriptSizePerTx = 200 * 1024 -- 200KiB
          , udppRefScriptCostStride = knownNonZeroBounded @25600 -- 25 KiB
          , udppRefScriptCostMultiplier = fromJust $ boundRational 1.2
          }
    }

-- | Some reasonable starting defaults for constructing a 'ConwayGenesis'.
-- Based on https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
conwayGenesisDefaults :: ConwayGenesis
conwayGenesisDefaults =
  ConwayGenesis
    { cgUpgradePParams = defaultUpgradeConwayParams
    , cgConstitution = DefaultClass.def
    , cgCommittee = DefaultClass.def
    , cgDelegs = mempty
    , cgInitialDReps = mempty
    }
 where
  defaultUpgradeConwayParams :: UpgradeConwayPParams Identity
  defaultUpgradeConwayParams =
    UpgradeConwayPParams
      { ucppPoolVotingThresholds = defaultPoolVotingThresholds
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
    defaultPoolVotingThresholds =
      PoolVotingThresholds
        { pvtPPSecurityGroup = 1 %! 2
        , pvtMotionNoConfidence = 1 %! 2
        , pvtHardForkInitiation = 1 %! 2
        , pvtCommitteeNormal = 1 %! 2
        , pvtCommitteeNoConfidence = 1 %! 2
        }

    defaultDRepVotingThresholds :: DRepVotingThresholds
    defaultDRepVotingThresholds =
      DRepVotingThresholds
        { dvtUpdateToConstitution = 0 %! 1
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
    testingCostModelV3 :: HasCallStack => L.CostModel
    testingCostModelV3 = mkCostModel' PlutusV3 $ snd <$> costModelParamsForTesting

    mkCostModel' :: (Integral i, Show i, HasCallStack) => Language -> [i] -> L.CostModel
    mkCostModel' lang params =
      case L.mkCostModel lang $ map fromIntegral params of
        Left err ->
          error $
            "CostModel parameters are not well-formed for "
              ++ show lang
              ++ ": "
              ++ show err
              ++ "\n"
              ++ show params
        Right costModel -> costModel

    costModelParamsForTesting :: HasCallStack => [(V3.ParamName, Int64)]
    costModelParamsForTesting =
      -- all geneses should contain only the number of cost model params equal to the initial number
      take (L.costModelInitParamCount PlutusV3)
        . Map.toList
        . fromJust
        $ extractCostModelParamsLedgerOrder mCostModel

    mCostModel :: MCostModel
    mCostModel =
      -- nothing to clear because v4 does not exist (yet).
      toMCostModel defaultCekCostModelForTesting & builtinCostModel %~ clearBuiltinCostModel'

    -- \*** FIXME!!! ***
    -- This is temporary to get the tests to pass
    clearBuiltinCostModel' :: m ~ MBuiltinCostModel => m -> m
    clearBuiltinCostModel' r =
      r
        { -- , paramIntegerToByteString = mempty -- Required for V2
          -- , paramByteStringToInteger = mempty -- Required for V2
          paramExpModInteger = mempty
        , paramDropList = mempty
        , paramLengthOfArray = mempty
        , paramListToArray = mempty
        , paramIndexArray = mempty
        }

    -- A helper function to lift to a "full" `MCostModel`, by mapping *all* of its fields to `Just`.
    -- The fields can be later on cleared, by assigning them to `Nothing`.
    toMCostModel
      :: CostModel CekMachineCosts BuiltinCostModel
      -> MCostModel
    toMCostModel cm =
      cm
        & machineCostModel
          %~ bmap (Just . runIdentity)
        & builtinCostModel
          %~ bmap (MCostingFun . Just)

    extractCostModelParamsLedgerOrder
      :: (IsParamName p, Ord p)
      => MCostModel
      -> Maybe (Map.Map p Int64)
    extractCostModelParamsLedgerOrder =
      extractInAlphaOrder
        >=> toLedgerOrder
     where
      extractInAlphaOrder = extractCostModelParams
      toLedgerOrder = mapKeysM readParamName

      mapKeysM :: (Monad m, Ord k2) => (k1 -> m k2) -> Map.Map k1 a -> m (Map.Map k2 a)
      mapKeysM = viaListM . mapM . firstM

      viaListM op = fmap Map.fromList . op . Map.toList
      firstM f (k, v) = (,v) <$> f k

type MCostModel = CostModel MCekMachineCosts MBuiltinCostModel

type MCekMachineCosts = CekMachineCostsBase Maybe

type MBuiltinCostModel = BuiltinCostModelBase MCostingFun

(%!) :: forall r. (HasCallStack, Typeable r, BoundedRational r) => Integer -> Integer -> r
n %! d = unsafeBoundedRational $ n Data.Ratio.% d

-- | Some reasonable starting defaults for constructing a 'AlonzoGenesis'.
-- Based on https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
alonzoGenesisDefaults
  :: AlonzoGenesis
alonzoGenesisDefaults =
  AlonzoGenesis
    { agPrices =
        Prices
          { prSteps = 721 %! 10000000
          , prMem = 577 %! 10000
          }
    , agMaxValSize = 5000
    , agMaxTxExUnits =
        ExUnits
          { exUnitsMem = 140000000
          , exUnitsSteps = 10000000000
          }
    , agMaxCollateralInputs = 3
    , agMaxBlockExUnits =
        ExUnits
          { exUnitsMem = 62000000
          , exUnitsSteps = 20000000000
          }
    , agCostModels = errorFail apiCostModels
    , agCollateralPercentage = 150
    , agCoinsPerUTxOWord = CoinPerWord $ Coin 34482
    }
 where
  apiCostModels =
    mkCostModelsLenient $
      fromList
        [ (fromIntegral $ fromEnum PlutusV1, defaultV1CostModel)
        , (fromIntegral $ fromEnum PlutusV2, defaultV2CostModel)
        ]
   where
    defaultV1CostModel =
      [ 205665
      , 812
      , 1
      , 1
      , 1000
      , 571
      , 0
      , 1
      , 1000
      , 24177
      , 4
      , 1
      , 1000
      , 32
      , 117366
      , 10475
      , 4
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 100
      , 100
      , 23000
      , 100
      , 19537
      , 32
      , 175354
      , 32
      , 46417
      , 4
      , 221973
      , 511
      , 0
      , 1
      , 89141
      , 32
      , 497525
      , 14068
      , 4
      , 2
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 1000
      , 28662
      , 4
      , 2
      , 245000
      , 216773
      , 62
      , 1
      , 1060367
      , 12586
      , 1
      , 208512
      , 421
      , 1
      , 187000
      , 1000
      , 52998
      , 1
      , 80436
      , 32
      , 43249
      , 32
      , 1000
      , 32
      , 80556
      , 1
      , 57667
      , 4
      , 1000
      , 10
      , 197145
      , 156
      , 1
      , 197145
      , 156
      , 1
      , 204924
      , 473
      , 1
      , 208896
      , 511
      , 1
      , 52467
      , 32
      , 64832
      , 32
      , 65493
      , 32
      , 22558
      , 32
      , 16563
      , 32
      , 76511
      , 32
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 69522
      , 11687
      , 0
      , 1
      , 60091
      , 32
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 806990
      , 30482
      , 4
      , 1927926
      , 82523
      , 4
      , 265318
      , 0
      , 4
      , 0
      , 85931
      , 32
      , 205665
      , 812
      , 1
      , 1
      , 41182
      , 32
      , 212342
      , 32
      , 31220
      , 32
      , 32696
      , 32
      , 43357
      , 32
      , 32247
      , 32
      , 38314
      , 32
      , 57996947
      , 18975
      , 10
      ]
    defaultV2CostModel =
      [ 205665
      , 812
      , 1
      , 1
      , 1000
      , 571
      , 0
      , 1
      , 1000
      , 24177
      , 4
      , 1
      , 1000
      , 32
      , 117366
      , 10475
      , 4
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 100
      , 100
      , 23000
      , 100
      , 19537
      , 32
      , 175354
      , 32
      , 46417
      , 4
      , 221973
      , 511
      , 0
      , 1
      , 89141
      , 32
      , 497525
      , 14068
      , 4
      , 2
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 1000
      , 28662
      , 4
      , 2
      , 245000
      , 216773
      , 62
      , 1
      , 1060367
      , 12586
      , 1
      , 208512
      , 421
      , 1
      , 187000
      , 1000
      , 52998
      , 1
      , 80436
      , 32
      , 43249
      , 32
      , 1000
      , 32
      , 80556
      , 1
      , 57667
      , 4
      , 1000
      , 10
      , 197145
      , 156
      , 1
      , 197145
      , 156
      , 1
      , 204924
      , 473
      , 1
      , 208896
      , 511
      , 1
      , 52467
      , 32
      , 64832
      , 32
      , 65493
      , 32
      , 22558
      , 32
      , 16563
      , 32
      , 76511
      , 32
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 69522
      , 11687
      , 0
      , 1
      , 60091
      , 32
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 1159724
      , 392670
      , 0
      , 2
      , 806990
      , 30482
      , 4
      , 1927926
      , 82523
      , 4
      , 265318
      , 0
      , 4
      , 0
      , 85931
      , 32
      , 205665
      , 812
      , 1
      , 1
      , 41182
      , 32
      , 212342
      , 32
      , 31220
      , 32
      , 32696
      , 32
      , 43357
      , 32
      , 32247
      , 32
      , 38314
      , 32
      , 35892428
      , 10
      , 9462713
      , 1021
      , 10
      , 38887044
      , 32947
      , 10
      ]

-- | Convert Rational to a bounded rational. Throw an exception when the rational is out of bounds.
unsafeBoundedRational
  :: forall r
   . (HasCallStack, Typeable r, BoundedRational r)
  => Rational
  -> r
unsafeBoundedRational x = fromMaybe (error errMessage) $ boundRational x
 where
  errMessage = show (typeRep (Proxy @r)) <> " is out of bounds: " <> show x
