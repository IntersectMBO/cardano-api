{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Genesis.Internal
  ( ShelleyGenesis (..)
  , shelleyGenesisDefaults
  , alonzoGenesisDefaults
  , decodeAlonzoGenesis
  , conwayGenesisDefaults

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

import Cardano.Api.Era.Internal.Core
  ( CardanoEra
  , forEraMaybeEon
  , monoidForEraInEon
  )
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.IO
import Cardano.Api.Monad.Error
  ( ExceptT
  , MonadError (throwError)
  , MonadTransError
  , liftEither
  , modifyError
  )

import Cardano.Chain.Genesis qualified
import Cardano.Crypto.Hash.Blake2b qualified
import Cardano.Crypto.Hash.Class qualified
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Prices (..))
import Cardano.Ledger.Api (CoinPerWord (..))
import Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.PParams
  ( DRepVotingThresholds (..)
  , PoolVotingThresholds (..)
  , UpgradeConwayPParams (..)
  )
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
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3

import Control.Monad
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Default.Class qualified as DefaultClass
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (sortOn)
import Data.ListMap qualified as ListMap
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ratio
import Data.Set qualified as S
import Data.Text (Text)
import Data.Time qualified as Time
import Data.Typeable
import Data.Vector qualified as V
import GHC.Exts (IsList (..))
import GHC.Stack (HasCallStack)
import Lens.Micro
import Lens.Micro.Aeson qualified as AL

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
          & ppKeyDepositL .~ 400000 -- require a non-zero deposit when registering keys
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
      Map.toList $
        fromJust $
          extractCostModelParamsLedgerOrder mCostModel

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

-- | Decode Alonzo genesis in an optionally era sensitive way.
--
-- Because the Plutus V2 cost model has changed between Babbage and Conway era, we need to know the era if we
-- want to decde Alonzo Genesis with a cost model baked in. If the V2 cost model is present in genesis, you
-- need to provide an era witness.
--
-- When an era witness is provided, for Plutus V2 model the function additionally:
-- 1. Does extra cost model parameters name validation: Checks for mandatory 175 parameters if provided in
--    a map form.
-- 2. If >= Conway: adds defaults for new 10 parameters, if they were not provided (maxBound)
-- 3. Removes extra parameters above the max count: Babbage - 175, Conway - 185.
decodeAlonzoGenesis
  :: forall era t m
   . MonadTransError String t m
  => Maybe (CardanoEra era)
  -- ^ An optional era witness in which we're reading the genesis
  -> LBS.ByteString
  -- ^ Genesis JSON
  -> t m AlonzoGenesis
decodeAlonzoGenesis Nothing genesisBs =
  modifyError ("Cannot decode Alonzo genesis: " <>) $
    liftEither $
      A.eitherDecode genesisBs
decodeAlonzoGenesis (Just era) genesisBs = modifyError ("Cannot decode era-sensitive Alonzo genesis: " <>) $ do
  genesisValue :: A.Value <- liftEither $ A.eitherDecode genesisBs
  -- Making a fixup of a costmodel is easier before JSON deserialization. This also saves us from building
  -- plutus' EvaluationContext one more time after cost model update.
  genesisValue' <-
    (AL.key "costModels" . AL.key "PlutusV2" . AL._Value) setCostModelDefaultValues genesisValue
  fromJsonE genesisValue'
 where
  setCostModelDefaultValues :: A.Value -> ExceptT String m A.Value
  setCostModelDefaultValues = \case
    obj@(A.Object _) -> do
      -- decode cost model into a map first
      costModel :: Map V2.ParamName Int64 <-
        modifyError ("Decoding cost model object: " <>) $ fromJsonE obj

      let costModelWithDefaults =
            sortOn fst
              . toList
              $ M.union costModel optionalCostModelDefaultValues

      -- check that we have all required params
      unless (allCostModelParams == (fst <$> costModelWithDefaults)) $ do
        let allCostModelParamsSet = fromList allCostModelParams
            providedCostModelParamsSet = fromList $ fst <$> costModelWithDefaults
        throwError $
          "Missing V2 Plutus cost model parameters: "
            <> show (toList $ S.difference allCostModelParamsSet providedCostModelParamsSet)

      -- We have already have required params, we already added optional ones (which are trimmed later
      -- if required). Continue processing further in array representation.
      setCostModelDefaultValues . A.toJSON $ map snd costModelWithDefaults
    A.Array vec
      -- here we rely on an assumption that params are in correct order, so that we can take only the
      -- required ones for an era
      | V.length vec < costModelExpectedCount ->
          pure . A.Array . V.take costModelExpectedCount $
            vec <> (A.toJSON . snd <$> optionalCostModelDefaultValues)
      | V.length vec > costModelExpectedCount -> pure . A.Array $ V.take costModelExpectedCount vec
    other -> pure other

  -- Plutus V2 params expected count depending on an era
  costModelExpectedCount :: Int
  costModelExpectedCount
    -- use all available parameters >= conway
    | isConwayOnwards = length allCostModelParams
    -- use only required params in < conway
    | otherwise = L.costModelParamsCount L.PlutusV2 -- Babbage

  -- A list-like of tuples (param name, value) with default maxBound value
  optionalCostModelDefaultValues :: (Item l ~ (V2.ParamName, Int64), IsList l) => l
  optionalCostModelDefaultValues = fromList $ map (,maxBound) optionalV2costModelParams

  allCostModelParams :: [V2.ParamName]
  allCostModelParams = [minBound .. maxBound]

  -- The new V2 cost model params introduced in Conway
  optionalV2costModelParams :: [V2.ParamName]
  optionalV2costModelParams =
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

  isConwayOnwards = isJust $ forEraMaybeEon @ConwayEraOnwards era

-- | Some reasonable starting defaults for constructing a 'AlonzoGenesis'.
-- Based on https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
-- The era determines Plutus V2 cost model parameters:
-- * Conway: 185
-- * <= Babbage: 175
alonzoGenesisDefaults
  :: CardanoEra era
  -> AlonzoGenesis
alonzoGenesisDefaults era =
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
        <> defaultV2CostModelNewConwayParams

    -- New Conway cost model parameters
    defaultV2CostModelNewConwayParams =
      monoidForEraInEon @ConwayEraOnwards era $
        const
          [ 1292075
          , 24469
          , 74
          , 0
          , 1
          , 936157
          , 49601
          , 237
          , 0
          , 1
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
