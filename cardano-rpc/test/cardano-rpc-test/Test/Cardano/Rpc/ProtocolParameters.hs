{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.ProtocolParameters where

import Cardano.Api.Experimental.Era
import Cardano.Api.ProtocolParameters
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO

import Data.Bits
import Data.Map.Strict qualified as M
import Data.Ratio
import GHC.IsList

import Test.Gen.Cardano.Api.Typed (genValidProtocolParameters)

import Hedgehog
import Hedgehog qualified as H

-- | Test if protocol parameters roundtrip between ledger and proto representation
hprop_roundtrip_protocol_parameters :: Property
hprop_roundtrip_protocol_parameters = H.property $ do
  let era = ConwayEra

  pp <- fmap unLedgerProtocolParameters . H.forAll $ genValidProtocolParameters (convert era)
  let costModels = L.costModelsValid $ pp ^. L.ppCostModelsL
      mCms = map (`M.lookup` costModels) [minBound .. maxBound]
      nonEmptyCostModels =
        fromList . flip mapMaybe mCms $ \mCm ->
          mCm >>= \cm ->
            if not (null $ L.getCostModelParams cm)
              then Just (L.getCostModelLanguage cm, cm)
              else Nothing
      -- The DTOs from proto files have types smaller than the ledger type. Common example is using
      -- Int64 instead of unbounded Integer. This makes conversion from ledger protocol parameters
      -- to proto types partial. We clip the values coming from ledger's arbitrary instance of
      -- Protocol Parameters in order to avoid issues in the serialisation round trip.
      pp' =
        obtainCommonConstraints era $
          pp
            & L.ppCoinsPerUTxOByteL %~ L.CoinPerByte . clipI 64 . L.unCoinPerByte
            & L.ppMinFeeBL %~ clipI 64
            & L.ppMinFeeAL %~ clipI 64
            & L.ppA0L %~ clipIBr
            & L.ppRhoL %~ clipIBr
            & L.ppTauL %~ clipIBr
            & L.ppMinFeeRefScriptCostPerByteL %~ clipIBr
            & L.ppKeyDepositL %~ clipI 64
            & L.ppPoolDepositL %~ clipI 64
            & L.ppCostModelsL .~ L.mkCostModels nonEmptyCostModels
            & L.ppPoolVotingThresholdsL . L.pvtMotionNoConfidenceL %~ clipIBr
            & L.ppPoolVotingThresholdsL . L.pvtCommitteeNormalL %~ clipIBr
            & L.ppPoolVotingThresholdsL . L.pvtCommitteeNoConfidenceL %~ clipIBr
            & L.ppPoolVotingThresholdsL . L.pvtHardForkInitiationL %~ clipIBr
            & L.ppPoolVotingThresholdsL . L.pvtPPSecurityGroupL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtMotionNoConfidenceL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtCommitteeNormalL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtCommitteeNoConfidenceL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtUpdateToConstitutionL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtHardForkInitiationL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtPPNetworkGroupL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtPPEconomicGroupL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtPPTechnicalGroupL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtPPGovGroupL %~ clipIBr
            & L.ppDRepVotingThresholdsL . L.dvtTreasuryWithdrawalL %~ clipIBr
            & L.ppPricesL . prStepsL %~ clipIBr
            & L.ppPricesL . prMemL %~ clipIBr

  H.tripping
    pp'
    (protocolParamsToUtxoRpcPParams era)
    (utxoRpcPParamsToProtocolParams era)

-- | Clip the bounded rational to a signed value not larger than 32 bits.
clipIBr :: (Show a, L.BoundedRational a) => a -> a
clipIBr input = do
  let r = L.unboundRational input
      nBits = [32, 16, 8, 4] -- nominator bit sizes to try
      dBits = nBits -- denominator bit sizes to try
      clippedNums = (`clipI` numerator r) <$> nBits
      clippedDens = (`clipI` denominator r) <$> dBits
  fromMaybe (error $ "impossible! clipped value is out of bounds. Original value: " <> show input) $
    -- try to clip value to the smaller values until one succeeds
    asum @[]
      [ L.boundRational $ numerator' % denominator'
      | numerator' <- clippedNums
      , denominator' <- clippedDens
      ]

-- | Clip the Integral to a value with n bit size.
clipI :: (Integral a, Bits a) => Int -> a -> a
clipI n v
  | v > 2 ^ (n - 1) - 1 || v < (-(2 ^ (n - 1))) = clipI n $ shiftR v 1
  | otherwise = fromIntegral v

pvMajorL :: Lens' L.ProtVer L.Version
pvMajorL = lens L.pvMajor $ \p v -> p{L.pvMajor = v}

pvMinorL :: Lens' L.ProtVer Natural
pvMinorL = lens L.pvMinor $ \p v -> p{L.pvMinor = v}

prStepsL :: Lens' L.Prices L.NonNegativeInterval
prStepsL = lens L.prSteps $ \p v -> p{L.prSteps = v}

prMemL :: Lens' L.Prices L.NonNegativeInterval
prMemL = lens L.prMem $ \p v -> p{L.prMem = v}
