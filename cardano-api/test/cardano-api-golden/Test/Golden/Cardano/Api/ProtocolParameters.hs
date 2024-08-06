{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TODO remove me when ProtocolParameters is deleted
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Golden.Cardano.Api.ProtocolParameters
  ( test_golden_ProtocolParameters
  , test_golden_ProtocolParameters_to_PParams
  )
where

import           Cardano.Api (AnyPlutusScriptVersion (AnyPlutusScriptVersion), CostModel (..),
                   ExecutionUnits (..), PlutusScriptVersion (..), makePraosNonce)
import           Cardano.Api.Ledger (Coin (..), EpochInterval (EpochInterval), StandardCrypto)
import           Cardano.Api.ProtocolParameters (ExecutionUnitPrices (..), ProtocolParameters (..))

import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.PParams (AlonzoPParams (..))
import           Cardano.Ledger.Babbage (BabbageEra)
import           Cardano.Ledger.Babbage.PParams (BabbagePParams (..))
import           Cardano.Ledger.Plutus.CostModels (costModelParamsCount)
import           Cardano.Ledger.Plutus.Language (Language (..))
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))

import           Data.Aeson (FromJSON, eitherDecode, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Map (Map)
import           Data.Proxy (Proxy (..))
import           GHC.Exts (IsList (..))

import           Hedgehog (Property, property, success)
import qualified Hedgehog.Extras.Aeson as H
import           Hedgehog.Internal.Property (failWith)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

test_golden_ProtocolParameters :: TestTree
test_golden_ProtocolParameters = testProperty "golden ProtocolParameters" $ do
  H.goldenTestJsonValuePretty
    legacyCardanoApiProtocolParameters
    "test/cardano-api-golden/files/golden/LegacyProtocolParameters.json"

test_golden_ProtocolParameters_to_PParams :: TestTree
test_golden_ProtocolParameters_to_PParams =
  testGroup
    "golden ProtocolParameter tests"
    [ testProperty "ShelleyPParams" $
        goldenLegacyProtocolParametersToPParams
          (Proxy :: Proxy (ShelleyPParams Identity (ShelleyEra StandardCrypto)))
    , testProperty "AlonzoPParams" $
        goldenLegacyProtocolParametersToPParams
          (Proxy :: Proxy (AlonzoPParams Identity (AlonzoEra StandardCrypto)))
    , testProperty "BabbagePParams" $
        goldenLegacyProtocolParametersToPParams
          (Proxy :: Proxy (BabbagePParams Identity (BabbageEra StandardCrypto)))
    ]

-- Test that tries decoding the legacy protocol parameters golden file
-- 'legacyCardanoApiProtocolParameters' as the type provided as a 'Proxy'.
goldenLegacyProtocolParametersToPParams :: forall pp. FromJSON pp => Proxy pp -> Property
goldenLegacyProtocolParametersToPParams proxy =
  property $ case decodedLegacyCardanoApiProtocolParameters of
    Left err ->
      failWith
        Nothing
        ( "goldenLegacyProtocolParametersToPParams could not decode golden file as "
            <> show proxy
            <> ": "
            <> show err
        )
    Right _ -> success
 where
  bytestringLegacyCardanoApiProtocolParameters :: ByteString
  bytestringLegacyCardanoApiProtocolParameters = encode legacyCardanoApiProtocolParameters

  decodedLegacyCardanoApiProtocolParameters :: Either String pp
  decodedLegacyCardanoApiProtocolParameters = eitherDecode bytestringLegacyCardanoApiProtocolParameters

legacyCardanoApiProtocolParameters :: ProtocolParameters
legacyCardanoApiProtocolParameters =
  ProtocolParameters
    { protocolParamUTxOCostPerByte = Just $ Coin 1_000_000
    , protocolParamTxFeePerByte = Coin 2_000_000
    , protocolParamTxFeeFixed = Coin 1_500_000
    , protocolParamTreasuryCut = 0.1
    , protocolParamStakePoolTargetNum = 100
    , protocolParamStakePoolDeposit = Coin 1_000_000_000
    , protocolParamStakeAddressDeposit = Coin 10_000_000
    , protocolParamProtocolVersion = (2, 3)
    , protocolParamPrices = Just executionUnitPrices
    , protocolParamPoolRetireMaxEpoch = Cardano.Api.Ledger.EpochInterval 4
    , protocolParamPoolPledgeInfluence = 0.54
    , protocolParamMonetaryExpansion = 0.23
    , protocolParamMinUTxOValue = Just $ Coin 3_000_000
    , protocolParamMinPoolCost = Coin 3_500_000
    , protocolParamMaxValueSize = Just 10
    , protocolParamMaxTxSize = 3_000
    , protocolParamMaxTxExUnits = Just executionUnits
    , protocolParamMaxCollateralInputs = Just 10
    , protocolParamMaxBlockHeaderSize = 1_200
    , protocolParamMaxBlockExUnits = Just executionUnits2
    , protocolParamMaxBlockBodySize = 5_000
    , protocolParamExtraPraosEntropy = Just $ makePraosNonce "entropyEntropy"
    , protocolParamDecentralization = Just 0.52
    , protocolParamCostModels = costModels
    , protocolParamCollateralPercent = Just 23
    }
 where
  executionUnitPrices :: ExecutionUnitPrices
  executionUnitPrices =
    ExecutionUnitPrices
      { priceExecutionSteps = 0.3
      , priceExecutionMemory = 0.2
      }

  costModels :: Map AnyPlutusScriptVersion CostModel
  costModels =
    fromList
      [ (AnyPlutusScriptVersion PlutusScriptV3, CostModel [1 .. numParams PlutusV3])
      , (AnyPlutusScriptVersion PlutusScriptV2, CostModel [1 .. numParams PlutusV2])
      , (AnyPlutusScriptVersion PlutusScriptV1, CostModel [1 .. numParams PlutusV1])
      ]

  numParams :: Language -> Int64
  numParams = fromIntegral . costModelParamsCount

  executionUnits :: ExecutionUnits
  executionUnits =
    ExecutionUnits
      { executionSteps = 4_300
      , executionMemory = 2_300
      }

  executionUnits2 :: ExecutionUnits
  executionUnits2 =
    ExecutionUnits
      { executionSteps = 5_600
      , executionMemory = 3_400
      }
