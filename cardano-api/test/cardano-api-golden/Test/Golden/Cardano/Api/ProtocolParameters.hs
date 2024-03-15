{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Cardano.Api.ProtocolParameters
  ( test_golden_ProtocolParameters
  , test_golden_ProtocolParameters_to_PParams
  ) where

import           Cardano.Api (AnyPlutusScriptVersion (AnyPlutusScriptVersion), CostModel (..),
                   ExecutionUnits (..), Lovelace (..), PlutusScriptVersion (..), makePraosNonce)
import           Cardano.Api.Ledger (EpochInterval (EpochInterval), StandardCrypto)
import           Cardano.Api.ProtocolParameters (ExecutionUnitPrices (..), ProtocolParameters (..))

import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.PParams (AlonzoPParams (..))
import           Cardano.Ledger.Babbage (BabbageEra)
import           Cardano.Ledger.Babbage.PParams (BabbagePParams (..))
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))

import           Data.Aeson (FromJSON, eitherDecode, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity (Identity)
import           Data.Map (Map)
import qualified Data.Map as M

import           Hedgehog (Property, property, success)
import qualified Hedgehog.Extras.Aeson as H
import           Hedgehog.Internal.Property (failWith)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

test_golden_ProtocolParameters :: TestTree
test_golden_ProtocolParameters = testProperty "golden ProtocolParameters" $ do
  H.goldenTestJsonValuePretty legacyCardanoApiProtocolParameters "test/cardano-api-golden/files/golden/ProtocolParameters"

test_golden_ProtocolParameters_to_PParams :: TestTree
test_golden_ProtocolParameters_to_PParams =
  testGroup "golden ProtocolParameter tests"
            [ testProperty "ShelleyPParams" $
                goldenProtocolParametersToPParams (undefined :: (ShelleyPParams Identity (ShelleyEra StandardCrypto)))
            , testProperty "AlonzoPParams" $
                goldenProtocolParametersToPParams (undefined :: (AlonzoPParams Identity (AlonzoEra StandardCrypto)))
            , testProperty "BabbagePParams" $
                goldenProtocolParametersToPParams (undefined :: (BabbagePParams Identity (BabbageEra StandardCrypto)))
            ]

goldenProtocolParametersToPParams :: forall pp. FromJSON pp => pp -> Property
goldenProtocolParametersToPParams _ =
  property $ case decodedLegacyCardanoApiProtocolParameters of
               Left err -> failWith Nothing ("could not decode: " <> show err)
               Right _ -> success
  where
    bytestringLegacyCardanoApiProtocolParameters :: ByteString
    bytestringLegacyCardanoApiProtocolParameters = encode legacyCardanoApiProtocolParameters

    decodedLegacyCardanoApiProtocolParameters :: Either String pp
    decodedLegacyCardanoApiProtocolParameters = eitherDecode bytestringLegacyCardanoApiProtocolParameters

legacyCardanoApiProtocolParameters :: ProtocolParameters
legacyCardanoApiProtocolParameters = ProtocolParameters { protocolParamUTxOCostPerByte = Just $ Lovelace 1000000
                                                        , protocolParamTxFeePerByte = Lovelace 2000000
                                                        , protocolParamTxFeeFixed = Lovelace 1500000
                                                        , protocolParamTreasuryCut = 0.1
                                                        , protocolParamStakePoolTargetNum = 100
                                                        , protocolParamStakePoolDeposit = Lovelace 1000000000
                                                        , protocolParamStakeAddressDeposit = Lovelace 10000000
                                                        , protocolParamProtocolVersion = (2, 3)
                                                        , protocolParamPrices = Just executionUnitPrices
                                                        , protocolParamPoolRetireMaxEpoch = Cardano.Api.Ledger.EpochInterval 4
                                                        , protocolParamPoolPledgeInfluence = 0.54
                                                        , protocolParamMonetaryExpansion = 0.23
                                                        , protocolParamMinUTxOValue = Just $ Lovelace 3000000
                                                        , protocolParamMinPoolCost = Lovelace 3500000
                                                        , protocolParamMaxValueSize = Just 10
                                                        , protocolParamMaxTxSize = 3000
                                                        , protocolParamMaxTxExUnits = Just executionUnits
                                                        , protocolParamMaxCollateralInputs = Just 10
                                                        , protocolParamMaxBlockHeaderSize = 1200
                                                        , protocolParamMaxBlockExUnits = Just executionUnits2
                                                        , protocolParamMaxBlockBodySize = 5000
                                                        , protocolParamExtraPraosEntropy = Just $ makePraosNonce "entropyEntropy"
                                                        , protocolParamDecentralization = Just 0.52
                                                        , protocolParamCostModels = costModels
                                                        , protocolParamCollateralPercent = Just 23
                                                        }
    where
    executionUnitPrices :: ExecutionUnitPrices
    executionUnitPrices = ExecutionUnitPrices { priceExecutionSteps = 0.3
                                              , priceExecutionMemory = 0.2
                                              }

    costModels :: Map AnyPlutusScriptVersion CostModel
    costModels = M.fromList [ (AnyPlutusScriptVersion PlutusScriptV3, CostModel [223,222..1])
                            , (AnyPlutusScriptVersion PlutusScriptV2, CostModel [1..175])
                            ]

    executionUnits :: ExecutionUnits
    executionUnits = ExecutionUnits { executionSteps = 4300
                                    , executionMemory = 2300
                                    }

    executionUnits2 :: ExecutionUnits
    executionUnits2 = ExecutionUnits { executionSteps = 5600
                                     , executionMemory = 3400
                                     }
