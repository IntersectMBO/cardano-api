module Test.Gen.Cardano.Api.ProtocolParameters where

import Cardano.Api
import Cardano.Api.Ledger

import Test.Gen.Cardano.Api.Typed (genCostModels)

import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as Q

genStrictMaybe :: MonadGen m => m a -> m (StrictMaybe a)
genStrictMaybe gen =
  Gen.sized $ \n ->
    Gen.frequency
      [ (2, pure SNothing)
      , (1 + fromIntegral n, SJust <$> gen)
      ]

genCommonProtocolParametersUpdate :: MonadGen m => m CommonProtocolParametersUpdate
genCommonProtocolParametersUpdate =
  CommonProtocolParametersUpdate
    <$> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary

genDeprecatedAfterMaryPParams :: MonadGen m => m (DeprecatedAfterMaryPParams era)
genDeprecatedAfterMaryPParams = DeprecatedAfterMaryPParams <$> genStrictMaybe Q.arbitrary

genDeprecatedAfterBabbagePParams :: MonadGen m => m (DeprecatedAfterBabbagePParams era)
genDeprecatedAfterBabbagePParams = DeprecatedAfterBabbagePParams <$> genStrictMaybe Q.arbitrary

genShelleyToAlonzoPParams :: MonadGen m => m (ShelleyToAlonzoPParams era)
genShelleyToAlonzoPParams =
  ShelleyToAlonzoPParams
    <$> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary

genAlonzoOnwardsPParams :: MonadGen m => m (AlonzoOnwardsPParams era)
genAlonzoOnwardsPParams =
  AlonzoOnwardsPParams
    <$> genStrictMaybe genCostModels
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary

genIntroducedInBabbagePParams :: MonadGen m => m (IntroducedInBabbagePParams era)
genIntroducedInBabbagePParams = IntroducedInBabbagePParams <$> genStrictMaybe Q.arbitrary

genIntroducedInConwayPParams :: MonadGen m => m (IntroducedInConwayPParams era)
genIntroducedInConwayPParams =
  IntroducedInConwayPParams
    <$> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary

genShelleyEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate ShelleyEra)
genShelleyEraBasedProtocolParametersUpdate =
  ShelleyEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genDeprecatedAfterMaryPParams
    <*> genDeprecatedAfterBabbagePParams
    <*> genShelleyToAlonzoPParams

genAllegraEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate AllegraEra)
genAllegraEraBasedProtocolParametersUpdate =
  AllegraEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genDeprecatedAfterMaryPParams
    <*> genShelleyToAlonzoPParams
    <*> genDeprecatedAfterBabbagePParams

genMaryEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate MaryEra)
genMaryEraBasedProtocolParametersUpdate =
  MaryEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genDeprecatedAfterMaryPParams
    <*> genShelleyToAlonzoPParams
    <*> genDeprecatedAfterBabbagePParams

genAlonzoEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate AlonzoEra)
genAlonzoEraBasedProtocolParametersUpdate =
  AlonzoEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genShelleyToAlonzoPParams
    <*> genAlonzoOnwardsPParams
    <*> genDeprecatedAfterBabbagePParams

genBabbageEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate BabbageEra)
genBabbageEraBasedProtocolParametersUpdate =
  BabbageEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genAlonzoOnwardsPParams
    <*> genDeprecatedAfterBabbagePParams
    <*> genIntroducedInBabbagePParams

genConwayEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate ConwayEra)
genConwayEraBasedProtocolParametersUpdate =
  ConwayEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genAlonzoOnwardsPParams
    <*> genIntroducedInBabbagePParams
    <*> genIntroducedInConwayPParams
