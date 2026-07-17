{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Gen.Cardano.Api.ProtocolParameters where

import Cardano.Api
import Cardano.Api.Ledger

import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary (genEraProtVer)

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

genDeprecatedAfterBabbagePParams
  :: forall era ledgerera m
   . MonadGen m
  => ShelleyBasedEra era
  -> m (DeprecatedAfterBabbagePParams ledgerera)
genDeprecatedAfterBabbagePParams sbe =
  shelleyBasedEraConstraints sbe $
    DeprecatedAfterBabbagePParams
      <$> genStrictMaybe (Q.quickcheck (genEraProtVer @(ShelleyLedgerEra era)))

genShelleyToAlonzoPParams :: MonadGen m => m (ShelleyToAlonzoPParams era)
genShelleyToAlonzoPParams =
  ShelleyToAlonzoPParams
    <$> genStrictMaybe Q.arbitrary
    <*> genStrictMaybe Q.arbitrary

genAlonzoOnwardsPParams :: MonadGen m => m (AlonzoOnwardsPParams era)
genAlonzoOnwardsPParams =
  -- Cost models don't roundtrip through CBOR, hence SNothing
  AlonzoOnwardsPParams SNothing
    <$> genStrictMaybe Q.arbitrary
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

genEraBasedProtocolParametersUpdate
  :: MonadGen m
  => CardanoEra era
  -> m (EraBasedProtocolParametersUpdate era)
genEraBasedProtocolParametersUpdate era =
  case era of
    ByronEra ->
      error
        "genEraBasedProtocolParametersUpdate: ByronEra does not support \
        \protocol parameter updates"
    ShelleyEra -> genShelleyEraBasedProtocolParametersUpdate
    AllegraEra -> genAllegraEraBasedProtocolParametersUpdate
    MaryEra -> genMaryEraBasedProtocolParametersUpdate
    AlonzoEra -> genAlonzoEraBasedProtocolParametersUpdate
    BabbageEra -> genBabbageEraBasedProtocolParametersUpdate
    ConwayEra -> genConwayEraBasedProtocolParametersUpdate
    DijkstraEra -> error "TODO Dijkstra: genEraBasedProtocolParametersUpdate: era not supported"

genShelleyEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate ShelleyEra)
genShelleyEraBasedProtocolParametersUpdate =
  ShelleyEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genDeprecatedAfterMaryPParams
    <*> genDeprecatedAfterBabbagePParams ShelleyBasedEraShelley
    <*> genShelleyToAlonzoPParams

genAllegraEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate AllegraEra)
genAllegraEraBasedProtocolParametersUpdate =
  AllegraEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genDeprecatedAfterMaryPParams
    <*> genShelleyToAlonzoPParams
    <*> genDeprecatedAfterBabbagePParams ShelleyBasedEraAllegra

genMaryEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate MaryEra)
genMaryEraBasedProtocolParametersUpdate =
  MaryEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genDeprecatedAfterMaryPParams
    <*> genShelleyToAlonzoPParams
    <*> genDeprecatedAfterBabbagePParams ShelleyBasedEraMary

genAlonzoEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate AlonzoEra)
genAlonzoEraBasedProtocolParametersUpdate =
  AlonzoEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genShelleyToAlonzoPParams
    <*> genAlonzoOnwardsPParams
    <*> genDeprecatedAfterBabbagePParams ShelleyBasedEraAlonzo

genBabbageEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate BabbageEra)
genBabbageEraBasedProtocolParametersUpdate =
  BabbageEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genAlonzoOnwardsPParams
    <*> genDeprecatedAfterBabbagePParams ShelleyBasedEraBabbage
    <*> genIntroducedInBabbagePParams

genConwayEraBasedProtocolParametersUpdate
  :: MonadGen m => m (EraBasedProtocolParametersUpdate ConwayEra)
genConwayEraBasedProtocolParametersUpdate =
  ConwayEraBasedProtocolParametersUpdate
    <$> genCommonProtocolParametersUpdate
    <*> genAlonzoOnwardsPParams
    <*> genIntroducedInBabbagePParams
    <*> genIntroducedInConwayPParams
