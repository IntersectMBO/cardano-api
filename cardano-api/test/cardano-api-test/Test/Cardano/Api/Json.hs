{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Api.Json
  ( tests
  ) where

import           Cardano.Api.Orphans ()
import           Cardano.Api.Shelley

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), eitherDecode, encode)
import           Data.Aeson.Types (Parser, parseEither)

import           Test.Gen.Cardano.Api (genAlonzoGenesis)
import           Test.Gen.Cardano.Api.Typed

import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

prop_json_roundtrip_alonzo_genesis :: Property
prop_json_roundtrip_alonzo_genesis = H.property $ do
  genesis <- forAll genAlonzoGenesis
  tripping genesis encode eitherDecode

prop_json_roundtrip_utxo :: Property
prop_json_roundtrip_utxo = H.property $ do
  utxo <- forAll $ genUTxO BabbageEra
  tripping utxo encode eitherDecode

prop_json_roundtrip_reference_scripts :: Property
prop_json_roundtrip_reference_scripts = H.property $ do
  rScript <- forAll $ genReferenceScript BabbageEra
  tripping rScript encode eitherDecode

prop_json_roundtrip_txoutvalue :: Property
prop_json_roundtrip_txoutvalue = H.property $ do
  oVal <- forAll $ genTxOutValue BabbageEra
  tripping oVal encode eitherDecode

prop_json_roundtrip_txout_tx_context :: Property
prop_json_roundtrip_txout_tx_context = H.property $ do
  txOut <- forAll $ genTxOutTxContext BabbageEra
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_utxo_context :: Property
prop_json_roundtrip_txout_utxo_context = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext BabbageEra
  tripping txOut encode eitherDecode

prop_json_roundtrip_eraInMode :: Property
prop_json_roundtrip_eraInMode = H.property $ do
  H.assert $ parseEither rountripEraInModeParser ByronEraInByronMode == Right ByronEraInByronMode
  H.assert $ parseEither rountripEraInModeParser ShelleyEraInShelleyMode == Right ShelleyEraInShelleyMode
  H.assert $ parseEither rountripEraInModeParser ByronEraInCardanoMode == Right ByronEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser ShelleyEraInCardanoMode == Right ShelleyEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser AllegraEraInCardanoMode == Right AllegraEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser MaryEraInCardanoMode == Right MaryEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser AlonzoEraInCardanoMode == Right AlonzoEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser BabbageEraInCardanoMode == Right BabbageEraInCardanoMode
  H.assert $ parseEither rountripEraInModeParser ConwayEraInCardanoMode == Right ConwayEraInCardanoMode

  where
    -- Defined this way instead of using 'tripping' in order to warn the
    -- developer if there's ever a new constructor in 'EraInMode' and we would
    -- need to add a new 'FromJSON' instance.
    rountripEraInModeParser :: EraInMode era mode -> Parser (EraInMode era mode)
    rountripEraInModeParser = \case
      ByronEraInByronMode -> parseJSON $ toJSON ByronEraInByronMode
      ShelleyEraInShelleyMode -> parseJSON $ toJSON ShelleyEraInShelleyMode
      ByronEraInCardanoMode -> parseJSON $ toJSON ByronEraInCardanoMode
      ShelleyEraInCardanoMode -> parseJSON $ toJSON ShelleyEraInCardanoMode
      AllegraEraInCardanoMode -> parseJSON $ toJSON AllegraEraInCardanoMode
      MaryEraInCardanoMode -> parseJSON $ toJSON MaryEraInCardanoMode
      AlonzoEraInCardanoMode -> parseJSON $ toJSON AlonzoEraInCardanoMode
      BabbageEraInCardanoMode -> parseJSON $ toJSON BabbageEraInCardanoMode
      ConwayEraInCardanoMode -> parseJSON $ toJSON ConwayEraInCardanoMode

prop_json_roundtrip_scriptdata_detailed_json :: Property
prop_json_roundtrip_scriptdata_detailed_json = H.property $ do
  sData <- forAll genHashableScriptData
  tripping sData scriptDataToJsonDetailedSchema scriptDataFromJsonDetailedSchema

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Json"
  [ testProperty "json roundtrip alonzo genesis"           prop_json_roundtrip_alonzo_genesis
  , testProperty "json roundtrip utxo"                     prop_json_roundtrip_utxo
  , testProperty "json roundtrip reference scripts"        prop_json_roundtrip_reference_scripts
  , testProperty "json roundtrip txoutvalue"               prop_json_roundtrip_txoutvalue
  , testProperty "json roundtrip txout tx context"         prop_json_roundtrip_txout_tx_context
  , testProperty "json roundtrip txout utxo context"       prop_json_roundtrip_txout_utxo_context
  , testProperty "json roundtrip eraInMode"                prop_json_roundtrip_eraInMode
  , testProperty "json roundtrip scriptdata detailed json" prop_json_roundtrip_scriptdata_detailed_json
  ]
