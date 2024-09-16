{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Api.Json
  ( tests
  )
where

import           Cardano.Api
import           Cardano.Api.Orphans ()
import           Cardano.Api.Shelley

import           Data.Aeson (eitherDecode, encode)

import           Test.Gen.Cardano.Api (genAlonzoGenesis)
import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

prop_json_roundtrip_alonzo_genesis :: Property
prop_json_roundtrip_alonzo_genesis = H.property $ do
  genesis <- forAll genAlonzoGenesis
  tripping genesis encode eitherDecode

prop_json_roundtrip_utxo :: Property
prop_json_roundtrip_utxo = H.property $ do
  utxo <- forAll $ genUTxO ShelleyBasedEraBabbage
  tripping utxo encode eitherDecode

prop_json_roundtrip_reference_scripts :: Property
prop_json_roundtrip_reference_scripts = H.property $ do
  rScript <- forAll $ genReferenceScript ShelleyBasedEraBabbage
  tripping rScript encode eitherDecode

prop_json_roundtrip_txoutvalue :: Property
prop_json_roundtrip_txoutvalue = H.property $ do
  oVal <- forAll $ genTxOutValue ShelleyBasedEraBabbage
  tripping oVal encode eitherDecode

prop_json_roundtrip_txout_tx_context :: Property
prop_json_roundtrip_txout_tx_context = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraBabbage
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_utxo_context :: Property
prop_json_roundtrip_txout_utxo_context = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraBabbage
  tripping txOut encode eitherDecode

prop_json_roundtrip_scriptdata_detailed_json :: Property
prop_json_roundtrip_scriptdata_detailed_json = H.property $ do
  sData <- forAll genHashableScriptData
  tripping sData scriptDataToJsonDetailedSchema scriptDataFromJsonDetailedSchema

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

prop_roundtrip_protocol_parameters_JSON :: Property
prop_roundtrip_protocol_parameters_JSON = H.property $ do
  AnyCardanoEra era <- forAll $ Gen.element [minBound .. maxBound]
  pp <- forAll (genProtocolParameters era)
  tripping pp encode eitherDecode

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Json"
    [ testProperty "json roundtrip alonzo genesis" prop_json_roundtrip_alonzo_genesis
    , testProperty "json roundtrip utxo" prop_json_roundtrip_utxo
    , testProperty "json roundtrip reference scripts" prop_json_roundtrip_reference_scripts
    , testProperty "json roundtrip txoutvalue" prop_json_roundtrip_txoutvalue
    , testProperty "json roundtrip txout tx context" prop_json_roundtrip_txout_tx_context
    , testProperty "json roundtrip txout utxo context" prop_json_roundtrip_txout_utxo_context
    , testProperty "json roundtrip scriptdata detailed json" prop_json_roundtrip_scriptdata_detailed_json
    , testProperty "json roundtrip praos nonce" prop_roundtrip_praos_nonce_JSON
    , testProperty "json roundtrip protocol parameters" prop_roundtrip_protocol_parameters_JSON
    ]
