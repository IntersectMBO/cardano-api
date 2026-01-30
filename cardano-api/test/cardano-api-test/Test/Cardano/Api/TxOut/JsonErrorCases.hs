{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Error case tests for TxOut JSON parsing
module Test.Cardano.Api.TxOut.JsonErrorCases
  ( tests
  )
where

import Cardano.Api hiding (Value)

import Data.Aeson (object, (.=))

import Test.Gen.Cardano.Api.TxOut
import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.TxOut.Helpers

import Hedgehog (Property, forAll)
import Hedgehog qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | All error case tests
tests :: TestTree
tests =
  testGroup
    "JsonErrorCases"
    [ testGroup "Conflicting Datums" testsConflictingDatums
    , testGroup "Mismatched Hashes" testsMismatchedHashes
    , testGroup "Partial Fields" testsPartialFields
    , testGroup "Invalid Data" testsInvalidData
    , testGroup "Missing Required Fields" testsMissingFields
    ]

-- -----------------------------------------------------------------------------
-- Conflicting Datum Tests
-- -----------------------------------------------------------------------------

testsConflictingDatums :: [TestTree]
testsConflictingDatums =
  [ testPropertyNamed
      "Babbage: reject conflicting Alonzo and Babbage datums"
      "prop_reject_conflicting_datums_babbage"
      prop_reject_conflicting_datums_babbage
  , testPropertyNamed
      "Conway: reject conflicting Alonzo and Conway datums"
      "prop_reject_conflicting_datums_conway"
      prop_reject_conflicting_datums_conway
      -- Dijkstra era not yet supported in shelleyBasedEraConstraints
      -- , testPropertyNamed
      --     "Dijkstra: reject conflicting Alonzo and Dijkstra datums"
      --     "prop_reject_conflicting_datums_dijkstra"
      --     prop_reject_conflicting_datums_dijkstra
  ]

prop_reject_conflicting_datums_babbage :: Property
prop_reject_conflicting_datums_babbage = H.property $ do
  json <- forAll genConflictingDatumJSON
  assertParseFailsWithMessage @(TxOut CtxTx BabbageEra)
    json
    "Alonzo era datum and a Babbage era datum"

prop_reject_conflicting_datums_conway :: Property
prop_reject_conflicting_datums_conway = H.property $ do
  json <- forAll genConflictingDatumJSON
  assertParseFailsWithMessage @(TxOut CtxTx ConwayEra) json "Alonzo era datum and a Conway era datum"

-- Dijkstra era not yet supported in shelleyBasedEraConstraints
-- prop_reject_conflicting_datums_dijkstra :: Property
-- prop_reject_conflicting_datums_dijkstra = H.property $ do
--   json <- forAll genConflictingDatumJSON
--   H.evalIO $ assertParseFailsWithMessage @(TxOut CtxTx DijkstraEra) json "Alonzo era datum and a"

-- -----------------------------------------------------------------------------
-- Mismatched Hash Tests
-- -----------------------------------------------------------------------------

testsMismatchedHashes :: [TestTree]
testsMismatchedHashes =
  [ testPropertyNamed
      "Babbage CtxTx: reject mismatched inline datum hash"
      "prop_reject_mismatched_hash_babbage_ctx_tx"
      prop_reject_mismatched_hash_babbage_ctx_tx
  , testPropertyNamed
      "Babbage CtxUTxO: reject mismatched inline datum hash"
      "prop_reject_mismatched_hash_babbage_ctx_utxo"
      prop_reject_mismatched_hash_babbage_ctx_utxo
  , testPropertyNamed
      "Conway CtxTx: reject mismatched inline datum hash"
      "prop_reject_mismatched_hash_conway_ctx_tx"
      prop_reject_mismatched_hash_conway_ctx_tx
  , testPropertyNamed
      "Conway CtxUTxO: reject mismatched inline datum hash"
      "prop_reject_mismatched_hash_conway_ctx_utxo"
      prop_reject_mismatched_hash_conway_ctx_utxo
  ]

prop_reject_mismatched_hash_babbage_ctx_tx :: Property
prop_reject_mismatched_hash_babbage_ctx_tx = H.property $ do
  json <- forAll genMismatchedInlineDatumHashJSON
  assertParseFailsWithMessage @(TxOut CtxTx BabbageEra)
    json
    "Inline datum not equivalent to inline datum hash"

prop_reject_mismatched_hash_babbage_ctx_utxo :: Property
prop_reject_mismatched_hash_babbage_ctx_utxo = H.property $ do
  json <- forAll genMismatchedInlineDatumHashJSON
  assertParseFailsWithMessage @(TxOut CtxUTxO BabbageEra)
    json
    "Inline datum not equivalent to inline datum hash"

prop_reject_mismatched_hash_conway_ctx_tx :: Property
prop_reject_mismatched_hash_conway_ctx_tx = H.property $ do
  json <- forAll genMismatchedInlineDatumHashJSON
  assertParseFailsWithMessage @(TxOut CtxTx ConwayEra)
    json
    "Inline datum not equivalent to inline datum hash"

prop_reject_mismatched_hash_conway_ctx_utxo :: Property
prop_reject_mismatched_hash_conway_ctx_utxo = H.property $ do
  json <- forAll genMismatchedInlineDatumHashJSON
  assertParseFailsWithMessage @(TxOut CtxUTxO ConwayEra)
    json
    "Inline datum not equivalent to inline datum hash"

-- -----------------------------------------------------------------------------
-- Partial Field Tests
-- -----------------------------------------------------------------------------

testsPartialFields :: [TestTree]
testsPartialFields =
  [ testPropertyNamed
      "Babbage CtxTx: reject partial inline datum fields"
      "prop_reject_partial_inline_datum_babbage_ctx_tx"
      prop_reject_partial_inline_datum_babbage_ctx_tx
  , testPropertyNamed
      "Babbage CtxUTxO: reject partial inline datum fields"
      "prop_reject_partial_inline_datum_babbage_ctx_utxo"
      prop_reject_partial_inline_datum_babbage_ctx_utxo
  , testPropertyNamed
      "Conway CtxTx: reject partial inline datum fields"
      "prop_reject_partial_inline_datum_conway_ctx_tx"
      prop_reject_partial_inline_datum_conway_ctx_tx
  ]

prop_reject_partial_inline_datum_babbage_ctx_tx :: Property
prop_reject_partial_inline_datum_babbage_ctx_tx = H.property $ do
  json <- forAll genPartialInlineDatumJSON
  assertParseFailsWithMessage @(TxOut CtxTx BabbageEra)
    json
    "either an inline datum hash or an inline datum"

prop_reject_partial_inline_datum_babbage_ctx_utxo :: Property
prop_reject_partial_inline_datum_babbage_ctx_utxo = H.property $ do
  json <- forAll genPartialInlineDatumJSON
  assertParseFailsWithMessage @(TxOut CtxUTxO BabbageEra)
    json
    "either an inline datum hash or an inline datum"

prop_reject_partial_inline_datum_conway_ctx_tx :: Property
prop_reject_partial_inline_datum_conway_ctx_tx = H.property $ do
  json <- forAll genPartialInlineDatumJSON
  assertParseFailsWithMessage @(TxOut CtxTx ConwayEra)
    json
    "either an inline datum hash or an inline datum"

-- -----------------------------------------------------------------------------
-- Invalid Data Tests
-- -----------------------------------------------------------------------------

testsInvalidData :: [TestTree]
testsInvalidData =
  [ testPropertyNamed
      "Alonzo: reject datum without hash"
      "prop_reject_datum_without_hash"
      prop_reject_datum_without_hash
  , testPropertyNamed
      "Babbage: reject invalid script data in datum"
      "prop_reject_invalid_script_data_datum"
      prop_reject_invalid_script_data_datum
  , testPropertyNamed
      "Babbage: reject invalid script data in inline datum"
      "prop_reject_invalid_script_data_inline_datum"
      prop_reject_invalid_script_data_inline_datum
  ]

prop_reject_datum_without_hash :: Property
prop_reject_datum_without_hash = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraAlonzo
  val <- forAll $ genTxOutValue ShelleyBasedEraAlonzo
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "datum" .= object ["int" .= (42 :: Int)]
          ]
  assertParseFailsWithMessage @(TxOut CtxTx AlonzoEra) json "Only datum JSON was found"

prop_reject_invalid_script_data_datum :: Property
prop_reject_invalid_script_data_datum = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraBabbage
  val <- forAll $ genTxOutValue ShelleyBasedEraBabbage
  scriptDataHash <- forAll genHashScriptData
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "datumhash" .= scriptDataHash
          , "datum" .= object ["invalid" .= ("structure" :: String)]
          ]
  assertParseFails @(TxOut CtxTx BabbageEra) json

prop_reject_invalid_script_data_inline_datum :: Property
prop_reject_invalid_script_data_inline_datum = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraBabbage
  val <- forAll $ genTxOutValue ShelleyBasedEraBabbage
  scriptDataHash <- forAll genHashScriptData
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "inlineDatumhash" .= scriptDataHash
          , "inlineDatum" .= object ["invalid" .= ("structure" :: String)]
          ]
  assertParseFails @(TxOut CtxTx BabbageEra) json

-- -----------------------------------------------------------------------------
-- Missing Required Fields Tests
-- -----------------------------------------------------------------------------

testsMissingFields :: [TestTree]
testsMissingFields =
  [ testPropertyNamed "reject missing address" "prop_reject_missing_address" prop_reject_missing_address
  , testPropertyNamed "reject missing value" "prop_reject_missing_value" prop_reject_missing_value
  ]

prop_reject_missing_address :: Property
prop_reject_missing_address = H.property $ do
  val <- forAll $ genTxOutValue ShelleyBasedEraBabbage
  let json = object ["value" .= val]
  assertParseFails @(TxOut CtxTx BabbageEra) json

prop_reject_missing_value :: Property
prop_reject_missing_value = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraBabbage
  let json = object ["address" .= addr]
  assertParseFails @(TxOut CtxTx BabbageEra) json
