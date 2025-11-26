{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Error case tests for TxOut JSON parsing
--
-- Note: These tests only use FromJSON (parsing), so they can test any
-- ShelleyBasedEra. ToJSON is not required for error case validation.
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

-- Note: Dijkstra era tests are commented out as shelleyBasedEraConstraints
-- doesn't yet fully support Dijkstra.
testsConflictingDatums :: [TestTree]
testsConflictingDatums =
  [ testPropertyNamed
      "Conway: reject conflicting Alonzo and Conway datums"
      "prop_reject_conflicting_datums_conway"
      prop_reject_conflicting_datums_conway
  ]

prop_reject_conflicting_datums_conway :: Property
prop_reject_conflicting_datums_conway = H.property $ do
  json <- forAll genConflictingDatumJSON
  assertParseFailsWithMessage @(TxOut CtxTx ConwayEra) json "Alonzo era datum and a Conway era datum"

-- -----------------------------------------------------------------------------
-- Mismatched Hash Tests
-- -----------------------------------------------------------------------------

testsMismatchedHashes :: [TestTree]
testsMismatchedHashes =
  [ testPropertyNamed
      "Conway CtxTx: reject mismatched inline datum hash"
      "prop_reject_mismatched_hash_conway_ctx_tx"
      prop_reject_mismatched_hash_conway_ctx_tx
  , testPropertyNamed
      "Conway CtxUTxO: reject mismatched inline datum hash"
      "prop_reject_mismatched_hash_conway_ctx_utxo"
      prop_reject_mismatched_hash_conway_ctx_utxo
  ]

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
      "Conway CtxTx: reject partial inline datum fields"
      "prop_reject_partial_inline_datum_conway_ctx_tx"
      prop_reject_partial_inline_datum_conway_ctx_tx
  , testPropertyNamed
      "Conway CtxUTxO: reject partial inline datum fields"
      "prop_reject_partial_inline_datum_conway_ctx_utxo"
      prop_reject_partial_inline_datum_conway_ctx_utxo
  ]

prop_reject_partial_inline_datum_conway_ctx_tx :: Property
prop_reject_partial_inline_datum_conway_ctx_tx = H.property $ do
  json <- forAll genPartialInlineDatumJSON
  assertParseFailsWithMessage @(TxOut CtxTx ConwayEra)
    json
    "either an inline datum hash or an inline datum"

prop_reject_partial_inline_datum_conway_ctx_utxo :: Property
prop_reject_partial_inline_datum_conway_ctx_utxo = H.property $ do
  json <- forAll genPartialInlineDatumJSON
  assertParseFailsWithMessage @(TxOut CtxUTxO ConwayEra)
    json
    "either an inline datum hash or an inline datum"

-- -----------------------------------------------------------------------------
-- Invalid Data Tests
-- -----------------------------------------------------------------------------

testsInvalidData :: [TestTree]
testsInvalidData =
  [ testPropertyNamed
      "Conway: reject datum without hash"
      "prop_reject_datum_without_hash"
      prop_reject_datum_without_hash
  , testPropertyNamed
      "Conway: reject invalid script data in datum"
      "prop_reject_invalid_script_data_datum"
      prop_reject_invalid_script_data_datum
  , testPropertyNamed
      "Conway: reject invalid script data in inline datum"
      "prop_reject_invalid_script_data_inline_datum"
      prop_reject_invalid_script_data_inline_datum
  ]

prop_reject_datum_without_hash :: Property
prop_reject_datum_without_hash = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraConway
  val <- forAll $ genTxOutValue ShelleyBasedEraConway
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "datum" .= object ["int" .= (42 :: Int)]
          ]
  assertParseFailsWithMessage @(TxOut CtxTx ConwayEra) json "Only datum JSON was found"

prop_reject_invalid_script_data_datum :: Property
prop_reject_invalid_script_data_datum = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraConway
  val <- forAll $ genTxOutValue ShelleyBasedEraConway
  scriptDataHash <- forAll genHashScriptData
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "datumhash" .= scriptDataHash
          , "datum" .= object ["invalid" .= ("structure" :: String)]
          ]
  assertParseFails @(TxOut CtxTx ConwayEra) json

prop_reject_invalid_script_data_inline_datum :: Property
prop_reject_invalid_script_data_inline_datum = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraConway
  val <- forAll $ genTxOutValue ShelleyBasedEraConway
  scriptDataHash <- forAll genHashScriptData
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "inlineDatumhash" .= scriptDataHash
          , "inlineDatum" .= object ["invalid" .= ("structure" :: String)]
          ]
  assertParseFails @(TxOut CtxTx ConwayEra) json

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
  val <- forAll $ genTxOutValue ShelleyBasedEraConway
  let json = object ["value" .= val]
  assertParseFails @(TxOut CtxTx ConwayEra) json

prop_reject_missing_value :: Property
prop_reject_missing_value = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraConway
  let json = object ["address" .= addr]
  assertParseFails @(TxOut CtxTx ConwayEra) json
