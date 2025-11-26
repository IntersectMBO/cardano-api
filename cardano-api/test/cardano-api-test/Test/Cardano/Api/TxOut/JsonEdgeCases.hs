{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Edge case tests for TxOut JSON instances
module Test.Cardano.Api.TxOut.JsonEdgeCases
  ( tests
  )
where

import Cardano.Api hiding (Value)

import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))

import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.TxOut.Gen
import Test.Cardano.Api.TxOut.Helpers

import Hedgehog (Property, forAll)
import Hedgehog qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | All edge case tests
tests :: TestTree
tests =
  testGroup
    "JsonEdgeCases"
    [ testGroup "Supplemental Datum Behavior" testsSupplementalDatum
    , testGroup "Null Field Handling" testsNullFields
    , testGroup "ToJSON Output Validation" testsToJSONValidation
    ]

-- -----------------------------------------------------------------------------
-- Supplemental Datum Tests
-- -----------------------------------------------------------------------------

testsSupplementalDatum :: [TestTree]
testsSupplementalDatum =
  [ testPropertyNamed
      "supplemental datum produces both datumhash and datum fields"
      "prop_supplemental_datum_produces_both_fields"
      prop_supplemental_datum_produces_both_fields
  , testPropertyNamed
      "supplemental datum roundtrips to supplemental (not hash)"
      "prop_supplemental_datum_roundtrips_to_supplemental"
      prop_supplemental_datum_roundtrips_to_supplemental
  ]

prop_supplemental_datum_produces_both_fields :: Property
prop_supplemental_datum_produces_both_fields = H.property $ do
  txOut <- forAll $ genTxOutWithSupplementalDatum AlonzoEraOnwardsAlonzo
  let json = toJSON txOut
  assertHasFields json ["datumhash", "datum"]
  -- Verify datumhash is not null
  case json of
    Object obj -> do
      case getObjectField obj "datumhash" of
        Just Null -> do
          H.annotate "datumhash should not be null for supplemental datum"
          H.failure
        Just _ -> return ()
        Nothing -> do
          H.annotate "datumhash field missing"
          H.failure
      case getObjectField obj "datum" of
        Just Null -> do
          H.annotate "datum should not be null for supplemental datum"
          H.failure
        Just _ -> return ()
        Nothing -> do
          H.annotate "datum field missing"
          H.failure
    _ -> do
      H.annotate "Expected JSON object"
      H.failure

prop_supplemental_datum_roundtrips_to_supplemental :: Property
prop_supplemental_datum_roundtrips_to_supplemental = H.property $ do
  txOut@(TxOut _ _ datum _) <- forAll $ genTxOutWithSupplementalDatum AlonzoEraOnwardsAlonzo
  case datum of
    TxOutSupplementalDatum{} -> do
      let decoded = eitherDecode @(TxOut CtxTx AlonzoEra) (encode txOut)
      case decoded of
        Right (TxOut _ _ decodedDatum _) ->
          case decodedDatum of
            TxOutSupplementalDatum{} -> H.success
            _ -> do
              H.annotate $ "Expected TxOutSupplementalDatum but got: " <> show decodedDatum
              H.failure
        Left err -> do
          H.annotate $ "Decode failed: " <> err
          H.failure
    _ -> do
      H.annotate "Expected TxOutSupplementalDatum"
      H.failure

-- -----------------------------------------------------------------------------
-- Null Field Handling Tests
-- -----------------------------------------------------------------------------

testsNullFields :: [TestTree]
testsNullFields =
  [ testPropertyNamed
      "null fields optional for parsing"
      "prop_null_fields_optional"
      prop_null_fields_optional
  , testPropertyNamed
      "explicit null fields accepted"
      "prop_explicit_null_fields_accepted"
      prop_explicit_null_fields_accepted
  ]

prop_null_fields_optional :: Property
prop_null_fields_optional = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraBabbage
  val <- forAll $ genTxOutValue ShelleyBasedEraBabbage
  let json = object ["address" .= addr, "value" .= val]
  case eitherDecode @(TxOut CtxTx BabbageEra) (encode json) of
    Right (TxOut _ _ datum _) ->
      assertDatumEqual datum TxOutDatumNone
    Left err -> do
      H.annotate $ "Parse failed: " <> err
      H.failure

prop_explicit_null_fields_accepted :: Property
prop_explicit_null_fields_accepted = H.property $ do
  addr <- forAll $ genAddressInEra ShelleyBasedEraBabbage
  val <- forAll $ genTxOutValue ShelleyBasedEraBabbage
  let json =
        object
          [ "address" .= addr
          , "value" .= val
          , "datumhash" .= Null
          , "datum" .= Null
          , "inlineDatum" .= Null
          , "referenceScript" .= Null
          ]
  case eitherDecode @(TxOut CtxTx BabbageEra) (encode json) of
    Right (TxOut _ _ datum refScript) -> do
      assertDatumEqual datum TxOutDatumNone
      case refScript of
        ReferenceScriptNone -> H.success
        _ -> do
          H.annotate $ "Expected ReferenceScriptNone but got: " <> show refScript
          H.failure
    Left err -> do
      H.annotate $ "Parse failed: " <> err
      H.failure

-- -----------------------------------------------------------------------------
-- ToJSON Output Validation Tests
-- -----------------------------------------------------------------------------

testsToJSONValidation :: [TestTree]
testsToJSONValidation =
  [ testPropertyNamed
      "no datum has null fields (Babbage)"
      "prop_toJSON_no_datum_has_null_fields"
      prop_toJSON_no_datum_has_null_fields
  , testPropertyNamed
      "inline datum uses inline fields"
      "prop_toJSON_inline_datum_uses_inline_fields"
      prop_toJSON_inline_datum_uses_inline_fields
  ]

prop_toJSON_no_datum_has_null_fields :: Property
prop_toJSON_no_datum_has_null_fields = H.property $ do
  txOut <- forAll $ genTxOutWithNoDatum ShelleyBasedEraBabbage
  let json = toJSON txOut
  assertHasFields json ["datumhash", "datum", "inlineDatum", "inlineDatumRaw", "referenceScript"]
  assertAllNull json ["datumhash", "datum", "inlineDatum", "inlineDatumRaw", "referenceScript"]

prop_toJSON_inline_datum_uses_inline_fields :: Property
prop_toJSON_inline_datum_uses_inline_fields = H.property $ do
  txOut <- forAll $ genTxOutWithInlineDatum BabbageEraOnwardsBabbage
  let json = toJSON txOut
  -- Should have inlineDatumhash and inlineDatum
  assertHasFields json ["inlineDatumhash", "inlineDatum"]
  case json of
    Object obj -> do
      -- inlineDatumhash and inlineDatum should not be null
      case getObjectField obj "inlineDatumhash" of
        Just Null -> do
          H.annotate "inlineDatumhash should not be null for inline datum"
          H.failure
        Just _ -> return ()
        Nothing -> do
          H.annotate "inlineDatumhash field missing"
          H.failure
      case getObjectField obj "inlineDatum" of
        Just Null -> do
          H.annotate "inlineDatum should not be null for inline datum"
          H.failure
        Just _ -> return ()
        Nothing -> do
          H.annotate "inlineDatum field missing"
          H.failure
      -- datum field should be null (datumhash doesn't exist for inline datums)
      assertFieldNull json "datum"
    _ -> do
      H.annotate "Expected JSON object"
      H.failure
