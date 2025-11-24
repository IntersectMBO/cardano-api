{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive roundtrip tests for TxOut JSON instances across all eras
module Test.Cardano.Api.TxOut.JsonRoundtrip
  ( tests
  )
where

import Cardano.Api

import Data.Aeson (eitherDecode, encode)

import Test.Gen.Cardano.Api.Era
import Test.Gen.Cardano.Api.TxOut
import Test.Gen.Cardano.Api.Typed

import Hedgehog (Property, forAll, tripping)
import Hedgehog qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | All roundtrip tests
tests :: TestTree
tests =
  testGroup
    "JsonRoundtrip"
    [ testGroup "CtxTx" testsCtxTx
    , testGroup "CtxUTxO" testsCtxUTxO
    , testGroup "Datum-Specific" testsDatumSpecific
    ]

-- | Roundtrip tests for TxOut CtxTx across all eras
testsCtxTx :: [TestTree]
testsCtxTx =
  [ testProperty "all eras" prop_json_roundtrip_txout_ctx_tx
  ]

-- | Roundtrip tests for TxOut CtxUTxO across all eras
testsCtxUTxO :: [TestTree]
testsCtxUTxO =
  [ testProperty "all eras" prop_json_roundtrip_txout_ctx_utxo
  ]

-- | Datum-specific roundtrip tests
testsDatumSpecific :: [TestTree]
testsDatumSpecific =
  [ testProperty "no datum (Alonzo)" prop_json_roundtrip_txout_no_datum
  , testProperty "datum hash (Alonzo)" prop_json_roundtrip_txout_datum_hash
  , testProperty "supplemental datum (Alonzo)" prop_json_roundtrip_txout_supplemental_datum
  , testProperty "inline datum (Babbage)" prop_json_roundtrip_txout_inline_datum
  ]

-- -----------------------------------------------------------------------------
-- CtxTx Roundtrip Properties
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_ctx_tx :: Property
prop_json_roundtrip_txout_ctx_tx = H.property $ do
  AnyShelleyBasedEra sbe <- forAll genAnyShelleyBasedEra
  shelleyBasedEraConstraints sbe $ do
    txOut <- forAll $ genTxOutTxContext sbe
    tripping txOut encode eitherDecode

-- -----------------------------------------------------------------------------
-- CtxUTxO Roundtrip Properties
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_ctx_utxo :: Property
prop_json_roundtrip_txout_ctx_utxo = H.property $ do
  AnyShelleyBasedEra sbe <- forAll genAnyShelleyBasedEra
  shelleyBasedEraConstraints sbe $ do
    txOut <- forAll $ genTxOutUTxOContext sbe
    tripping txOut encode eitherDecode

-- -----------------------------------------------------------------------------
-- Datum-Specific Roundtrip Properties
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_no_datum :: Property
prop_json_roundtrip_txout_no_datum = H.property $ do
  txOut <- forAll $ genTxOutWithNoDatum ShelleyBasedEraAlonzo
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_datum_hash :: Property
prop_json_roundtrip_txout_datum_hash = H.property $ do
  txOut <- forAll $ genTxOutWithDatumHash AlonzoEraOnwardsAlonzo
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_supplemental_datum :: Property
prop_json_roundtrip_txout_supplemental_datum = H.property $ do
  txOut <- forAll $ genTxOutWithSupplementalDatum AlonzoEraOnwardsAlonzo
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_inline_datum :: Property
prop_json_roundtrip_txout_inline_datum = H.property $ do
  txOut <- forAll $ genTxOutWithInlineDatum BabbageEraOnwardsBabbage
  tripping txOut encode eitherDecode
