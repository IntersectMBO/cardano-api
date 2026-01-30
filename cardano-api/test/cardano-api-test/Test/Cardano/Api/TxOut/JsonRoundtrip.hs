{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive roundtrip tests for TxOut JSON instances
--
-- Note: These tests only cover Conway and Dijkstra eras because:
-- - TxOut ToJSON now uses the experimental Exp.IsEra constraint (Conway+)
-- - TxOut FromJSON uses IsShelleyBasedEra constraint
-- - Roundtrip tests require both encode and decode, so only Conway+ is tested
module Test.Cardano.Api.TxOut.JsonRoundtrip
  ( tests
  )
where

import Cardano.Api

import Data.Aeson (eitherDecode, encode)

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

-- | Roundtrip tests for TxOut CtxTx (Conway era)
--
-- Note: Dijkstra era is not yet fully supported by shelleyBasedEraConstraints.
testsCtxTx :: [TestTree]
testsCtxTx =
  [ testProperty "conway" prop_json_roundtrip_txout_ctx_tx_conway
  ]

-- | Roundtrip tests for TxOut CtxUTxO (Conway era)
--
-- Note: Dijkstra era is not yet fully supported by shelleyBasedEraConstraints.
testsCtxUTxO :: [TestTree]
testsCtxUTxO =
  [ testProperty "conway" prop_json_roundtrip_txout_ctx_utxo_conway
  ]

-- | Datum-specific roundtrip tests (Conway era)
testsDatumSpecific :: [TestTree]
testsDatumSpecific =
  [ testProperty "no datum (Conway)" prop_json_roundtrip_txout_no_datum
  , testProperty "datum hash (Conway)" prop_json_roundtrip_txout_datum_hash
  , testProperty "supplemental datum (Conway)" prop_json_roundtrip_txout_supplemental_datum
  , testProperty "inline datum (Conway)" prop_json_roundtrip_txout_inline_datum
  ]

-- -----------------------------------------------------------------------------
-- CtxTx Roundtrip Properties
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_ctx_tx_conway :: Property
prop_json_roundtrip_txout_ctx_tx_conway = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraConway
  tripping txOut encode eitherDecode

-- -----------------------------------------------------------------------------
-- CtxUTxO Roundtrip Properties
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_ctx_utxo_conway :: Property
prop_json_roundtrip_txout_ctx_utxo_conway = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraConway
  tripping txOut encode eitherDecode

-- -----------------------------------------------------------------------------
-- Datum-Specific Roundtrip Properties (Conway era)
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_no_datum :: Property
prop_json_roundtrip_txout_no_datum = H.property $ do
  txOut <- forAll $ genTxOutWithNoDatum ShelleyBasedEraConway
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_datum_hash :: Property
prop_json_roundtrip_txout_datum_hash = H.property $ do
  txOut <- forAll $ genTxOutWithDatumHash AlonzoEraOnwardsConway
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_supplemental_datum :: Property
prop_json_roundtrip_txout_supplemental_datum = H.property $ do
  txOut <- forAll $ genTxOutWithSupplementalDatum AlonzoEraOnwardsConway
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_inline_datum :: Property
prop_json_roundtrip_txout_inline_datum = H.property $ do
  txOut <- forAll $ genTxOutWithInlineDatum BabbageEraOnwardsConway
  tripping txOut encode eitherDecode
