{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive roundtrip tests for TxOut JSON instances across all eras
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

-- | Roundtrip tests for TxOut CtxTx across all eras
testsCtxTx :: [TestTree]
testsCtxTx =
  [ testProperty "shelley" prop_json_roundtrip_txout_ctx_tx_shelley
  , testProperty "allegra" prop_json_roundtrip_txout_ctx_tx_allegra
  , testProperty "mary" prop_json_roundtrip_txout_ctx_tx_mary
  , testProperty "alonzo" prop_json_roundtrip_txout_ctx_tx_alonzo
  , testProperty "babbage" prop_json_roundtrip_txout_ctx_tx_babbage
  , testProperty "conway" prop_json_roundtrip_txout_ctx_tx_conway
  -- Dijkstra era not yet supported in shelleyBasedEraConstraints
  -- , testProperty "dijkstra" prop_json_roundtrip_txout_ctx_tx_dijkstra
  ]

-- | Roundtrip tests for TxOut CtxUTxO across all eras
testsCtxUTxO :: [TestTree]
testsCtxUTxO =
  [ testProperty "shelley" prop_json_roundtrip_txout_ctx_utxo_shelley
  , testProperty "allegra" prop_json_roundtrip_txout_ctx_utxo_allegra
  , testProperty "mary" prop_json_roundtrip_txout_ctx_utxo_mary
  , testProperty "alonzo" prop_json_roundtrip_txout_ctx_utxo_alonzo
  , testProperty "babbage" prop_json_roundtrip_txout_ctx_utxo_babbage
  , testProperty "conway" prop_json_roundtrip_txout_ctx_utxo_conway
  -- Dijkstra era not yet supported in shelleyBasedEraConstraints
  -- , testProperty "dijkstra" prop_json_roundtrip_txout_ctx_utxo_dijkstra
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

prop_json_roundtrip_txout_ctx_tx_shelley :: Property
prop_json_roundtrip_txout_ctx_tx_shelley = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraShelley
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_tx_allegra :: Property
prop_json_roundtrip_txout_ctx_tx_allegra = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraAllegra
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_tx_mary :: Property
prop_json_roundtrip_txout_ctx_tx_mary = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraMary
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_tx_alonzo :: Property
prop_json_roundtrip_txout_ctx_tx_alonzo = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraAlonzo
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_tx_babbage :: Property
prop_json_roundtrip_txout_ctx_tx_babbage = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraBabbage
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_tx_conway :: Property
prop_json_roundtrip_txout_ctx_tx_conway = H.property $ do
  txOut <- forAll $ genTxOutTxContext ShelleyBasedEraConway
  tripping txOut encode eitherDecode

-- Dijkstra era not yet supported in shelleyBasedEraConstraints
-- prop_json_roundtrip_txout_ctx_tx_dijkstra :: Property
-- prop_json_roundtrip_txout_ctx_tx_dijkstra = H.property $ do
--   txOut <- forAll $ genTxOutTxContext ShelleyBasedEraDijkstra
--   tripping txOut encode eitherDecode

-- -----------------------------------------------------------------------------
-- CtxUTxO Roundtrip Properties
-- -----------------------------------------------------------------------------

prop_json_roundtrip_txout_ctx_utxo_shelley :: Property
prop_json_roundtrip_txout_ctx_utxo_shelley = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraShelley
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_utxo_allegra :: Property
prop_json_roundtrip_txout_ctx_utxo_allegra = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraAllegra
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_utxo_mary :: Property
prop_json_roundtrip_txout_ctx_utxo_mary = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraMary
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_utxo_alonzo :: Property
prop_json_roundtrip_txout_ctx_utxo_alonzo = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraAlonzo
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_utxo_babbage :: Property
prop_json_roundtrip_txout_ctx_utxo_babbage = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraBabbage
  tripping txOut encode eitherDecode

prop_json_roundtrip_txout_ctx_utxo_conway :: Property
prop_json_roundtrip_txout_ctx_utxo_conway = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraConway
  tripping txOut encode eitherDecode

-- Dijkstra era not yet supported in shelleyBasedEraConstraints
-- prop_json_roundtrip_txout_ctx_utxo_dijkstra :: Property
-- prop_json_roundtrip_txout_ctx_utxo_dijkstra = H.property $ do
--   txOut <- forAll $ genTxOutUTxOContext ShelleyBasedEraDijkstra
--   tripping txOut encode eitherDecode

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
