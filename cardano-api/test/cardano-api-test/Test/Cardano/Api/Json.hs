{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Api.Json
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Tx qualified as Exp

import Cardano.Ledger.Core qualified as L

import Data.Aeson (eitherDecode, encode)

import Test.Gen.Cardano.Api (genAlonzoGenesis)
import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.Orphans ()

import Hedgehog (Property, forAll, tripping, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

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

-- | Round-trips a 'TxOut' whose inline datum uses non-canonical CBOR bytes
-- (definite-length array instead of the canonical indefinite-length form).
prop_json_roundtrip_txout_noncanonical_inline_datum :: Property
prop_json_roundtrip_txout_noncanonical_inline_datum = H.property $ do
  hsd <- forAll genNonCanonicalHashableScriptData
  addr <- forAll $ genAddressInEra ShelleyBasedEraConway
  val <- forAll $ genTxOutValue ShelleyBasedEraConway
  let txOut =
        TxOut addr val (TxOutDatumInline BabbageEraOnwardsConway hsd) ReferenceScriptNone
          :: TxOut CtxUTxO ConwayEra
  tripping txOut encode eitherDecode

prop_json_roundtrip_scriptdata_detailed_json :: Property
prop_json_roundtrip_scriptdata_detailed_json = H.property $ do
  sData <- forAll genHashableScriptData
  tripping sData scriptDataToJsonDetailedSchema scriptDataFromJsonDetailedSchema

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

-- | Verify that the new experimental 'TxOut' 'ToJSON' instance produces
-- the same JSON as the legacy 'txOutToJsonValue' for UTxO outputs across
-- all Shelley-based eras. Dijkstra is skipped because
-- 'shelleyBasedEraConstraints' is not yet implemented for it.
prop_new_txout_json_matches_legacy :: Property
prop_new_txout_json_matches_legacy = H.property $ do
  AnyShelleyBasedEra sbe <- forAll $ Gen.element [minBound .. maxBound]
  case sbe of
    ShelleyBasedEraShelley -> go sbe
    ShelleyBasedEraAllegra -> go sbe
    ShelleyBasedEraMary -> go sbe
    ShelleyBasedEraAlonzo -> go sbe
    ShelleyBasedEraBabbage -> go sbe
    ShelleyBasedEraConway -> go sbe
    ShelleyBasedEraDijkstra -> pure () -- shelleyBasedEraConstraints not yet implemented

go
  :: ( IsCardanoEra era
     , L.EraTxOut (ShelleyLedgerEra era)
     , ToJSON (Exp.TxOut (ShelleyLedgerEra era))
     )
  => ShelleyBasedEra era
  -> H.PropertyT IO ()
go sbe = do
  oldTxOut <- forAll $ genTxOutUTxOContext sbe
  let ledgerTxOut = toShelleyTxOut sbe oldTxOut
      newTxOut = Exp.TxOut ledgerTxOut
  toJSON oldTxOut === toJSON newTxOut

-- | Verify that the new experimental 'TxOut' round-trips through JSON
-- (encode then decode) for all Shelley-based eras except Dijkstra, for which
-- 'shelleyBasedEraConstraints' is not yet implemented.
prop_new_txout_json_roundtrip :: Property
prop_new_txout_json_roundtrip = H.property $ do
  AnyShelleyBasedEra sbe <- forAll $ Gen.element [minBound .. maxBound]
  case sbe of
    ShelleyBasedEraShelley -> goRoundtrip sbe
    ShelleyBasedEraAllegra -> goRoundtrip sbe
    ShelleyBasedEraMary -> goRoundtrip sbe
    ShelleyBasedEraAlonzo -> goRoundtrip sbe
    ShelleyBasedEraBabbage -> goRoundtrip sbe
    ShelleyBasedEraConway -> goRoundtrip sbe
    ShelleyBasedEraDijkstra -> pure () -- shelleyBasedEraConstraints not yet implemented

goRoundtrip
  :: ( L.EraTxOut (ShelleyLedgerEra era)
     , ToJSON (Exp.TxOut (ShelleyLedgerEra era))
     , FromJSON (Exp.TxOut (ShelleyLedgerEra era))
     )
  => ShelleyBasedEra era
  -> H.PropertyT IO ()
goRoundtrip sbe = do
  oldTxOut <- forAll $ genTxOutUTxOContext sbe
  let ledgerTxOut = toShelleyTxOut sbe oldTxOut
      newTxOut = Exp.TxOut ledgerTxOut
  tripping newTxOut encode eitherDecode

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
    , testProperty "json roundtrip txout noncanonical inline datum" prop_json_roundtrip_txout_noncanonical_inline_datum
    , testProperty "json roundtrip scriptdata detailed json" prop_json_roundtrip_scriptdata_detailed_json
    , testProperty "json roundtrip praos nonce" prop_roundtrip_praos_nonce_JSON
    , testProperty "new TxOut ToJSON matches legacy" prop_new_txout_json_matches_legacy
    , testProperty "new TxOut JSON roundtrip" prop_new_txout_json_roundtrip
    ]
