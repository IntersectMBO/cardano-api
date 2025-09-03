{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Ledger
  ( tests
  )
where

import Cardano.Api

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Hashes

import Control.Monad
import Control.Monad.Identity

import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.Orphans ()

import Hedgehog qualified as H
import Hedgehog.Internal.Property
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- prop_original_scriptdata_bytes_preserved and prop_roundtrip_scriptdata_plutusdata
-- allow us to generate a 'HashableScriptData' value from JSON with the original bytes being
-- derived from a JSON 'Value'. We serialize the 'ScriptData' (derived from the 'Value')
-- to CBOR and take those as the original bytes. Under the hood ScriptData is converted to PlutusData
-- before serializing.

prop_original_scriptdata_bytes_preserved :: Property
prop_original_scriptdata_bytes_preserved = H.property $ do
  schema <- forAll genScriptDataSchema
  sDataValue <- scriptDataToJson schema <$> forAll genHashableScriptData
  case scriptDataJsonToHashable schema sDataValue of
    Left e -> failWith Nothing $ show e
    Right hScriptData -> do
      let ScriptDataHash apiHash = hashScriptDataBytes hScriptData
          ledgerAlonzoData = toAlonzoData hScriptData :: L.Data L.AlonzoEra
      -- We check that our hashScriptDataBytes is equivalent to `L.hashData`
      -- This test will let us know if our 'hashScriptDataBytes' is ever broken
      L.hashData ledgerAlonzoData === apiHash

      -- We also check that the original bytes are the same after the calling
      -- toAlonzoData :: HashableScriptData -> L.Data ledgerera.
      originalBytes ledgerAlonzoData === getOriginalScriptDataBytes hScriptData

prop_roundtrip_scriptdata_plutusdata :: Property
prop_roundtrip_scriptdata_plutusdata = H.property $ do
  sd <- getScriptData <$> forAll genHashableScriptData
  H.tripping sd toPlutusData (Identity . fromPlutusData)

prop_roundtrip_ledger_txout :: Property
prop_roundtrip_ledger_txout = H.property $ do
  forM_ [minBound .. maxBound] $ \(AnyShelleyBasedEra era) -> do
    txOut <- forAll $ genTxOutUTxOContext era
    txOut H.=== fromShelleyTxOut era (toShelleyTxOut era txOut)

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Ledger"
    [ testProperty "roundtrip ScriptData" prop_roundtrip_scriptdata_plutusdata
    , testProperty "script data bytes preserved" prop_original_scriptdata_bytes_preserved
    , testProperty "roundtrip Ledger TxOut" prop_roundtrip_ledger_txout
    ]
