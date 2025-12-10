{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Golden tests for TxOut, TxOutValue, AddressInEra, and ReferenceScript JSON serialization.
--
-- These tests ensure backwards compatibility of the JSON format for types whose
-- ToJSON/FromJSON instances were migrated from IsShelleyBasedEra/IsCardanoEra
-- constraints to Exp.IsEra constraints.
module Test.Golden.Cardano.Api.TxOut
  ( -- * TxOut golden tests
    tasty_golden_TxOut_ConwayEra_simple
  , tasty_golden_TxOut_ConwayEra_datumHash
  , tasty_golden_TxOut_ConwayEra_inlineDatum
  , tasty_golden_TxOut_ConwayEra_referenceScript
  , tasty_golden_TxOut_ConwayEra_full

    -- * TxOutValue golden tests
  , tasty_golden_TxOutValue_ConwayEra_lovelaceOnly
  , tasty_golden_TxOutValue_ConwayEra_multiAsset

    -- * AddressInEra golden tests
  , tasty_golden_AddressInEra_ConwayEra

    -- * ReferenceScript golden tests
  , tasty_golden_ReferenceScript_ConwayEra

    -- * UTxO golden tests
  , tasty_golden_UTxO_ConwayEra
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Control.Error.Util (hush)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe (fromJust)
import GHC.Exts (IsList (..))
import System.FilePath ((</>))

import Hedgehog.Extras (UnitIO)
import Hedgehog.Extras qualified as H

-- -----------------------------------------------------------------------------
-- Test data construction helpers
-- -----------------------------------------------------------------------------

goldenPath :: FilePath
goldenPath = "test/cardano-api-golden/files/TxOut"

-- | Helper to encode value as pretty JSON string
toJsonString :: Aeson.ToJSON a => a -> String
toJsonString a = BL8.unpack (encodePretty a)

-- | Create a simple payment address for testing
mkTestAddress :: ShelleyBasedEra era -> AddressInEra era
mkTestAddress sbe =
  shelleyAddressInEra sbe $
    makeShelleyAddress
      Mainnet
      (PaymentCredentialByKey testPaymentKeyHash)
      NoStakeAddress

testPaymentKeyHash :: Hash PaymentKey
testPaymentKeyHash =
  fromJust $
    hush $
      deserialiseFromRawBytesHex "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"

-- | Create a test TxOutValue with only lovelace
mkLovelaceValue :: ShelleyBasedEra era -> L.Coin -> TxOutValue era
mkLovelaceValue = lovelaceToTxOutValue

-- | Create a test TxOutValue with multi-assets
mkMultiAssetValue :: ShelleyBasedEra era -> TxOutValue era
mkMultiAssetValue sbe =
  shelleyBasedEraConstraints sbe $
    TxOutValueShelleyBased sbe $
      toLedgerValue (maryEraOnwardsToMaryEraOnwards sbe) testMultiAssetValue
 where
  maryEraOnwardsToMaryEraOnwards :: ShelleyBasedEra era -> MaryEraOnwards era
  maryEraOnwardsToMaryEraOnwards = \case
    ShelleyBasedEraConway -> MaryEraOnwardsConway
    ShelleyBasedEraDijkstra -> MaryEraOnwardsDijkstra
    _ -> error "mkMultiAssetValue: unsupported era"

testMultiAssetValue :: Value
testMultiAssetValue =
  fromList
    [ (AdaAssetId, Quantity 2_000_000)
    , (AssetId testPolicyId testAssetName, Quantity 100)
    ]

testPolicyId :: PolicyId
testPolicyId =
  fromJust $
    hush $
      deserialiseFromRawBytesHex "a0000000000000000000000000000000000000000000000000000000"

testAssetName :: AssetName
testAssetName =
  fromJust $
    hush $
      deserialiseFromRawBytes AsAssetName "TestToken"

-- | Create a test datum hash
testDatumHash :: Hash ScriptData
testDatumHash =
  fromJust $
    hush $
      deserialiseFromRawBytesHex "ffd29f3e52e7cf2eb451a59448fd55f9c64e4c1ad1ab0e500d6ceb6d7ff97e9c"

-- | Create test inline datum (ScriptData)
testScriptData :: HashableScriptData
testScriptData =
  fromJust $
    hush $
      deserialiseFromCBOR AsHashableScriptData testScriptDataCBOR
 where
  -- CBOR encoding of ScriptDataNumber 42
  testScriptDataCBOR :: ByteString
  testScriptDataCBOR = "\24\42"

-- | Create a test simple script for reference script testing
testSimpleScript :: Script SimpleScript'
testSimpleScript =
  SimpleScript $
    RequireAllOf
      [ RequireSignature testPaymentKeyHash
      ]

-- | Create a test reference script
mkTestReferenceScript :: ShelleyBasedEra era -> ReferenceScript era
mkTestReferenceScript = \case
  ShelleyBasedEraConway ->
    ReferenceScript BabbageEraOnwardsConway (ScriptInAnyLang SimpleScriptLanguage testSimpleScript)
  ShelleyBasedEraDijkstra ->
    ReferenceScript BabbageEraOnwardsDijkstra (ScriptInAnyLang SimpleScriptLanguage testSimpleScript)
  _ -> error "mkTestReferenceScript: unsupported era"

-- -----------------------------------------------------------------------------
-- TxOut golden tests - Conway Era
-- -----------------------------------------------------------------------------

tasty_golden_TxOut_ConwayEra_simple :: UnitIO ()
tasty_golden_TxOut_ConwayEra_simple =
  H.diffVsGoldenFile
    (toJsonString txOut)
    (goldenPath </> "conway" </> "txout-simple.json")
 where
  txOut :: TxOut CtxTx ConwayEra
  txOut =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkLovelaceValue ShelleyBasedEraConway (L.Coin 1_000_000))
      TxOutDatumNone
      ReferenceScriptNone

tasty_golden_TxOut_ConwayEra_datumHash :: UnitIO ()
tasty_golden_TxOut_ConwayEra_datumHash =
  H.diffVsGoldenFile
    (toJsonString txOut)
    (goldenPath </> "conway" </> "txout-datumhash.json")
 where
  txOut :: TxOut CtxTx ConwayEra
  txOut =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkLovelaceValue ShelleyBasedEraConway (L.Coin 2_000_000))
      (TxOutDatumHash AlonzoEraOnwardsConway testDatumHash)
      ReferenceScriptNone

tasty_golden_TxOut_ConwayEra_inlineDatum :: UnitIO ()
tasty_golden_TxOut_ConwayEra_inlineDatum =
  H.diffVsGoldenFile
    (toJsonString txOut)
    (goldenPath </> "conway" </> "txout-inlinedatum.json")
 where
  txOut :: TxOut CtxTx ConwayEra
  txOut =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkLovelaceValue ShelleyBasedEraConway (L.Coin 3_000_000))
      (TxOutDatumInline BabbageEraOnwardsConway testScriptData)
      ReferenceScriptNone

tasty_golden_TxOut_ConwayEra_referenceScript :: UnitIO ()
tasty_golden_TxOut_ConwayEra_referenceScript =
  H.diffVsGoldenFile
    (toJsonString txOut)
    (goldenPath </> "conway" </> "txout-referencescript.json")
 where
  txOut :: TxOut CtxTx ConwayEra
  txOut =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkLovelaceValue ShelleyBasedEraConway (L.Coin 4_000_000))
      TxOutDatumNone
      (mkTestReferenceScript ShelleyBasedEraConway)

tasty_golden_TxOut_ConwayEra_full :: UnitIO ()
tasty_golden_TxOut_ConwayEra_full =
  H.diffVsGoldenFile
    (toJsonString txOut)
    (goldenPath </> "conway" </> "txout-full.json")
 where
  txOut :: TxOut CtxTx ConwayEra
  txOut =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkMultiAssetValue ShelleyBasedEraConway)
      (TxOutDatumInline BabbageEraOnwardsConway testScriptData)
      (mkTestReferenceScript ShelleyBasedEraConway)

-- -----------------------------------------------------------------------------
-- TxOutValue golden tests
-- -----------------------------------------------------------------------------

tasty_golden_TxOutValue_ConwayEra_lovelaceOnly :: UnitIO ()
tasty_golden_TxOutValue_ConwayEra_lovelaceOnly =
  H.diffVsGoldenFile
    (toJsonString txOutValue)
    (goldenPath </> "conway" </> "txoutvalue-lovelace.json")
 where
  txOutValue :: TxOutValue ConwayEra
  txOutValue = mkLovelaceValue ShelleyBasedEraConway (L.Coin 5_000_000)

tasty_golden_TxOutValue_ConwayEra_multiAsset :: UnitIO ()
tasty_golden_TxOutValue_ConwayEra_multiAsset =
  H.diffVsGoldenFile
    (toJsonString txOutValue)
    (goldenPath </> "conway" </> "txoutvalue-multiasset.json")
 where
  txOutValue :: TxOutValue ConwayEra
  txOutValue = mkMultiAssetValue ShelleyBasedEraConway

-- -----------------------------------------------------------------------------
-- AddressInEra golden tests
-- -----------------------------------------------------------------------------

tasty_golden_AddressInEra_ConwayEra :: UnitIO ()
tasty_golden_AddressInEra_ConwayEra =
  H.diffVsGoldenFile
    (toJsonString addr)
    (goldenPath </> "conway" </> "address.json")
 where
  addr :: AddressInEra ConwayEra
  addr = mkTestAddress ShelleyBasedEraConway

-- -----------------------------------------------------------------------------
-- ReferenceScript golden tests
-- -----------------------------------------------------------------------------

tasty_golden_ReferenceScript_ConwayEra :: UnitIO ()
tasty_golden_ReferenceScript_ConwayEra =
  H.diffVsGoldenFile
    (toJsonString refScript)
    (goldenPath </> "conway" </> "referencescript.json")
 where
  refScript :: ReferenceScript ConwayEra
  refScript = mkTestReferenceScript ShelleyBasedEraConway

-- -----------------------------------------------------------------------------
-- UTxO golden tests
-- -----------------------------------------------------------------------------

tasty_golden_UTxO_ConwayEra :: UnitIO ()
tasty_golden_UTxO_ConwayEra =
  H.diffVsGoldenFile
    (toJsonString utxo)
    (goldenPath </> "conway" </> "utxo.json")
 where
  utxo :: UTxO ConwayEra
  utxo =
    UTxO $
      fromList
        [ (testTxIn, txOut1)
        , (testTxIn2, txOut2)
        ]

  txOut1 :: TxOut CtxUTxO ConwayEra
  txOut1 =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkLovelaceValue ShelleyBasedEraConway (L.Coin 1_000_000))
      TxOutDatumNone
      ReferenceScriptNone

  txOut2 :: TxOut CtxUTxO ConwayEra
  txOut2 =
    TxOut
      (mkTestAddress ShelleyBasedEraConway)
      (mkMultiAssetValue ShelleyBasedEraConway)
      (TxOutDatumHash AlonzoEraOnwardsConway testDatumHash)
      ReferenceScriptNone

  testTxIn :: TxIn
  testTxIn =
    TxIn testTxId1 (TxIx 0)

  testTxIn2 :: TxIn
  testTxIn2 =
    TxIn testTxId2 (TxIx 1)

  testTxId1 :: TxId
  testTxId1 =
    fromJust $
      hush $
        deserialiseFromRawBytesHex "0000000000000000000000000000000000000000000000000000000000000001"

  testTxId2 :: TxId
  testTxId2 =
    fromJust $
      hush $
        deserialiseFromRawBytesHex "0000000000000000000000000000000000000000000000000000000000000002"
