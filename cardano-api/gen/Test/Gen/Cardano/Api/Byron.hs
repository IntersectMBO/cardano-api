{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Gen.Cardano.Api.Byron
  ( tests
  ) where

import           Cardano.Api hiding (txIns)
import           Cardano.Api.Byron

import           Data.Proxy

import           Test.Gen.Cardano.Api.Typed

import           Hedgehog
import           Test.Hedgehog.Roundtrip.CBOR
import           Test.Tasty
import           Test.Tasty.Hedgehog

prop_byron_roundtrip_txbody_CBOR :: Property
prop_byron_roundtrip_txbody_CBOR = property $ do
  let byron = ByronEra
  x <- forAll $ makeSignedByronTransaction [] <$> genTxBodyByron
  tripping (ByronTx ByronEraOnlyByron x) (serialiseTxLedgerCddl byron) deserialiseByronTxCddl


prop_byron_roundtrip_tx_CBOR :: Property
prop_byron_roundtrip_tx_CBOR =  property $ do
  let byron = ByronEra
  x <- forAll genTxByron
  cardanoEraConstraints byron $ trippingCbor (proxyToAsType Proxy) x


prop_byron_roundtrip_witness_CBOR :: Property
prop_byron_roundtrip_witness_CBOR = property $ do
  let byron = ByronEra
  x <- forAll genByronKeyWitness
  cardanoEraConstraints byron $ trippingCbor (AsKeyWitness (proxyToAsType Proxy)) x


prop_byron_roundtrip_Tx_Cddl :: Property
prop_byron_roundtrip_Tx_Cddl = property $ do
  let byron = ByronEra
  x <- forAll genTxByron
  tripping x (serialiseTxLedgerCddl byron) deserialiseByronTxCddl

tests :: TestTree
tests = testGroup "Test.Gen.Cardano.Api.Byron"
  [ testProperty "Byron roundtrip txbody CBOR"         prop_byron_roundtrip_txbody_CBOR
  , testProperty "Byron roundtrip tx certificate CBOR" prop_byron_roundtrip_tx_CBOR
  , testProperty "Byron roundtrip witness CBOR"        prop_byron_roundtrip_witness_CBOR
  , testProperty "Byron roundtrip tx CBOR"             prop_byron_roundtrip_Tx_Cddl
  ]

