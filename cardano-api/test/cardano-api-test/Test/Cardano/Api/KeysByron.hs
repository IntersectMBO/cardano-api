{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.KeysByron
  ( tests
  )
where

import Cardano.Api (AsType (AsByronKey, AsSigningKey), Key (deterministicSigningKey))

import Test.Cardano.Api.Orphans ()
import Test.Gen.Cardano.Crypto.Seed qualified as Gen

import Hedgehog (Property)
import Hedgehog qualified as H
import Test.Hedgehog.Roundtrip.CBOR (trippingCbor)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_roundtrip_byron_key_CBOR :: Property
prop_roundtrip_byron_key_CBOR = H.property $ do
  seed <- H.forAll $ deterministicSigningKey AsByronKey <$> Gen.genSeedForKey AsByronKey
  trippingCbor (AsSigningKey AsByronKey) seed

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.KeysByron"
    [ testProperty "roundtrip byron key CBOR" prop_roundtrip_byron_key_CBOR
    ]
