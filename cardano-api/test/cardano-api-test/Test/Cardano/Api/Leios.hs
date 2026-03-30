{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Api.Leios
  ( tests
  )
where

import Cardano.Api
  ( AsType (..)
  , BlsPossessionProof
  , blsPossessionProof
  , createBlsPossessionProof
  , serialiseToRawBytesHex
  )

import Test.Gen.Cardano.Api.Typed (genSigningKey)

import Hedgehog (Property, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Cardano.Api.Leios"
    [ testProperty "prop_show_blsPossessionProof_roundtrip" prop_show_blsPossessionProof_roundtrip
    ]

-- | Property: @show proof == "blsPossessionProof " ++ show (serialiseToRawBytesHex proof)@,
-- and the hex fed to 'blsPossessionProof' reconstructs the same value.
prop_show_blsPossessionProof_roundtrip :: Property
prop_show_blsPossessionProof_roundtrip = property $ do
  sk <- forAll $ genSigningKey AsBlsKey
  let proof :: BlsPossessionProof
      proof = createBlsPossessionProof sk
      hex = serialiseToRawBytesHex proof
      expected = "blsPossessionProof " ++ show hex
  show proof === expected
  blsPossessionProof hex === proof
