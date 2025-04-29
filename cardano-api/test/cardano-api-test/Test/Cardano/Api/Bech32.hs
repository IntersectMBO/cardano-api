module Test.Cardano.Api.Bech32
  ( tests
  )
where

import Test.Gen.Cardano.Api.Typed (genAddressShelley, genStakeAddress)

import Hedgehog (Property)
import Test.Hedgehog.Roundtrip.Bech32 (roundtrip_Bech32)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_roundtrip_Address_Shelley :: Property
prop_roundtrip_Address_Shelley = roundtrip_Bech32 genAddressShelley

prop_roundtrip_StakeAddress :: Property
prop_roundtrip_StakeAddress = roundtrip_Bech32 genStakeAddress

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.Bech32"
    [ testProperty "roundtrip Address Shelley" prop_roundtrip_Address_Shelley
    , testProperty "roundtrip StakeAddress" prop_roundtrip_StakeAddress
    ]
