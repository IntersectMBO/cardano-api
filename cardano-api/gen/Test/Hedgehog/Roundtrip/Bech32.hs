module Test.Hedgehog.Roundtrip.Bech32
  ( roundtrip_Bech32
  )
where

import Cardano.Api

import Hedgehog (Gen, Property)
import Hedgehog qualified as H

roundtrip_Bech32
  :: (SerialiseAsBech32 a, Eq a, Show a)
  => Gen a -> Property
roundtrip_Bech32 gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToBech32 deserialiseFromBech32
