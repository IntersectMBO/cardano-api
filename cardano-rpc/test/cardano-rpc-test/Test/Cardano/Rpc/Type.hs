{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.Type where

import Cardano.Api.Experimental.Era
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range as H

-- | Test that @BigInt@ protobuf type conversion to/from Integer roundtrips
hprop_roundtrip_bigint :: Property
hprop_roundtrip_bigint = H.property $ do
  int <- forAll genLargeInteger
  let bigInt = inject int

  int' <- utxoRpcBigIntToInteger bigInt
  H.note_ "Check that Integer -> BigInt -> Integer preserves the value"
  int === int'

  let bigInt' = inject int'
  H.note_ "Check that BigInt -> Integer -> BigInt preserves the value"
  bigInt === bigInt'

-- generate integer for each of the BigInt proto type constructors
genLargeInteger :: Gen Integer
genLargeInteger =
  H.choice
    [ H.integral $ H.linearFrom 0 (maxI64 + 1) (2 ^ (128 :: Int)) -- large positive - bigUInt
    , H.integral $ H.linearFrom 0 (-(2 ^ (128 :: Int))) (minI64 - 1) -- large negative - bigNInt
    , H.integral $ H.linearFrom 0 minI64 maxI64 -- within Int64 size - int
    ]
 where
  minI64 = fromIntegral $ minBound @Int64 :: Integer
  maxI64 = fromIntegral $ maxBound @Int64 :: Integer
