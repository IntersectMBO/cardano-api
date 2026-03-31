{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.Type where

import Cardano.Api (BlockNo (..), ChainPoint (..), SlotNo (..))
import Cardano.Api.Experimental.Era
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.BaseTypes (WithOrigin (..))

import RIO

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Test.Gen.Cardano.Api.Typed (genBlockHeaderHash)

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

-- | Test that ChainPoint protobuf message roundtrips, including the timestamp field.
-- Note: @At (BlockNo 0)@ is excluded because it encodes identically to @Origin@.
hprop_roundtrip_chain_point_msg :: Property
hprop_roundtrip_chain_point_msg = H.property $ do
  chainPoint <- forAll genChainPoint
  blockNo <- forAll genWithOriginBlockNo
  -- Generate from Word64 milliseconds directly to stay in range and at ms precision
  timestamp <-
    forAll $
      posixSecondsToUTCTime . (/ 1000) . fromIntegral
        <$> H.word64 (H.linearFrom 0 0 maxBound)

  H.tripping
    (chainPoint, blockNo, timestamp)
    (uncurry3 mkChainPointMsg)
    (first @Either displayException . utxoRpcChainPointMsgToChainPoint)
 where
  uncurry3 f (a, b, c) = f a b c

-- | Generate a 'ChainPoint', avoiding @ChainPoint (SlotNo 0) hash@ which is
-- ambiguous with 'ChainPointAtGenesis' after protobuf encoding (both map to
-- @slot=0, hash=empty@).
genChainPoint :: Gen ChainPoint
genChainPoint =
  H.choice
    [ pure ChainPointAtGenesis
    , ChainPoint . SlotNo <$> H.word64 (H.linear 1 maxBound) <*> genBlockHeaderHash
    ]

-- | Generate a @WithOrigin BlockNo@, excluding @At (BlockNo 0)@ which is
-- ambiguous with 'Origin' after protobuf encoding (both map to @height=0@).
genWithOriginBlockNo :: Gen (WithOrigin BlockNo)
genWithOriginBlockNo =
  H.choice
    [ pure Origin
    , At . BlockNo <$> H.word64 (H.linear 1 maxBound)
    ]

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
