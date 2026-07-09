{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.Type where

import Cardano.Api (BlockNo (..), ChainPoint (..), SlotNo (..), getScriptData)
import Cardano.Api.Experimental.Era
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.BaseTypes (WithOrigin (..))

import RIO

import Data.Ratio ((%))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.GRPC.Spec (Proto)

import Test.Gen.Cardano.Api.Typed (genBlockHeaderHash, genHashableScriptData)

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

-- | Rationals whose numerator fits @int32@ and denominator fits @uint32@
-- roundtrip exactly through the proto 'U5c.RationalNumber' representation.
hprop_roundtrip_rational_in_range :: Property
hprop_roundtrip_rational_in_range = H.property $ do
  num <-
    forAll . H.integral $
      H.linearFrom 0 (fromIntegral $ minBound @Int32) (fromIntegral $ maxBound @Int32)
  den <- forAll . H.integral $ H.linear 1 (fromIntegral $ maxBound @Word32)
  let r = num % den :: Rational
      msg = inject r :: Proto U5c.RationalNumber
  inject msg === r

-- | Rationals with components exceeding the fixed proto field widths convert
-- to the best representable approximation.
hprop_rational_conversion_approximates_out_of_range :: Property
hprop_rational_conversion_approximates_out_of_range = H.propertyOnce $ do
  H.note_ "In-range values convert exactly"
  throughProto (3 % 4) === 3 % 4
  throughProto (-(3 % 7)) === (-3) % 7
  throughProto (fromIntegral (maxBound @Int32) % fromIntegral (maxBound @Word32))
    === fromIntegral (maxBound @Int32) % fromIntegral (maxBound @Word32)

  H.note_ "The approximation of 1 % 2^33 carries a valid, non-zero denominator"
  let smallRational = 1 % 2 ^ (33 :: Int) :: Rational
      smallMsg = inject smallRational :: Proto U5c.RationalNumber
  H.assertWith (smallMsg ^. U5c.denominator) (/= 0)
  H.note_ "The approximation stays within 1 % 2^33 of the original value"
  H.assertWith (abs (inject smallMsg - smallRational)) (<= 1 % 2 ^ (33 :: Int))
  H.note_ "0 is the best approximation: it is closer to 1 % 2^33 than 1 % maxBound @Word32 is"
  inject smallMsg === (0 :: Rational)

  H.note_ "1 % 2^32 approximates to the largest representable denominator"
  throughProto (1 % 2 ^ (32 :: Int)) === 1 % (2 ^ (32 :: Int) - 1)

  H.note_ "Values beyond the numerator bounds are clamped to them"
  throughProto (2 ^ (40 :: Int) % 3) === fromIntegral (maxBound @Int32) % 1
  throughProto (-((2 ^ (40 :: Int)) % 3)) === (-(fromIntegral $ maxBound @Int32)) % 1

  H.note_ "Unit-interval-scale values keep sub-2^-32 precision"
  let unitScale = 1234567890123456789 % 9876543210987654321 :: Rational
      unitScaleMsg = inject unitScale :: Proto U5c.RationalNumber
  H.assertWith (unitScaleMsg ^. U5c.denominator) (/= 0)
  H.assertWith (abs (inject unitScaleMsg - unitScale)) (<= 1 % 2 ^ (32 :: Int))
 where
  throughProto :: Rational -> Rational
  throughProto r = inject (inject r :: Proto U5c.RationalNumber)

-- | For arbitrary rationals in the unit interval with components up to
-- 'Word64' scale (like ledger @UnitInterval@ values), the proto approximation
-- never produces a zero denominator and stays within 2^-32 of the original.
hprop_rational_approximation_error_bound :: Property
hprop_rational_approximation_error_bound = H.property $ do
  den <- forAll . H.integral $ H.linear 1 (10 ^ (19 :: Int))
  num <- forAll . H.integral $ H.linear 0 den
  let r = num % den :: Rational
      msg = inject r :: Proto U5c.RationalNumber
  H.assertWith (msg ^. U5c.denominator) (/= 0)
  H.assertWith (abs (inject msg - r)) (<= 1 % 2 ^ (32 :: Int))

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

-- | 'ScriptData' roundtrips through proto 'PlutusData'.
hprop_roundtrip_plutusData :: Property
hprop_roundtrip_plutusData = H.property $ do
  scriptData <- getScriptData <$> forAll genHashableScriptData
  H.tripping
    scriptData
    scriptDataToUtxoRpcPlutusData
    (first @Either displayException . utxoRpcPlutusDataToScriptData)

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
