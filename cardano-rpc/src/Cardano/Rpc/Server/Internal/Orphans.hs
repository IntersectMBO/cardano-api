{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans where

import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c

import RIO hiding (toList)

import Data.Default
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Message (Message)
import Data.Ratio (denominator, numerator, (%))
import Network.GRPC.Spec

---------------
-- Conversion
---------------

-- It's easier to use 'Proto a' wrappers for RPC types, because it makes lens automatically available.

instance Inject (Proto U5c.RationalNumber) Rational where
  inject r = r ^. U5c.numerator . to fromIntegral % r ^. U5c.denominator . to fromIntegral

-- | Convert a 'Rational' into a protobuf 'U5c.RationalNumber'.
--
-- The UTxO RPC spec fixes the field widths to an @int32@ numerator and a
-- @uint32@ denominator, while on-chain rationals (e.g. bounded ratios backed
-- by 'Word64') can exceed both. When the numerator and the denominator fit
-- their fields, the conversion is exact. Otherwise the result is the best
-- representable approximation of the value: magnitudes beyond
-- @maxBound \@Int32@ are clamped to it, so out-of-range negative values
-- clamp to @-maxBound \@Int32@ (one above @minBound \@Int32@), and other
-- values are approximated using continued fractions, keeping both
-- components within their field bounds.
-- The conversion is therefore lossy for such out-of-range values, and values
-- close enough to zero may approximate to zero.
instance Inject Rational (Proto U5c.RationalNumber) where
  inject r =
    defMessage
      & U5c.numerator .~ fromInteger num
      & U5c.denominator .~ fromInteger den
   where
    (num, den)
      | numerator r >= fromIntegral (minBound @Int32)
      , numerator r <= maxNum
      , denominator r <= maxDen =
          (numerator r, denominator r)
      | otherwise = do
          let approximation = bestApproximation $ abs r
          (signum (numerator r) * numerator approximation, denominator approximation)

    maxNum = fromIntegral (maxBound @Int32) :: Integer

    maxDen = fromIntegral (maxBound @Word32) :: Integer

    -- Best approximation of a non-negative rational keeping the numerator and
    -- the denominator within 'maxNum' and 'maxDen' respectively: walk the
    -- continued fraction convergents of the value until a bound is exceeded,
    -- then pick the closer of the last in-bounds convergent and the largest
    -- in-bounds semiconvergent.
    bestApproximation :: Rational -> Rational
    bestApproximation x
      | x > fromIntegral maxNum = maxNum % 1
      | otherwise = go 0 1 1 0 x
     where
      -- (hPrev, kPrev) and (hCur, kCur) are the components of the two most
      -- recent convergents, y is the current complete quotient. The first
      -- convergent (floor x % 1) is always in bounds because x <= maxNum, so
      -- the out-of-bounds branch never divides by kCur = 0.
      go :: Integer -> Integer -> Integer -> Integer -> Rational -> Rational
      go hPrev kPrev hCur kCur y = do
        let a = floor y
            hNew = a * hCur + hPrev
            kNew = a * kCur + kPrev
            rest = y - fromIntegral a
        if hNew > maxNum || kNew > maxDen
          then do
            -- the largest coefficient for which the semiconvergent still fits
            -- both bounds
            let aFromNum = if hCur > 0 then (maxNum - hPrev) `div` hCur else a
                aMax = a `min` aFromNum `min` ((maxDen - kPrev) `div` kCur)
                semiconvergent = (aMax * hCur + hPrev) % (aMax * kCur + kPrev)
                convergent = hCur % kCur
            if aMax >= 1 && abs (x - semiconvergent) <= abs (x - convergent)
              then semiconvergent
              else convergent
          else
            if rest == 0
              then hNew % kNew
              else go hCur kCur hNew kNew (recip rest)

instance Inject (Proto U5c.ExUnits) L.ExUnits where
  inject r =
    L.ExUnits
      { L.exUnitsMem = r ^. U5c.memory . to fromIntegral
      , L.exUnitsSteps = r ^. U5c.steps . to fromIntegral
      }

instance Inject L.ExUnits (Proto U5c.ExUnits) where
  inject L.ExUnits{L.exUnitsMem = mem, L.exUnitsSteps = steps} =
    defMessage
      & U5c.memory .~ fromIntegral mem
      & U5c.steps .~ fromIntegral steps

-- | Note that conversion is not total in the other direction
instance Inject TxIn (Proto U5c.TxoRef) where
  inject (TxIn txId' (TxIx txIx)) =
    defMessage
      & U5c.hash .~ serialiseToRawBytes txId'
      & U5c.index .~ fromIntegral txIx

instance Message a => Default (Proto a) where
  def = defMessage

instance Inject Integer (Proto U5c.BigInt) where
  inject int
    | int <= fromIntegral (maxBound @Int64)
        && int >= fromIntegral (minBound @Int64) =
        inject @Int64 $ fromIntegral int
    | int < 0 =
        -- https://www.rfc-editor.org/rfc/rfc8949.html#name-bignums see 3.4.3 for negative integers
        defMessage & U5c.bigNInt .~ serialiseToRawBytes (fromIntegral @_ @Natural (-1 - int))
    | otherwise =
        defMessage & U5c.bigUInt .~ serialiseToRawBytes (fromIntegral @_ @Natural int)

instance Inject Int64 (Proto U5c.BigInt) where
  inject int = defMessage & U5c.int .~ int

instance Inject L.Coin (Proto U5c.BigInt) where
  inject = inject . fromIntegral @_ @Integer

-----------
-- Errors
-----------

-- TODO add RIO to cardano-api and move this instance there

instance Error StringException where
  prettyError = prettyException

instance IsString e => MonadFail (Either e) where
  fail = Left . fromString
