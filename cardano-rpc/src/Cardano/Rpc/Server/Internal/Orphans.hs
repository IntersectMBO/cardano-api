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

-- x = U5c.numerator :: _

instance Inject (Proto U5c.RationalNumber) Rational where
  inject r = r ^. U5c.numerator . to fromIntegral % r ^. U5c.denominator . to fromIntegral

-- NB. this clips value in Integer -> Int64/Word64 conversion here
instance Inject Rational (Proto U5c.RationalNumber) where
  inject r =
    defMessage
      & U5c.numerator .~ fromIntegral (numerator r)
      & U5c.denominator .~ fromIntegral (denominator r)

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
