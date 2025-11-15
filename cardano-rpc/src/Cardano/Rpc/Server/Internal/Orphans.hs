{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans where

import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

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

instance Inject (Proto UtxoRpc.RationalNumber) Rational where
  inject r = r ^. #numerator . to fromIntegral % r ^. #denominator . to fromIntegral

-- NB. this clips value in Integer -> Int64/Word64 conversion here
instance Inject Rational (Proto UtxoRpc.RationalNumber) where
  inject r =
    defMessage
      & #numerator .~ fromIntegral (numerator r)
      & #denominator .~ fromIntegral (denominator r)

instance Inject (Proto UtxoRpc.ExUnits) L.ExUnits where
  inject r =
    L.ExUnits
      { L.exUnitsMem = r ^. #memory . to fromIntegral
      , L.exUnitsSteps = r ^. #steps . to fromIntegral
      }

instance Inject L.ExUnits (Proto UtxoRpc.ExUnits) where
  inject L.ExUnits{L.exUnitsMem = mem, L.exUnitsSteps = steps} =
    defMessage
      & #memory .~ fromIntegral mem
      & #steps .~ fromIntegral steps

-- | Note that conversion is not total in the other direction
instance Inject TxIn (Proto UtxoRpc.TxoRef) where
  inject (TxIn txId' (TxIx txIx)) =
    defMessage
      & #hash .~ serialiseToRawBytes txId'
      & #index .~ fromIntegral txIx

instance Message a => Default (Proto a) where
  def = defMessage

-----------
-- Errors
-----------

-- TODO add RIO to cardano-api and move this instance there

instance Error StringException where
  prettyError = prettyException

instance IsString e => MonadFail (Either e) where
  fail = Left . fromString
