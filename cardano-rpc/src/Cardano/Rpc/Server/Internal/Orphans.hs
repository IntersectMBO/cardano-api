{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans () where

import Cardano.Api.Era (Inject (..))

import Cardano.Ledger.Plutus qualified as L
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import RIO

import Data.ProtoLens (defMessage)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Network.GRPC.Spec

instance Inject (Proto UtxoRpc.RationalNumber) (Ratio Integer) where
  inject r = r ^. #numerator . to fromIntegral % r ^. #denominator . to fromIntegral

instance Inject (Ratio Integer) (Proto UtxoRpc.RationalNumber) where
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
