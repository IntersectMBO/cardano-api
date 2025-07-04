{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans () where

import Cardano.Api.Block (ChainPoint (..), Hash (..), SlotNo (..))
import Cardano.Api.Era (Inject (..))
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import Cardano.Ledger.Plutus qualified as L

import RIO

import Data.ByteString.Short qualified as SBS
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

instance Inject ChainPoint (Proto UtxoRpc.ChainPoint) where
  inject chainPoint = do
    let (slotNo, blockHash) =
          case chainPoint of
            ChainPointAtGenesis -> (0, mempty)
            ChainPoint (SlotNo slot) (HeaderHash hash) -> (slot, SBS.fromShort hash)
    defMessage
      & #slot .~ slotNo
      & #hash .~ blockHash
