{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.TxOutput where

import Cardano.Api.Experimental.Era
import Cardano.Api.ProtocolParameters
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus qualified as L

import RIO

import Data.Bits
import Data.Map.Strict qualified as M
import Data.Ratio
import GHC.IsList

import Test.Gen.Cardano.Api.Typed (genValidProtocolParameters)

import Hedgehog
import Hedgehog qualified as H

-- | Test if protocol parameters roundtrip between ledger and proto representation
hprop_roundtrip_tx_output :: Property
hprop_roundtrip_tx_output = H.property $ do
  pure ()
