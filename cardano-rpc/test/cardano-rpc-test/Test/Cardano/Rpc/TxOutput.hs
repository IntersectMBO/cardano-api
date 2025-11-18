{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.TxOutput where

import Cardano.Api.Experimental.Era
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO

import Test.Gen.Cardano.Api.Typed
  ( genTxOutUTxOContext
  )

import Hedgehog
import Hedgehog qualified as H

-- | Test if TxOut in UTXO context does roundtrip
hprop_roundtrip_tx_output :: Property
hprop_roundtrip_tx_output = H.property $ do
  let era = ConwayEra

  txOut <- forAll $ genTxOutUTxOContext (convert era)

  H.tripping
    txOut
    txOutToUtxoRpcTxOutput
    (first @Either displayException . utxoRpcTxOutputToTxOut)
