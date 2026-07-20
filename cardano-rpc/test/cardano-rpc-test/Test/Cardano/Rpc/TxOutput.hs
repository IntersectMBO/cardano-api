{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.TxOutput where

import Cardano.Api.Experimental.Era
import Cardano.Api.Plutus (hashScriptDataBytes)
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import RIO

import Test.Gen.Cardano.Api.Typed
  ( genTxOutUTxOContext
  )

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

era :: Era ConwayEra
era = ConwayEra

-- | Test if TxOut in UTXO context does roundtrip
hprop_roundtrip_tx_output :: Property
hprop_roundtrip_tx_output = H.property $ do
  txOut <- forAll $ genTxOutUTxOContext (convert era)

  H.tripping
    txOut
    (txOutToUtxoRpcTxOutput (convert era))
    (first @Either displayException . utxoRpcTxOutputToTxOut)

-- | Test that TxOutput fields carry raw bytes on the wire
hprop_tx_output_wire_format :: Property
hprop_tx_output_wire_format = H.property $ do
  txOut@(TxOut addressInEra _ datum _) <- forAll $ genTxOutUTxOContext (convert era)

  let protoTxOutput = txOutToUtxoRpcTxOutput (convert era) txOut

  H.note_ "Address field carries raw ledger address bytes"
  protoTxOutput ^. U5c.address === serialiseToRawBytes addressInEra

  case datum of
    TxOutDatumNone -> pure ()
    TxOutDatumHash _ scriptDataHash -> do
      H.note_ "Datum hash field carries the raw script data hash"
      protoTxOutput ^. U5c.datum . U5c.hash === serialiseToRawBytes scriptDataHash
    TxOutDatumInline _ hashableScriptData -> do
      H.note_ "Inline datum hash field carries the datum hash, not the datum CBOR"
      protoTxOutput ^. U5c.datum . U5c.hash
        === serialiseToRawBytes (hashScriptDataBytes hashableScriptData)
