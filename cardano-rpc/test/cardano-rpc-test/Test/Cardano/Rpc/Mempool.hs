{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Rpc.Mempool where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronTxToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool (txInModeToTxInMempool)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import Cardano.Chain.Byron.API (reAnnotateUsing)
import Cardano.Chain.MempoolPayload (AMempoolPayload (..))
import Cardano.Crypto qualified as Byron (hashToBytes)
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR (encCBOR))
import Cardano.Ledger.Binary qualified as CBOR
import Ouroboros.Consensus.Byron.Ledger.Mempool qualified as Consensus (fromMempoolPayload)

import RIO hiding (toList)

import Data.ProtoLens (decodeMessage, encodeMessage)
import Network.GRPC.Spec (Proto (..))

import Test.Gen.Cardano.Api.Typed (genTx)

import Test.Cardano.Chain.MempoolPayload.Gen (genMempoolPayload)
import Test.Cardano.Crypto.Gen (genProtocolMagicId)

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | One-way projection property for 'txInModeToTxInMempool' at a
-- Shelley-based era: there is no inverse conversion, so the proto message
-- and the transaction are projected onto comparable facts which must agree.
txInModeToTxInMempoolShelley :: forall era. ShelleyBasedEra era -> Property
txInModeToTxInMempoolShelley sbe = H.withTests 20 . H.property $ anyEraTxConstraints sbe $ do
  tx@(ShelleyTx _ ledgerTx) <- forAll $ genTx sbe
  protoTxInMempool <- H.nothingFail $ txInModeToTxInMempool (TxInMode sbe tx)

  H.note_ "Wire-level protobuf roundtrip, forcing the full message"
  decodeMessage (encodeMessage protoTxInMempool) === Right protoTxInMempool

  H.note_ "The reference is the transaction id"
  protoTxInMempool ^. U5c.ref === serialiseToRawBytes (fromShelleyTxId (L.txIdTx ledgerTx))

  H.note_ "The native bytes are the CBOR-serialised transaction"
  protoTxInMempool ^. U5c.nativeBytes === serialiseToCBOR tx

  H.note_ "The stage is always mempool"
  protoTxInMempool ^. U5c.stage === Proto U5c.STAGE_MEMPOOL

  H.note_ "The cardano field reuses the tx-to-proto conversion"
  protoTxInMempool ^. U5c.cardano === txToUtxoRpcTx ledgerTx

-- | One projection property per Shelley-based era, from the 'Bounded'
-- enumeration of 'AnyShelleyBasedEra'.
test_tx_in_mode_to_tx_in_mempool_shelley :: [TestTree]
test_tx_in_mode_to_tx_in_mempool_shelley =
  [ testPropertyNamed (show sbe) (fromString (show sbe)) $ txInModeToTxInMempoolShelley sbe
  | AnyShelleyBasedEra sbe <- [minBound .. maxBound]
  ]

-- | Coverage for all four Byron mempool payload shapes, generated via
-- cardano-ledger-byron's own 'genMempoolPayload' and re-annotated with real
-- CBOR bytes exactly as ouroboros-consensus's own @Arbitrary (GenTx
-- ByronBlock)@ instance does, so the fixture matches what a node's mempool
-- actually holds. 'MempoolTx' converts to a populated 'UtxoRpc.TxInMempool';
-- the other three (delegation certificate, update proposal, update vote)
-- have no proto @Tx@ representation and convert to 'Nothing'.
hprop_tx_in_mode_to_tx_in_mempool_byron :: Property
hprop_tx_in_mode_to_tx_in_mempool_byron = H.property $ do
  protocolMagicId <- forAll genProtocolMagicId
  payload <- forAll $ genMempoolPayload protocolMagicId
  let reannotatedPayload = reAnnotateUsing encCBOR decCBOR payload
      genTx' = Consensus.fromMempoolPayload reannotatedPayload
      result = txInModeToTxInMempool (TxInByronSpecial genTx')

  case reannotatedPayload of
    MempoolTx aTxAux -> do
      protoTxInMempool <- H.nothingFail result

      H.note_ "Wire-level protobuf roundtrip, forcing the full message"
      decodeMessage (encodeMessage protoTxInMempool) === Right protoTxInMempool

      H.note_ "The reference is the Byron transaction id"
      protoTxInMempool ^. U5c.ref === Byron.hashToBytes (byronIdTx aTxAux)

      H.note_ "The native bytes are the annotated original Byron transaction bytes"
      protoTxInMempool ^. U5c.nativeBytes === CBOR.recoverBytes aTxAux

      H.note_ "The cardano field reuses the Byron tx-to-proto conversion"
      protoTxInMempool ^. U5c.cardano === byronTxToUtxoRpcTx aTxAux

      H.note_ "The stage is always mempool"
      protoTxInMempool ^. U5c.stage === Proto U5c.STAGE_MEMPOOL
    MempoolDlg{} -> result === Nothing
    MempoolUpdateProposal{} -> result === Nothing
    MempoolUpdateVote{} -> result === Nothing
