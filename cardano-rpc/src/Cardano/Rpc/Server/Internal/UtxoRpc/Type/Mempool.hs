{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Conversion of a mempool transaction to the UTxO RPC @TxInMempool@
-- message.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool
  ( txInModeToTxInMempool
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronTxToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import Cardano.Crypto qualified as Byron (hashToBytes)
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Convert a transaction read from the mempool to the UTxO RPC
-- 'UtxoRpc.TxInMempool' message, always at 'U5c.STAGE_MEMPOOL'.
--
-- @native_bytes@ fidelity differs by era: the Shelley-onwards ledger @Tx@ is
-- not 'SafeToHash'-backed, so its @native_bytes@ is a canonical
-- re-encoding, not the submitter's original bytes (same caveat as
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Type.TxOutput.txOutToUtxoRpcTxOutput').
-- The Byron arm has no such caveat: its @ATxAux@ carries a real byte
-- annotation, so 'CBOR.recoverBytes' recovers the true original bytes.
--
-- 'Nothing' for the three Byron special payloads (delegation certificates,
-- update proposals, update votes): they have no proto @Tx@ representation,
-- and cannot occur on any network still running today, since Byron
-- transitioned to Shelley years before any currently live Cardano network
-- started.
txInModeToTxInMempool :: TxInMode -> Maybe (Proto UtxoRpc.TxInMempool)
txInModeToTxInMempool = \case
  TxInMode sbe tx@(ShelleyTx _ ledgerTx) ->
    Just $
      anyEraTxConstraints sbe $
        defMessage
          & U5c.ref .~ serialiseToRawBytes (fromShelleyTxId (L.txIdTx ledgerTx))
          & U5c.nativeBytes .~ serialiseToCBOR tx
          & U5c.stage .~ Proto U5c.STAGE_MEMPOOL
          & U5c.cardano .~ txToUtxoRpcTx ledgerTx
  TxInByronSpecial genTx -> case genTx of
    ByronTx byronTxId aTxAux ->
      Just $
        defMessage
          & U5c.ref .~ Byron.hashToBytes byronTxId
          & U5c.nativeBytes .~ CBOR.recoverBytes aTxAux
          & U5c.stage .~ Proto U5c.STAGE_MEMPOOL
          & U5c.cardano .~ byronTxToUtxoRpcTx aTxAux
    ByronDlg{} -> Nothing
    ByronUpdateProposal{} -> Nothing
    ByronUpdateVote{} -> Nothing
