{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Conversion of a mempool transaction to the UTxO RPC @TxInMempool@
-- message.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool
  ( txInModeToTxInMempool
  , txInModeToTxInMempoolFields
  , TxInMempoolFields (..)
  , mkWatchMempoolResponse
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as Query
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.FieldMask
  ( FieldMaskTree (..)
  , applyFieldMask
  , maskedSet
  , subMask
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronTxToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import Cardano.Crypto qualified as Byron (hashToBytes)
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Per-transaction inputs of 'mkWatchMempoolResponse'. The fields are lazy:
-- a field the mask does not select is never computed.
data TxInMempoolFields = TxInMempoolFields
  { txInMempoolFieldsRef :: ByteString
  , txInMempoolFieldsNativeBytes :: ByteString
  -- ^ Lazy: computed only when the mask or the predicate needs it.
  , txInMempoolFieldsCardano :: Query.Tx
  -- ^ Lazy: computed only when the mask or the predicate needs it.
  }

-- | Convert a transaction read from the mempool to the lazy per-field inputs
-- of the UTxO RPC 'UtxoRpc.TxInMempool' message.
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
txInModeToTxInMempoolFields :: TxInMode -> Maybe TxInMempoolFields
txInModeToTxInMempoolFields = \case
  TxInMode sbe tx@(ShelleyTx _ ledgerTx) ->
    Just $
      anyEraTxConstraints sbe $
        TxInMempoolFields
          { txInMempoolFieldsRef = serialiseToRawBytes (fromShelleyTxId (L.txIdTx ledgerTx))
          , txInMempoolFieldsNativeBytes = serialiseToCBOR tx
          , txInMempoolFieldsCardano = getProto $ txToUtxoRpcTx ledgerTx
          }
  TxInByronSpecial genTx -> case genTx of
    ByronTx byronTxId aTxAux ->
      Just $
        TxInMempoolFields
          { txInMempoolFieldsRef = Byron.hashToBytes byronTxId
          , txInMempoolFieldsNativeBytes = CBOR.recoverBytes aTxAux
          , txInMempoolFieldsCardano = getProto $ byronTxToUtxoRpcTx aTxAux
          }
    ByronDlg{} -> Nothing
    ByronUpdateProposal{} -> Nothing
    ByronUpdateVote{} -> Nothing

-- | Convert a transaction read from the mempool to the full UTxO RPC
-- 'UtxoRpc.TxInMempool' message, always at 'U5c.STAGE_MEMPOOL'.
txInModeToTxInMempool :: TxInMode -> Maybe (Proto UtxoRpc.TxInMempool)
txInModeToTxInMempool txInMode = do
  fields <- txInModeToTxInMempoolFields txInMode
  pure . Proto $ mkTxInMempool SelectAll fields

-- | Assemble the response, then trim it to the mask. An input the mask does not
-- select is never evaluated.
mkWatchMempoolResponse
  :: FieldMaskTree UtxoRpc.WatchMempoolResponse
  -> TxInMempoolFields
  -> UtxoRpc.WatchMempoolResponse
mkWatchMempoolResponse fieldMask fields =
  applyFieldMask fieldMask $
    defMessage & maskedSet @"tx" fieldMask (mkTxInMempool (subMask @"tx" fieldMask) fields)

-- | Build the message from the fields the mask selects. An unselected field is never evaluated.
mkTxInMempool :: FieldMaskTree UtxoRpc.TxInMempool -> TxInMempoolFields -> UtxoRpc.TxInMempool
mkTxInMempool fieldMask fields =
  defMessage
    & maskedSet @"ref" fieldMask (txInMempoolFieldsRef fields)
    & maskedSet @"nativeBytes" fieldMask (txInMempoolFieldsNativeBytes fields)
    & maskedSet @"stage" fieldMask U5c.STAGE_MEMPOOL
    & maskedSet @"cardano" fieldMask (txInMempoolFieldsCardano fields)
