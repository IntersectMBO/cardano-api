{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Conversion of a mempool transaction to the UTxO RPC @TxInMempool@
-- message, and field-mask-aware building of it from a lazily computed set
-- of per-transaction fields.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool
  ( txInModeToTxInMempool
  , txInModeToTxInMempoolFields
  , TxInMempoolFields (..)
  , txInMempoolMaskTable
  , buildTxInMempool
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronTxToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx (anyEraTxConstraints, txToUtxoRpcTx)

import Cardano.Crypto qualified as Byron (hashToBytes)
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Per-transaction inputs to 'buildTxInMempool', one field per top-level
-- 'U5c.TxInMempool' field. Fields are deliberately non-strict: a field
-- excluded by a field mask must never be computed.
data TxInMempoolFields = TxInMempoolFields
  { txInMempoolFieldsRef :: ByteString
  , txInMempoolFieldsNativeBytes :: ByteString
  , txInMempoolFieldsCardano :: Proto UtxoRpc.Tx
  }

-- | Convert a transaction read from the mempool into the per-field inputs
-- of a UTxO RPC 'U5c.TxInMempool' message (see 'buildTxInMempool').
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
          , txInMempoolFieldsCardano = txToUtxoRpcTx ledgerTx
          }
  TxInByronSpecial genTx -> case genTx of
    ByronTx byronTxId aTxAux ->
      Just
        TxInMempoolFields
          { txInMempoolFieldsRef = Byron.hashToBytes byronTxId
          , txInMempoolFieldsNativeBytes = CBOR.recoverBytes aTxAux
          , txInMempoolFieldsCardano = byronTxToUtxoRpcTx aTxAux
          }
    ByronDlg{} -> Nothing
    ByronUpdateProposal{} -> Nothing
    ByronUpdateVote{} -> Nothing

-- | Convert a transaction read from the mempool to the full UTxO RPC
-- 'U5c.TxInMempool' message, always at 'U5c.STAGE_MEMPOOL'. See
-- 'txInModeToTxInMempoolFields' for the per-field conversion and its
-- fidelity caveats.
txInModeToTxInMempool :: TxInMode -> Maybe (Proto U5c.TxInMempool)
txInModeToTxInMempool = fmap (buildTxInMempool []) . txInModeToTxInMempoolFields

-- | Field-mask table for 'U5c.TxInMempool': proto field name paired with a
-- builder that sets that field on a message from 'TxInMempoolFields'. A
-- tripwire test asserts the names stay in sync with the generated proto
-- descriptors.
txInMempoolMaskTable
  :: [(Text, TxInMempoolFields -> Proto U5c.TxInMempool -> Proto U5c.TxInMempool)]
txInMempoolMaskTable =
  [ ("ref", \fields -> U5c.ref .~ txInMempoolFieldsRef fields)
  , ("native_bytes", \fields -> U5c.nativeBytes .~ txInMempoolFieldsNativeBytes fields)
  , ("stage", \_ -> U5c.stage .~ Proto U5c.STAGE_MEMPOOL)
  , ("cardano", \fields -> U5c.cardano .~ txInMempoolFieldsCardano fields)
  ]

-- | Build a 'U5c.TxInMempool' message from field-mask paths and
-- 'TxInMempoolFields', computing only the fields the mask selects - an
-- excluded field's 'TxInMempoolFields' thunk is never forced. An empty mask
-- (matching an absent field mask) selects every field. There is no
-- established field-mask convention elsewhere in cardano-rpc to follow (the
-- only prior art, 'Cardano.Rpc.Server.Internal.UtxoRpc.Query.readParamsMethod',
-- ignores its field mask outright), so this deliberately does not attempt
-- nested paths (e.g. into the @cardano@ payload) - an unrecognised or
-- nested path simply contributes nothing.
buildTxInMempool :: [Text] -> TxInMempoolFields -> Proto U5c.TxInMempool
buildTxInMempool paths fields = foldl' apply defMessage txInMempoolMaskTable
 where
  apply acc (name, builder)
    | null paths || name `elem` paths = builder fields acc
    | otherwise = acc
