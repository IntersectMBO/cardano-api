{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handler for the @WatchMempool@ SubmitService RPC method: stream
-- mempool transactions matching a predicate as they enter the mempool.
module Cardano.Rpc.Server.Internal.UtxoRpc.Mempool
  ( watchMempoolMethod
  , watchMempoolStream
  , txInMempoolMaskTable
  )
where

import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.Monad (MonadRpc)
import Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (matchesTxPredicate)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool (txInModeToTxInMempool)
import Cardano.Rpc.Server.NodeKernelAccess
  ( MempoolWatchSnapshot (..)
  , grabNodeKernelAccess
  , nextMempoolWatchSnapshot
  , watchMempoolSnapshot
  )

import Ouroboros.Consensus.Mempool.API qualified as Consensus (TicketNo, zeroTicketNo)

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec (NextElem (NextElem), Proto)

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

-- | Handle the @WatchMempool@ SubmitService RPC method.
--
-- Streams new mempool entries matching the request's predicate, each with
-- 'U5c.stage' always @STAGE_MEMPOOL@ (the first locally observable stage;
-- see 'Cardano.Rpc.Server.Internal.UtxoRpc.Predicate.matchesTxPredicate').
--
-- Runs until the client disconnects or the stream is otherwise closed.
watchMempoolMethod
  :: MonadRpc e m
  => Proto U5c.WatchMempoolRequest
  -- ^ Request containing a filter predicate and an optional field mask
  -> (NextElem (Proto U5c.WatchMempoolResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
watchMempoolMethod request send = do
  nodeKernelAccess <- grabNodeKernelAccess
  watchMempoolStream
    (watchMempoolSnapshot nodeKernelAccess)
    (nextMempoolWatchSnapshot nodeKernelAccess)
    (request ^. U5c.predicate)
    (request ^. U5c.fieldMask . FieldMask.paths)
    send

-- | The @WatchMempool@ streaming loop. Emits every mempool entry newer than
-- the last one seen, oldest first, filtered by the predicate and pruned by
-- the field mask.
--
-- The collaborators are plain arguments rather than a 'NodeKernelAccess' so
-- tests can drive the loop with scripted snapshots, no live mempool
-- required (see @Test.Cardano.Rpc.WatchMempoolStream@, mirroring
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Sync.followTipStream').
--
-- Removals and slot-only changes (no new ticket) produce no message: the
-- proto has no representation for eviction, and a max-ticket comparison
-- upstream would otherwise miss exactly these transitions (see
-- 'Cardano.Rpc.Server.NodeKernelAccess.nextMempoolWatchSnapshot').
watchMempoolStream
  :: forall m
   . MonadIO m
  => m MempoolWatchSnapshot
  -- ^ Read the current mempool watch snapshot, without blocking - used
  -- once, for the initial baseline
  -> (MempoolWatchSnapshot -> m MempoolWatchSnapshot)
  -- ^ Block until the snapshot differs from the given one, then return the
  -- new one
  -> Proto U5c.TxPredicate
  -- ^ Predicate filtering which new entries are sent
  -> [Text]
  -- ^ Requested field mask paths, naming top-level 'U5c.TxInMempool'
  -- fields; empty means no pruning
  -> (NextElem (Proto U5c.WatchMempoolResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
watchMempoolStream readSnapshot nextSnapshot predicate fieldMaskPaths send = do
  initial <- readSnapshot
  go Consensus.zeroTicketNo initial
 where
  go :: Consensus.TicketNo -> MempoolWatchSnapshot -> m ()
  go lastSeenTicket snapshot@MempoolWatchSnapshot{mempoolWatchTxsAfter = txsAfter} = do
    let newEntries = txsAfter lastSeenTicket
    forM_ newEntries $ \(txInMode, _ticketNo) ->
      forM_ (txInModeToTxInMempool txInMode) $ \txInMempool ->
        when (matchesTxPredicate predicate (txInMempool ^. U5c.cardano)) $
          liftIO . send . NextElem $
            defMessage & U5c.tx .~ pruneTxInMempool fieldMaskPaths txInMempool
    let lastSeenTicket' = case newEntries of
          [] -> lastSeenTicket
          _ -> snd (last newEntries)
    nextSnapshot snapshot >>= go lastSeenTicket'

-- | Field-mask table for 'U5c.TxInMempool': proto field name paired with a
-- copier for that field. A tripwire test asserts the names stay in sync
-- with the generated proto descriptors.
txInMempoolMaskTable
  :: [(Text, Proto U5c.TxInMempool -> Proto U5c.TxInMempool -> Proto U5c.TxInMempool)]
txInMempoolMaskTable =
  [ ("ref", \tx -> U5c.ref .~ tx ^. U5c.ref)
  , ("native_bytes", \tx -> U5c.nativeBytes .~ tx ^. U5c.nativeBytes)
  , ("stage", \tx -> U5c.stage .~ tx ^. U5c.stage)
  , ("cardano", \tx -> U5c.cardano .~ tx ^. U5c.cardano)
  ]

-- | Minimal field-mask pruning for 'U5c.TxInMempool': keeps only the
-- top-level fields named in @paths@, or the whole message when @paths@ is
-- empty (matching an absent field mask). There is no established
-- field-mask convention elsewhere in cardano-rpc to follow (the only prior
-- art, 'Cardano.Rpc.Server.Internal.UtxoRpc.Query.readParamsMethod',
-- ignores its field mask outright), so this deliberately does not attempt
-- nested paths (e.g. into the @cardano@ payload) - an unrecognised or
-- nested path simply contributes nothing.
pruneTxInMempool :: [Text] -> Proto U5c.TxInMempool -> Proto U5c.TxInMempool
pruneTxInMempool paths tx
  | null paths = tx
  | otherwise = foldl' apply defMessage txInMempoolMaskTable
 where
  apply acc (name, copier)
    | name `elem` paths = copier tx acc
    | otherwise = acc
