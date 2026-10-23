{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handler for the @WatchMempool@ SubmitService RPC method: stream
-- mempool transactions matching a predicate as they enter the mempool.
module Cardano.Rpc.Server.Internal.UtxoRpc.Mempool
  ( watchMempoolMethod
  , watchMempoolStream
  )
where

import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask (FieldMaskTree, fillFrom, resolve, runFillPure)
import Cardano.Rpc.Server.Internal.Monad (MonadRpc)
import Cardano.Rpc.Server.Internal.UtxoRpc.Predicate (matchesTxPredicate)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool
  ( TxInMempoolFields (..)
  , txInModeToTxInMempoolFields
  , watchMempoolFill
  )
import Cardano.Rpc.Server.NodeKernelAccess
  ( MempoolWatchSnapshot (..)
  , grabNodeKernelAccess
  , nextMempoolWatchSnapshot
  , watchMempoolSnapshot
  )

import Ouroboros.Consensus.Mempool.API qualified as Consensus (TicketNo, zeroTicketNo)

import RIO

import Network.GRPC.Spec (NextElem (NextElem), Proto (..), getProto)

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

-- | Handle the @WatchMempool@ SubmitService RPC method.
--
-- Streams new mempool entries matching the request's predicate, each with
-- 'U5c.stage' always @STAGE_MEMPOOL@ (the first locally observable stage;
-- see 'Cardano.Rpc.Server.Internal.UtxoRpc.Predicate.matchesTxPredicate').
--
-- The field mask is resolved once, before the stream starts; paths start at
-- the response message, for example @tx.native_bytes@.
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
  let fieldMask = resolve $ getProto request ^. U5c.fieldMask . FieldMask.paths
  watchMempoolStream
    (watchMempoolSnapshot nodeKernelAccess)
    (nextMempoolWatchSnapshot nodeKernelAccess)
    (request ^. U5c.predicate)
    fieldMask
    send

-- | The @WatchMempool@ streaming loop. Emits every mempool entry newer than
-- the last one seen, oldest first, filtered by the predicate and trimmed to
-- the field mask.
--
-- Removals produce no message: the proto has no representation for eviction.
watchMempoolStream
  :: forall m
   . MonadIO m
  => m MempoolWatchSnapshot
  -- ^ Read the current mempool watch snapshot, without blocking - used
  -- once, for the initial baseline
  -> (Consensus.TicketNo -> m MempoolWatchSnapshot)
  -- ^ Block until an entry newer than the given ticket exists, then return
  -- the snapshot
  -> Proto U5c.TxPredicate
  -- ^ Predicate filtering which new entries are sent
  -> FieldMaskTree U5c.WatchMempoolResponse
  -- ^ Field mask resolved once for the whole stream
  -> (NextElem (Proto U5c.WatchMempoolResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
watchMempoolStream doReadSnapshot nextSnapshot predicate fieldMask send = do
  initial <- doReadSnapshot
  go Consensus.zeroTicketNo initial
 where
  go :: Consensus.TicketNo -> MempoolWatchSnapshot -> m ()
  go lastSeenTicket MempoolWatchSnapshot{mempoolWatchTxsAfter = txsAfter} = do
    let newEntries = txsAfter lastSeenTicket
    forM_ newEntries $ \(txInMode, _ticketNo) ->
      forM_ (txInModeToTxInMempoolFields txInMode) $ \fields ->
        when (matchesTxPredicate predicate . Proto $ txInMempoolFieldsCardano fields) $
          liftIO . send . NextElem . Proto . runFillPure fieldMask $
            fillFrom fields watchMempoolFill
    let lastSeenTicket' = case newEntries of
          [] -> lastSeenTicket
          _ -> snd (last newEntries)
    nextSnapshot lastSeenTicket' >>= go lastSeenTicket'
