{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handler for the UTxO RPC @WaitForTx@ SubmitService method: stream stage
-- transitions for a set of requested transactions until every one of them
-- reaches @STAGE_CONFIRMED@.
module Cardano.Rpc.Server.Internal.UtxoRpc.WaitForTx
  ( waitForTxMethod
  , waitForTxStream
  , WaitForTxEvent (..)
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (tipHeaderPoint)
import Cardano.Rpc.Server.NodeKernelAccess

import Cardano.Ledger.Core qualified as L
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Consensus (txForgetValidated)
import Ouroboros.Consensus.Mempool.API qualified as Consensus (MempoolSnapshot (snapshotTxs))

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import Network.GRPC.Spec
  ( GrpcError (GrpcInvalidArgument)
  , NextElem (NextElem, NoNextElem)
  , Proto (Proto)
  )

-- | Handle the @WaitForTx@ SubmitService RPC method.
--
-- Tracks every requested transaction id through two concurrent event
-- sources feeding one bounded queue that this handler drains: a mempool
-- watcher (the same STM change detection @WatchMempool@ uses) that reports
-- @STAGE_MEMPOOL@ the first time a ref is seen in the mempool, and a chain
-- follower ('withFollower', opened at stream start and positioned at the
-- current tip) that reports @STAGE_CONFIRMED@ when a ref appears in an
-- applied block. The stream closes once every ref has reached
-- @STAGE_CONFIRMED@; blocking indefinitely between events is safe here
-- (grapesy runs with the http2 timeout manager disabled).
--
-- __Race contract:__ a transaction that reaches the chain between
-- submission and this call is invisible - it is no longer in the mempool
-- and will not appear in any block applied after the stream opens, so the
-- stream would wait forever on that ref. Clients must open @WaitForTx@
-- before or concurrently with submitting the transaction.
--
-- __Eviction is non-terminal:__ a ref that leaves the mempool without
-- on-chain inclusion produces no message; the stream keeps waiting, since
-- chain churn can legitimately re-add it later.
--
-- __Delivery is deduplicated:__ a ref that is evicted and later re-added to
-- the mempool does not get a second @STAGE_MEMPOOL@.
--
-- Duplicate refs in the request are treated as one. An empty request
-- closes the stream immediately, with no messages sent - vacuously
-- satisfied, and without opening a follower.
--
-- Throws @INVALID_ARGUMENT@ if a ref is not a well-formed transaction id.
waitForTxMethod
  :: MonadRpc e m
  => Proto U5c.WaitForTxRequest
  -- ^ Request containing the transaction ids to wait for
  -> (NextElem (Proto U5c.WaitForTxResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
waitForTxMethod request send = do
  nodeKernelAccess <- grabNodeKernelAccess
  refs <- Set.fromList <$> traverse parseRef (request ^. U5c.ref)
  if Set.null refs
    then liftIO $ send NoNextElem
    else withFollower nodeKernelAccess $ \follower -> do
      positionFollowerAtCurrentTip nodeKernelAccess follower
      queue <- newTBQueueIO waitForTxQueueBound
      withAsync (mempoolProducer nodeKernelAccess queue) $ \_ ->
        withAsync (followerProducer follower queue) $ \_ ->
          waitForTxStream (atomically $ readTBQueue queue) send refs
 where
  -- Comfortably above normal chain/mempool churn between two consumer
  -- wake-ups, so a slow client applies backpressure to the producers
  -- instead of unbounded buffering.
  waitForTxQueueBound = 64

-- | Position a freshly opened chain follower at the current tip (or origin,
-- at genesis), so it reports only blocks applied from now on - consuming
-- 'nextChange' before positioning would replay the entire chain from
-- origin. Passing origin alongside the tip makes the intersection
-- infallible: origin is always on the chain, so a tip that moves between
-- the read and this call still resolves, no retry needed.
positionFollowerAtCurrentTip
  :: MonadIO m
  => NodeKernelAccess
  -> ChainFollower
  -> m ()
positionFollowerAtCurrentTip nodeKernelAccess ChainFollower{findIntersect} = do
  tipHeader <- readChainTipHeader nodeKernelAccess
  let tipPoint = maybe ChainPointAtGenesis tipHeaderPoint tipHeader
  void $ findIntersect [tipPoint, ChainPointAtGenesis]

-- | An event fed into 'waitForTxStream' by its two producers.
data WaitForTxEvent
  = -- | The full set of transaction ids visible in a changed mempool
    -- snapshot, including the initial snapshot read at producer start (so a
    -- ref already in the mempool when the stream opens is not missed).
    MempoolSnapshotChanged (Set TxId)
  | -- | The set of transaction ids confirmed in a newly applied block.
    BlockApplied (Set TxId)
  deriving Show

-- | Feed 'MempoolSnapshotChanged' events: the current snapshot immediately,
-- then again every time it changes ('nextMempoolSnapshot', the same STM
-- change detection @WatchMempool@ uses). Emitting the initial snapshot
-- unconditionally, rather than waiting for the first change, is what makes
-- a transaction already in the mempool when the stream opens visible.
mempoolProducer
  :: MonadIO m
  => NodeKernelAccess
  -> TBQueue WaitForTxEvent
  -> m ()
mempoolProducer nodeKernelAccess queue = do
  initial <- getMempoolSnapshot nodeKernelAccess
  emit initial
  go initial
 where
  emit snapshot = atomically . writeTBQueue queue . MempoolSnapshotChanged $ mempoolTxIds snapshot
  go previous = do
    next <- nextMempoolSnapshot nodeKernelAccess previous
    emit next
    go next

-- | Every transaction id currently visible in a mempool snapshot.
mempoolTxIds
  :: Consensus.MempoolSnapshot (Consensus.CardanoBlock Consensus.StandardCrypto) -> Set TxId
mempoolTxIds snapshot =
  Set.fromList
    [ ref
    | (tx, _ticketNo, _txMeasure) <- Consensus.snapshotTxs snapshot
    , Just ref <- [txInModeTxId . fromConsensusGenTx $ Consensus.txForgetValidated tx]
    ]

-- | Feed 'BlockApplied' events for every block the follower applies.
-- Rollbacks ('ChainRollBack') are ignored: a confirmed-then-rolled-back
-- transaction is not re-announced by @WaitForTx@; @FollowTip@'s @undo@
-- stream is the client-side tool for that.
followerProducer
  :: MonadIO m
  => ChainFollower
  -> TBQueue WaitForTxEvent
  -> m ()
followerProducer ChainFollower{nextChange} queue =
  forever $
    nextChange >>= \case
      ChainApply (_, blockInMode) ->
        atomically . writeTBQueue queue . BlockApplied $ blockTxIds blockInMode
      ChainRollBack _ -> pure ()

-- | Every transaction id confirmed in a block. Byron blocks contribute no
-- ids ('getBlockTxs' returns @[]@ for them: Byron transactions predate
-- cardano-api's 'Tx' entirely, and cannot occur on any network still
-- running today).
--
-- Computed straight off the ledger tx via 'L.txIdTx', the same route
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool.txInModeToTxInMempool'
-- and 'Cardano.Rpc.Server.Internal.UtxoRpc.Submit.submitTxMethod' use -
-- 'getTxId'\/'getTxBody' would work too, but 'getTxBody' is deprecated in
-- favour of the experimental API.
blockTxIds :: BlockInMode -> Set TxId
blockTxIds (BlockInMode _ block) = case block of
  ByronBlock{} -> Set.empty
  ShelleyBlock sbe _ ->
    shelleyBasedEraConstraints sbe $
      Set.fromList
        [fromShelleyTxId (L.txIdTx ledgerTx) | ShelleyTx _ ledgerTx <- getBlockTxs block]

-- | Extract the cardano-api transaction id of a mempool entry, for matching
-- against @WaitForTxRequest@ refs. 'Nothing' for the three Byron special
-- payloads (delegation certificates, update proposals, update votes): they
-- carry no transaction id and, like Byron transactions themselves, cannot
-- occur on any network still running today (see
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool.txInModeToTxInMempool').
txInModeTxId :: TxInMode -> Maybe TxId
txInModeTxId = \case
  TxInMode sbe (ShelleyTx _ ledgerTx) ->
    Just $ shelleyBasedEraConstraints sbe $ fromShelleyTxId (L.txIdTx ledgerTx)
  TxInByronSpecial genTx -> case genTx of
    ByronTx _ aTxAux -> Just $ getTxIdByron aTxAux
    ByronDlg{} -> Nothing
    ByronUpdateProposal{} -> Nothing
    ByronUpdateVote{} -> Nothing

-- | Parse a request ref into a transaction id.
-- Throws @INVALID_ARGUMENT@ if the bytes are not a well-formed transaction id.
parseRef :: MonadRpc e m => ByteString -> m TxId
parseRef bytes =
  either (const throwInvalidRef) pure $ deserialiseFromRawBytes AsTxId bytes
 where
  throwInvalidRef =
    throwGrpcErrorWithMessage GrpcInvalidArgument $
      "invalid transaction id (" <> tshow (BS.length bytes) <> " bytes)"

-- | The per-ref state 'waitForTxStream' tracks across events: refs still
-- awaiting confirmation, and the subset of those that have already
-- surfaced a @STAGE_MEMPOOL@ notification (so an evicted-then-re-added ref
-- does not fire a second one).
data WaitForTxState = WaitForTxState
  { pendingRefs :: !(Set TxId)
  , mempoolNotifiedRefs :: !(Set TxId)
  }

-- | The @WaitForTx@ streaming loop, extracted from 'waitForTxMethod' so it
-- can be driven here with an injected merged event source - no live mempool
-- or ChainDB required (mirrors
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Sync.followTipStream').
--
-- Closes the stream (sends 'NoNextElem') once every requested ref has
-- reached @STAGE_CONFIRMED@; checked before ever blocking on 'nextEvent', so
-- an empty ref set - or a script whose last event confirms the final ref -
-- closes without an extra, unnecessary wait.
waitForTxStream
  :: forall m
   . MonadIO m
  => m WaitForTxEvent
  -- ^ Block for the next merged mempool\/follower event
  -> (NextElem (Proto U5c.WaitForTxResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> Set TxId
  -- ^ Requested transaction ids, already deduplicated
  -> m ()
waitForTxStream nextEvent send refs =
  go WaitForTxState{pendingRefs = refs, mempoolNotifiedRefs = Set.empty}
 where
  go :: WaitForTxState -> m ()
  go state
    | Set.null (pendingRefs state) = liftIO $ send NoNextElem
    | otherwise = do
        event <- nextEvent
        state' <- case event of
          MempoolSnapshotChanged current -> do
            let newlyMempool =
                  (pendingRefs state `Set.intersection` current) `Set.difference` mempoolNotifiedRefs state
            liftIO $ mapM_ (`sendStage` U5c.STAGE_MEMPOOL) (Set.toList newlyMempool)
            pure state{mempoolNotifiedRefs = mempoolNotifiedRefs state <> newlyMempool}
          BlockApplied confirmed -> do
            let newlyConfirmed = pendingRefs state `Set.intersection` confirmed
            liftIO $ mapM_ (`sendStage` U5c.STAGE_CONFIRMED) (Set.toList newlyConfirmed)
            pure state{pendingRefs = pendingRefs state `Set.difference` newlyConfirmed}
        go state'

  sendStage :: TxId -> U5c.Stage -> IO ()
  sendStage ref stage =
    send . NextElem $
      defMessage
        & U5c.ref .~ serialiseToRawBytes ref
        & U5c.stage .~ Proto stage
