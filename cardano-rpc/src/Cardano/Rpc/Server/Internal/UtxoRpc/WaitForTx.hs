{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Handler for the UTxO RPC @WaitForTx@ SubmitService method: stream stage
-- transitions for a set of requested transactions until every one of them
-- reaches @STAGE_CONFIRMED@.
module Cardano.Rpc.Server.Internal.UtxoRpc.WaitForTx
  ( waitForTxMethod

    -- * Internals, exported for tests
  , maxWaitForTxRefs
  , parseRequestRefs
  , waitForTxIdleTimeout
  , idleTimeLeft
  , nextIdleDeadline
  , idleDeadlineAfterStep
  , DrainedChange (..)
  , drainedBlocks
  , WaitForTxEvent (..)
  , WaitForTxState (..)
  , initialWaitForTxState
  , stepWaitForTx
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.NodeKernelAccess

import Cardano.Ledger.Core qualified as L
import Ouroboros.Consensus.Mempool.API qualified as Consensus (MempoolSnapshot (snapshotHasTx))

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import Data.Time.Clock (DiffTime)
import Data.Traversable (mapAccumL)
import Network.GRPC.Spec
  ( GrpcError (GrpcDeadlineExceeded, GrpcInvalidArgument)
  , NextElem (NextElem, NoNextElem)
  , Proto (Proto)
  )

-- | Handle the @WaitForTx@ SubmitService RPC method.
--
-- Reports @STAGE_MEMPOOL@ when a ref enters the mempool and
-- @STAGE_CONFIRMED@ when a ref appears in a block applied after the stream
-- opened, and closes the stream once every ref has reached
-- @STAGE_CONFIRMED@.
--
-- __Threading model:__ one thread per call, with no shared state. Each step
-- is one STM wait on the chain tip and the pending refs' mempool membership,
-- bounded by 'timeout'; on a tip move the follower is drained without blocking.
--
-- __Idle timeout:__ grapesy notices a gone client only when the handler
-- sends to it, so a stream that has sent nothing for ten minutes ends with
-- @DEADLINE_EXCEEDED@. A client that still wants the outcome reopens it.
--
-- __Race contract:__ a transaction that reaches the chain between
-- submission and this call is invisible - it is no longer in the mempool
-- and will not appear in any block applied after the stream opens, so the
-- stream never reports that ref. Clients must open @WaitForTx@
-- before or concurrently with submitting the transaction.
--
-- __Reconnect gap:__ for the same reason, a confirmation that lands between
-- an idle close and the reopen is missed.
--
-- __Rollbacks are not reported:__ a ref confirmed in a block that is later
-- rolled back is not announced again; @FollowTip@'s @undo@ stream is the
-- client-side tool for that.
--
-- __Eviction is non-terminal:__ a ref that leaves the mempool without
-- on-chain inclusion produces no message; the stream keeps waiting, since
-- chain churn can legitimately re-add it later.
--
-- __Mempool delivery follows observed membership:__ a ref gets @STAGE_MEMPOOL@
-- when first observed in the mempool, and again after an observed removal and
-- re-entry. Removal and re-entry between snapshots can be missed.
--
-- Duplicate refs in the request are treated as one. An empty request
-- closes the stream immediately, with no messages sent - vacuously
-- satisfied, and without opening a follower.
--
-- Throws @INVALID_ARGUMENT@ if a ref is not a well-formed transaction id or
-- the request has more than 'maxWaitForTxRefs' refs, and
-- @DEADLINE_EXCEEDED@ when the idle timeout expires.
waitForTxMethod
  :: forall e m
   . MonadRpc e m
  => Proto U5c.WaitForTxRequest
  -- ^ Request containing the transaction ids to wait for
  -> (NextElem (Proto U5c.WaitForTxResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
waitForTxMethod request send = do
  refs <- parseRequestRefs $ request ^. U5c.ref
  case initialWaitForTxState refs of
    Nothing -> liftIO $ send NoNextElem
    Just state -> do
      nodeKernelAccess <- grabNodeKernelAccess
      withBlockFollower nodeKernelAccess $ \follower -> do
        tip <- positionFollowerAtCurrentTip nodeKernelAccess follower
        now <- getMonotonicDiffTime
        waitLoop nodeKernelAccess follower state tip (now + waitForTxIdleTimeout)
 where
  -- Wait for the next node change before the idle deadline, step the core on
  -- it and send its responses, until the core closes the stream.
  waitLoop
    :: NodeKernelAccess
    -> ChainFollower BlockInMode
    -> WaitForTxState
    -> ChainPoint
    -> DiffTime
    -> m ()
  waitLoop nodeKernelAccess follower state lastTip deadline = do
    now <- getMonotonicDiffTime
    (change, expired) <- case idleTimeLeft deadline now of
      Nothing -> (,True) <$> readyChangeOrIdleTimeout
      Just timeLeft ->
        timeout timeLeft (atomically nextChange) >>= \case
          Nothing -> (,True) <$> readyChangeOrIdleTimeout
          Just waited -> pure (waited, False)
    (event, tip) <- case change of
      TipMoved newTip -> do
        blocks <- drainFollower follower $ pendingRefs state
        pure (TipBlocks blocks, newTip)
      MempoolChanged inMempool -> pure (MempoolMembership inMempool, lastTip)
    let (next, responses) = stepWaitForTx state event
    liftIO $ mapM_ (send . NextElem) responses
    sentAt <- getMonotonicDiffTime
    case next of
      Nothing -> liftIO $ send NoNextElem
      Just state' ->
        maybe throwIdleTimeout (waitLoop nodeKernelAccess follower state' tip) $
          idleDeadlineAfterStep expired sentAt deadline responses
   where
    nextChange :: STM NodeChange
    nextChange = nextNodeChange nodeKernelAccess state lastTip

    -- Past the idle deadline, still take a change that is already waiting,
    -- and only then end the stream.
    readyChangeOrIdleTimeout :: m NodeChange
    readyChangeOrIdleTimeout = maybe throwIdleTimeout pure =<< atomically (optional nextChange)

  throwIdleTimeout :: m a
  throwIdleTimeout =
    throwGrpcErrorWithMessage
      GrpcDeadlineExceeded
      "no WaitForTx update for 10 minutes, reopen the stream to keep waiting"

-- | The most refs one @WaitForTx@ request may carry. Each pending ref is
-- checked against the mempool on every wake-up.
maxWaitForTxRefs :: Int
maxWaitForTxRefs = 500

-- | Parse the request refs into a set of transaction ids.
-- Throws @INVALID_ARGUMENT@ if there are more than 'maxWaitForTxRefs' refs,
-- duplicates included, or if a ref is not a well-formed transaction id.
parseRequestRefs :: MonadIO m => [ByteString] -> m (Set TxId)
parseRequestRefs refs = do
  let refCount = length refs
  when (refCount > maxWaitForTxRefs) $
    throwGrpcErrorWithMessage GrpcInvalidArgument $
      "too many transaction refs: "
        <> tshow refCount
        <> ", maximum "
        <> tshow maxWaitForTxRefs
        <> "; batch your requests"
  Set.fromList <$> traverse parseRef refs

-- | How long a @WaitForTx@ stream may go without sending a message before it
-- ends with @DEADLINE_EXCEEDED@: ten minutes.
waitForTxIdleTimeout :: DiffTime
waitForTxIdleTimeout = 10 * 60

-- | The microseconds left until the idle deadline, rounded up so that they
-- never become a zero wait, or 'Nothing' once the deadline has passed.
idleTimeLeft
  :: DiffTime
  -- ^ The idle deadline, on the monotonic clock
  -> DiffTime
  -- ^ Monotonic time now
  -> Maybe Int
idleTimeLeft deadline now
  | now >= deadline = Nothing
  | otherwise = Just . toMicros $ deadline - now

-- | The idle deadline after a step: one 'waitForTxIdleTimeout' from now if the
-- step sent a response, unchanged otherwise.
nextIdleDeadline
  :: DiffTime
  -- ^ Monotonic time now, after sending
  -> DiffTime
  -- ^ The current idle deadline
  -> [Proto U5c.WaitForTxResponse]
  -- ^ The responses the step sent
  -> DiffTime
nextIdleDeadline now deadline responses
  | null responses = deadline
  | otherwise = now + waitForTxIdleTimeout

-- | The idle deadline after a step, or 'Nothing' if the stream must end with
-- @DEADLINE_EXCEEDED@: a change taken past the deadline that sent no response.
idleDeadlineAfterStep
  :: Bool
  -- ^ Whether the change was taken past the idle deadline
  -> DiffTime
  -- ^ Monotonic time now, after sending
  -> DiffTime
  -- ^ The current idle deadline
  -> [Proto U5c.WaitForTxResponse]
  -- ^ The responses the step sent
  -> Maybe DiffTime
idleDeadlineAfterStep expired now deadline responses
  | expired && null responses = Nothing
  | otherwise = Just $ nextIdleDeadline now deadline responses

-- | Microseconds for 'timeout', rounded up.
toMicros :: DiffTime -> Int
toMicros = ceiling . (* 1000000)

-- | The monotonic clock, in seconds since some fixed point.
getMonotonicDiffTime :: MonadIO m => m DiffTime
getMonotonicDiffTime = realToFrac <$> getMonotonicTime

-- | A change on the node that 'waitForTxMethod' wakes up for.
data NodeChange
  = -- | The chain tip moved to the given point.
    TipMoved ChainPoint
  | -- | The pending refs now in the mempool, which differ from those
    -- already announced.
    MempoolChanged (Set TxId)

-- | Wait in STM for the chain tip to move away from the last one seen or,
-- failing that, for the pending refs in the mempool to differ from those
-- already announced.
nextNodeChange
  :: NodeKernelAccess
  -> WaitForTxState
  -> ChainPoint
  -- ^ The last tip seen, which the follower has been drained up to
  -> STM NodeChange
nextNodeChange nodeKernelAccess WaitForTxState{pendingRefs, mempoolNotifiedRefs} lastTip =
  tipMoved <|> mempoolChanged
 where
  tipMoved :: STM NodeChange
  tipMoved = do
    tip <- readChainTipPoint nodeKernelAccess
    checkSTM $ tip /= lastTip
    pure $ TipMoved tip

  mempoolChanged :: STM NodeChange
  mempoolChanged = do
    -- Ids in the ledger's current era, the one the mempool stores txs in. Consensus compares
    -- ids by hash only, so the brief ledger/mempool era gap at a hard fork is harmless.
    AnyCardanoEra era <- readCurrentEra nodeKernelAccess
    snapshot <- readMempoolSnapshot nodeKernelAccess
    let inMempool =
          Set.filter
            (Consensus.snapshotHasTx snapshot . Consensus.toConsensusTxId . Consensus.TxIdInMode era)
            pendingRefs
    checkSTM $ inMempool /= mempoolNotifiedRefs
    pure $ MempoolChanged inMempool

-- | Take every chain update the follower has ready, without blocking, and
-- return the pending refs in each applied block that the same drain did not
-- roll back, oldest block first.
drainFollower
  :: forall m
   . MonadIO m
  => ChainFollower BlockInMode
  -> Set TxId
  -- ^ The pending refs
  -> m [Set TxId]
drainFollower ChainFollower{tryNextChange} pendingRefs = drainedBlocks <$> go []
 where
  go :: [DrainedChange] -> m [DrainedChange]
  go changes =
    tryNextChange >>= \case
      Nothing -> pure $ reverse changes
      Just (ChainApply blockInMode@(BlockInMode _ block)) -> do
        let BlockHeader slot headerHash _ = getBlockHeader block
            inBlock = pendingRefs `Set.intersection` blockTxIds blockInMode
        go $ DrainedBlock (ChainPoint slot headerHash) inBlock : changes
      Just (ChainRollBack point) -> go $ DrainedRollBack point : changes

-- | A chain update taken by 'drainFollower', reduced to what @WaitForTx@
-- needs.
data DrainedChange
  = -- | An applied block's point and the pending refs in it.
    DrainedBlock ChainPoint (Set TxId)
  | -- | A rollback to the given point.
    DrainedRollBack ChainPoint
  deriving Show

-- | The refs of each drained block that no later rollback in the same drain
-- undid, oldest block first. A rollback to a point before every drained
-- block undoes them all.
drainedBlocks :: [DrainedChange] -> [Set TxId]
drainedBlocks = reverse . map snd . foldl' step []
 where
  -- Blocks newest first, so a rollback drops the prefix after its target.
  step :: [(ChainPoint, Set TxId)] -> DrainedChange -> [(ChainPoint, Set TxId)]
  step blocks = \case
    DrainedBlock point refs -> (point, refs) : blocks
    DrainedRollBack point -> dropWhile ((/= point) . fst) blocks

-- | An event 'stepWaitForTx' handles.
data WaitForTxEvent
  = -- | The chain tip moved: the transaction ids in the blocks applied since
    -- the previous tip move, one set per block, oldest first. Ids that are
    -- not pending may be left out.
    TipBlocks [Set TxId]
  | -- | The pending refs now in the mempool, after that set changed.
    MempoolMembership (Set TxId)
  deriving (Eq, Show)

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

-- | Parse a request ref into a transaction id.
-- Throws @INVALID_ARGUMENT@ if the bytes are not a well-formed transaction id.
parseRef :: forall m. MonadIO m => ByteString -> m TxId
parseRef bytes =
  either (const throwInvalidRef) pure $ deserialiseFromRawBytes AsTxId bytes
 where
  throwInvalidRef :: m TxId
  throwInvalidRef =
    throwGrpcErrorWithMessage GrpcInvalidArgument $
      "invalid transaction id (" <> tshow (BS.length bytes) <> " bytes)"

-- | The per-ref state of a @WaitForTx@ stream: refs still awaiting
-- confirmation, and the refs already announced as in the mempool and still
-- in it (so an evicted-then-re-added ref is announced again).
data WaitForTxState = WaitForTxState
  { pendingRefs :: !(Set TxId)
  , mempoolNotifiedRefs :: !(Set TxId)
  }
  deriving (Eq, Show)

-- | The state of a new stream for the given refs, already deduplicated, or
-- 'Nothing' if there are none and the stream closes at once.
initialWaitForTxState :: Set TxId -> Maybe WaitForTxState
initialWaitForTxState refs =
  whilePending WaitForTxState{pendingRefs = refs, mempoolNotifiedRefs = Set.empty}

-- | One step of the @WaitForTx@ state machine: the next state, or 'Nothing'
-- once every ref is confirmed and the stream closes, and the responses to
-- send, in order. Confirmations follow block order.
stepWaitForTx
  :: WaitForTxState
  -> WaitForTxEvent
  -> (Maybe WaitForTxState, [Proto U5c.WaitForTxResponse])
stepWaitForTx state =
  first whilePending . \case
    TipBlocks blocks -> concat <$> mapAccumL confirmBlock state blocks
    MempoolMembership inMempool -> announceMempool state inMempool

-- | Confirm the pending refs in one block: @STAGE_CONFIRMED@ for each, and
-- each leaves both sets.
confirmBlock
  :: WaitForTxState
  -> Set TxId
  -> (WaitForTxState, [Proto U5c.WaitForTxResponse])
confirmBlock WaitForTxState{pendingRefs, mempoolNotifiedRefs} block =
  ( WaitForTxState
      { pendingRefs = pendingRefs `Set.difference` confirmed
      , mempoolNotifiedRefs = mempoolNotifiedRefs `Set.difference` confirmed
      }
  , [ defMessage
        & U5c.ref .~ serialiseToRawBytes ref
        & U5c.stage .~ Proto U5c.STAGE_CONFIRMED
    | ref <- toList confirmed
    ]
  )
 where
  confirmed :: Set TxId
  confirmed = pendingRefs `Set.intersection` block

-- | Record the pending refs now in the mempool: @STAGE_MEMPOOL@ for each
-- one that was not already announced.
announceMempool
  :: WaitForTxState
  -> Set TxId
  -> (WaitForTxState, [Proto U5c.WaitForTxResponse])
announceMempool state@WaitForTxState{pendingRefs, mempoolNotifiedRefs} inMempool =
  ( state{mempoolNotifiedRefs = present}
  , [ defMessage
        & U5c.ref .~ serialiseToRawBytes ref
        & U5c.stage .~ Proto U5c.STAGE_MEMPOOL
    | ref <- toList $ present `Set.difference` mempoolNotifiedRefs
    ]
  )
 where
  present :: Set TxId
  present = pendingRefs `Set.intersection` inMempool

-- | The state, or 'Nothing' once no ref is pending.
whilePending :: WaitForTxState -> Maybe WaitForTxState
whilePending state@WaitForTxState{pendingRefs}
  | Set.null pendingRefs = Nothing
  | otherwise = Just state
