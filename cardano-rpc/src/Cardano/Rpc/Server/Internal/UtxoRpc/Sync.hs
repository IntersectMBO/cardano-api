{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for the UTxO RPC @SyncService@ - synchronising chain data
-- (fetching blocks, dumping history, following the tip).
module Cardano.Rpc.Server.Internal.UtxoRpc.Sync
  ( fetchBlockMethod
  , followTipMethod
  , followTipStream
  , readTipMethod
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Tracing ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Block (mkAnyChainBlock)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint
  ( chainPointToBlockRef
  , mkTipBlockRef
  , tipHeaderPoint
  )
import Cardano.Rpc.Server.NodeKernelAccess

import Cardano.Ledger.BaseTypes qualified as L

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Sequence qualified as Seq
import Data.Time.Clock (UTCTime)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec
  ( GrpcError (GrpcInternal, GrpcInvalidArgument, GrpcNotFound)
  , NextElem (NextElem)
  , Proto
  )

-- | Handle the @FetchBlock@ SyncService RPC method.
-- Fetches a block from ChainDB by slot and header hash.
-- Byron-era transactions carry no fee: Byron fees are implicit (inputs minus
-- outputs) and computing them needs UTxO lookups this handler does not do.
-- Returns @NOT_FOUND@ if the requested block is missing.
-- Returns @INVALID_ARGUMENT@ if the block reference has an invalid hash.
fetchBlockMethod
  :: MonadRpc e m
  => Proto U5c.FetchBlockRequest
  -- ^ Request containing a block reference (slot + hash)
  -> m (Proto U5c.FetchBlockResponse)
  -- ^ Response containing the fetched block with raw CBOR and cardano header
fetchBlockMethod request = do
  nodeKernelAccess@NodeKernelAccess{systemStart, readEraHistory} <- grabNodeKernelAccess
  (slot, headerHash) <- blockRefToPoint (request ^. U5c.ref)
  let throwNotFound =
        throwGrpcErrorWithMessage GrpcNotFound $
          "block not found at slot " <> tshow (unSlotNo slot)
  (rawBytes, blockInMode) <-
    fetchBlock nodeKernelAccess slot headerHash >>= maybe throwNotFound pure
  timestamp <- slotTimestampOrThrow systemStart readEraHistory slot
  pure $ defMessage & U5c.block .~ mkAnyChainBlock rawBytes blockInMode timestamp

-- | Handle the @ReadTip@ SyncService RPC method.
-- Reads the current chain tip from ChainDB and returns it as slot, block
-- header hash, block height and slot timestamp.
-- When the chain is at origin, the tip field is left unset.
readTipMethod
  :: MonadRpc e m
  => Proto U5c.ReadTipRequest
  -> m (Proto U5c.ReadTipResponse)
readTipMethod _request = do
  NodeKernelAccess{chainDb, systemStart, readEraHistory} <- grabNodeKernelAccess
  tip <- readTipBlockRef chainDb (slotTimestampOrThrow systemStart readEraHistory)
  pure $ defMessage & U5c.maybe'tip .~ tip

-- | Handle the @FollowTip@ SyncService RPC method: stream fully parsed
-- blocks as the chain advances.
--
-- Where the stream starts: at the first of the request's intersection
-- points found on the chain, in client preference order. A block ref with
-- an empty hash means origin. An empty intersect list means the current
-- tip.
--
-- What the client receives: first a @reset@ announcing the start point,
-- then an @apply@ per adopted block. A rollback becomes @undo@ actions
-- carrying the rolled-back blocks, re-fetched from ChainDB and streamed
-- newest first. When the blocks can no longer be re-fetched, because
-- garbage collection won the race against the client, the rollback
-- becomes a @reset@ carrying the rollback point's @BlockRef@ instead,
-- slot and hash only, like ChainSync's @MsgRollBackward@. The tracked
-- window is sized to the node's security parameter /k/, so no rollback
-- consensus can produce falls outside it (see
-- 'Cardano.Rpc.Server.NodeKernelAccess.Type.NodeKernelAccess').
-- Every response also carries the current chain tip.
--
-- Errors: @INVALID_ARGUMENT@ if an intersection block ref has an invalid
-- hash, @NOT_FOUND@ if none of the intersection points are on the chain.
--
-- Runs until the client disconnects or the stream is otherwise closed;
-- 'withFollower' closes the follower on every exit path.
followTipMethod
  :: MonadRpc e m
  => Proto U5c.FollowTipRequest
  -- ^ Request containing optional intersection points (slot + hash)
  -> (NextElem (Proto U5c.FollowTipResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
followTipMethod request send = do
  nodeKernelAccess@NodeKernelAccess{chainDb, systemStart, readEraHistory, securityParam} <-
    grabNodeKernelAccess
  requestedPoints <- traverse blockRefToIntersectPoint (request ^. U5c.intersect)
  withFollower nodeKernelAccess $ \follower -> do
    -- an empty intersect list follows from the current tip; resolving it
    -- reaches into ChainDB directly (there is no 'ChainFollower' operation
    -- for "the current tip point"), so this step stays here rather than
    -- moving into 'followTipStream', which only takes an already-resolved,
    -- non-empty point list
    let slotTimestamp = slotTimestampOrThrow systemStart readEraHistory
    startPoints <-
      if null requestedPoints
        then do
          tipHeader <- liftIO $ Consensus.getTipHeader chainDb
          pure [maybe ChainPointAtGenesis tipHeaderPoint tipHeader]
        else pure requestedPoints
    followTipStream
      follower
      (readTipBlockRef chainDb slotTimestamp)
      slotTimestamp
      (fetchBlockByChainPoint nodeKernelAccess)
      (fromIntegral . L.unNonZero $ Consensus.maxRollbacks securityParam)
      send
      startPoints

-- | Convert an intersection @BlockRef@ to a 'ChainPoint'. A block ref with
-- an empty hash denotes origin, so clients can append it to the intersect
-- list as a catch-all: origin is on every chain, which makes the
-- intersection infallible.
-- Throws @INVALID_ARGUMENT@ if a non-empty hash is malformed.
blockRefToIntersectPoint
  :: MonadRpc e m
  => Proto U5c.BlockRef
  -> m ChainPoint
blockRefToIntersectPoint blockRef
  | BS.null (blockRef ^. U5c.hash) = pure ChainPointAtGenesis
  | otherwise = uncurry ChainPoint <$> blockRefToPoint blockRef

-- | Convert a @BlockRef@ into its slot and deserialised block header hash.
-- Throws @INVALID_ARGUMENT@ if the hash is malformed.
blockRefToPoint
  :: MonadRpc e m
  => Proto U5c.BlockRef
  -> m (SlotNo, Hash BlockHeader)
blockRefToPoint blockRef = do
  let slot = SlotNo $ blockRef ^. U5c.slot
      hashBytes = blockRef ^. U5c.hash
      throwInvalidHash =
        throwGrpcErrorWithMessage GrpcInvalidArgument $
          "invalid block header hash (" <> tshow (BS.length hashBytes) <> " bytes)"
  headerHash <-
    deserialiseFromRawBytes (proxyToAsType (Proxy @(Hash BlockHeader))) hashBytes
      & either (const throwInvalidHash) pure
  pure (slot, headerHash)

-- | Adapt 'fetchBlock', which takes a slot and a hash, to the point-based
-- re-fetch parameter of 'followTipStream', which uses it to rebuild @undo@
-- payloads.
--
-- The genesis arm returns 'Nothing' only to keep the function total; it
-- cannot actually be reached. 'followTipStream' re-fetches only points
-- from its applied-points window, and every entry there comes from a
-- decoded block's header, which always has a real slot and hash (see
-- 'TrackedPoints').
fetchBlockByChainPoint
  :: MonadIO m
  => NodeKernelAccess
  -> ChainPoint
  -> m (Maybe (ByteString, BlockInMode))
fetchBlockByChainPoint _nodeKernelAccess ChainPointAtGenesis = pure Nothing
fetchBlockByChainPoint nodeKernelAccess (ChainPoint slot headerHash) =
  fetchBlock nodeKernelAccess slot headerHash

-- | The applied points 'followTipStream' tracks for undo re-fetch, newest
-- first.
--
-- The entries are raw @(slot, header hash)@ pairs, not 'ChainPoint's.
-- Every entry comes from 'getBlockHeader' on a decoded 'ChainApply'
-- payload, so it always has a real slot and hash. The pair type turns
-- "an applied point is never genesis" from a convention into a fact of
-- the type: a @Seq ChainPoint@ would admit 'ChainPointAtGenesis' even
-- though it could never legitimately occur here.
type TrackedPoints = Seq.Seq (SlotNo, Hash BlockHeader)

-- | The @FollowTip@ streaming loop. Finds the intersection with the given
-- points, then streams chain changes as they arrive.
--
-- The collaborators are plain arguments rather than a 'NodeKernelAccess'
-- so tests can drive the loop with a scripted follower and stubbed
-- capabilities, no live ChainDB required (see
-- @Test.Cardano.Rpc.FollowTipStream@, the first unit coverage of this
-- loop). 'MonadIO' is enough for all of them, including the gRPC error
-- path ('throwGrpcErrorWithMessage').
--
-- How a rollback is delivered, by case:
--
-- 1. The target is within the tracked window, meaning it is one of the
--    last @trackingCap@ applied points or the window floor (the stream's
--    start point, on which a rollback undoes everything tracked). Each
--    rolled-back block is re-fetched by point and emitted as @undo@,
--    newest first.
-- 2. A re-fetch misses mid-undo because garbage collection won the race:
--    a single @reset@ at the rollback target. Partial undo followed by
--    reset is coherent because @reset@ is absolute positioning.
-- 3. The target is outside the window, either deeper than the cap or the
--    initial rollback-to-intersection when nothing is tracked yet: a
--    single @reset@, as in 2.
--
-- Every emitted message, apply or undo or reset, carries the current tip.
--
-- Throws @NOT_FOUND@ if none of the intersection points are on the chain.
-- Runs until the client disconnects or the stream is otherwise closed;
-- follower cleanup is the caller's responsibility (see 'withFollower').
followTipStream
  :: forall m
   . HasCallStack
  => MonadIO m
  => ChainFollower
  -- ^ Follower to stream changes from
  -> m (Maybe (Proto U5c.BlockRef))
  -- ^ Read the current chain tip, projected into a @BlockRef@
  -> (SlotNo -> m UTCTime)
  -- ^ Convert a slot to its wall-clock timestamp
  -> (ChainPoint -> m (Maybe (ByteString, BlockInMode)))
  -- ^ Re-fetch a block by point, to reconstruct @undo@ payloads on
  -- rollback. 'Nothing' means the block is no longer available (e.g. the
  -- VolatileDB has garbage-collected it past the immutable tip); the loop
  -- falls back to @reset@ in that case.
  -> Int
  -- ^ How many applied points to track for undo re-fetch. In production
  -- this is the node's security parameter /k/
  -- ('Cardano.Rpc.Server.NodeKernelAccess.Type.securityParam'). Consensus
  -- never rolls back more than /k/ blocks, so tracking /k/ points covers
  -- every rollback the protocol can produce, on any network. An entry
  -- costs roughly 40 bytes, so the window costs about @40 * k@ bytes per
  -- stream: around 86 KB on mainnet, where /k/ = 2160. A rollback deeper
  -- than the cap degrades to @reset@.
  -> (NextElem (Proto U5c.FollowTipResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> [ChainPoint]
  -- ^ Resolved, non-empty intersection points, in client preference order
  -> m ()
followTipStream ChainFollower{nextChange, findIntersect} readTip slotTimestamp fetchBlockByPoint trackingCap send startPoints =
  -- freezes the caller's call stack (e.g. 'followTipMethod's) so the
  -- @NOT_FOUND@ thrown below, and any exception raised by the collaborator
  -- actions threaded through the loop, points at the real call site rather
  -- than somewhere inside this loop. This is what the 'HasCallStack'
  -- constraint above is for.
  withFrozenCallStack $ do
    resolvedIntersection <- findIntersect startPoints
    startPoint <- case resolvedIntersection of
      Nothing ->
        throwGrpcErrorWithMessage GrpcNotFound $
          "no intersection found: none of the "
            <> tshow (length startPoints)
            <> " intersect points are on the chain"
      Just point -> pure point
    -- after a successful 'findIntersect' the follower's next instruction is
    -- a 'RollBack' to the intersection - the loop reports it as the initial
    -- 'reset' announcing where the stream starts (the "nothing tracked yet"
    -- branch of 'handleRollback' below, since the intersection is also the
    -- initial window floor and nothing has been applied yet)
    go startPoint Seq.empty
 where
  sendMessage action = do
    tip <- readTip
    liftIO . send . NextElem $ action & U5c.maybe'tip .~ tip

  go :: ChainPoint -> TrackedPoints -> m ()
  go floorPoint tracked = do
    change <- nextChange
    (floorPoint', tracked') <- case change of
      ChainApply (rawBytes, blockInMode@(BlockInMode _ block)) -> do
        let BlockHeader slot headerHash _ = getBlockHeader block
        timestamp <- slotTimestamp slot
        sendMessage $ defMessage & U5c.apply .~ mkAnyChainBlock rawBytes blockInMode timestamp
        pure (floorPoint, trackApplied trackingCap (slot, headerHash) tracked)
      ChainRollBack point -> handleRollback point floorPoint tracked
    go floorPoint' tracked'

  -- \| Dispatch a rollback to undo or reset. The window floor starts as
  -- the stream's start point and only ever moves forward, to a rollback
  -- target that fell outside the window (the 'Nothing' case below). A
  -- later rollback landing on the new floor can then be served as undo
  -- instead of degrading to reset a second time.
  handleRollback :: ChainPoint -> ChainPoint -> TrackedPoints -> m (ChainPoint, TrackedPoints)
  handleRollback point floorPoint tracked =
    case windowSplit point floorPoint tracked of
      Nothing -> do
        -- deeper than the cap, below the stream's start, or the initial
        -- rollback-to-intersection with nothing tracked yet: today's
        -- unchanged fallback, which also preserves the first-message-reset
        -- invariant. The target becomes the new floor.
        sendMessage $ defMessage & U5c.reset .~ chainPointToBlockRef point
        pure (point, Seq.empty)
      Just (undone, kept)
        | Seq.null undone ->
            -- nothing newer than the target is tracked (the stream-opening
            -- rollback, or a rollback that is a no-op against the current
            -- position): reset communicates the position, with nothing to
            -- undo
            sendMessage (defMessage & U5c.reset .~ chainPointToBlockRef point)
              $> (floorPoint, kept)
        | otherwise -> do
            kept' <- undoNewestFirst point kept undone
            pure (floorPoint, kept')

  -- \| Split the tracked points at the rollback target. 'Nothing' means
  -- the target is outside the window: not the floor and not a tracked
  -- point. Otherwise the first half is the points strictly newer than the
  -- target, to be undone newest first, and the second half is what
  -- survives: the target's own entry, if it is tracked, and everything
  -- older.
  windowSplit :: ChainPoint -> ChainPoint -> TrackedPoints -> Maybe (TrackedPoints, TrackedPoints)
  windowSplit point floorPoint tracked = case point of
    ChainPoint slot headerHash
      | Just i <- Seq.findIndexL (== (slot, headerHash)) tracked ->
          Just (Seq.take i tracked, Seq.drop i tracked)
    _
      | point == floorPoint -> Just (tracked, Seq.empty)
      | otherwise -> Nothing

  -- \| Re-fetch and emit @undo@ for each pending point, newest first.
  -- Stops at the first fetch miss (garbage collection won the race) and
  -- sends a single absolute @reset@ at the rollback point instead.
  undoNewestFirst :: ChainPoint -> TrackedPoints -> TrackedPoints -> m TrackedPoints
  undoNewestFirst point kept = loop
   where
    loop pending = case Seq.viewl pending of
      Seq.EmptyL -> pure kept
      (slot, headerHash) Seq.:< rest -> do
        fetched <- fetchBlockByPoint (ChainPoint slot headerHash)
        case fetched of
          Just (rawBytes, blockInMode) -> do
            timestamp <- slotTimestamp slot
            sendMessage $ defMessage & U5c.undo .~ mkAnyChainBlock rawBytes blockInMode timestamp
            loop rest
          Nothing ->
            sendMessage (defMessage & U5c.reset .~ chainPointToBlockRef point) $> kept

  trackApplied :: Int -> (SlotNo, Hash BlockHeader) -> TrackedPoints -> TrackedPoints
  trackApplied cap entry tracked = Seq.take cap (entry Seq.<| tracked)

-- | Read the current chain tip and project it into a @BlockRef@ via
-- 'mkTipBlockRef', or 'Nothing' at origin.
readTipBlockRef
  :: MonadIO m
  => Consensus.ChainDB IO (Consensus.CardanoBlock Consensus.StandardCrypto)
  -> (SlotNo -> m UTCTime)
  -- ^ Convert a slot to its wall-clock timestamp
  -> m (Maybe (Proto U5c.BlockRef))
readTipBlockRef chainDb slotTimestamp = do
  tipHeader <- liftIO $ Consensus.getTipHeader chainDb
  forM tipHeader $ \header ->
    mkTipBlockRef header <$> slotTimestamp (Consensus.blockSlot header)

-- | Convert a slot to its wall-clock timestamp.
-- Throws @INTERNAL@ when the slot is past the era history horizon.
slotTimestampOrThrow
  :: MonadIO m
  => SystemStart
  -> m EraHistory
  -- ^ Read current era history from the ledger state
  -> SlotNo
  -> m UTCTime
slotTimestampOrThrow systemStart readEraHistory slot = do
  eraHistory <- readEraHistory
  slotToUTCTime systemStart eraHistory slot
    & either (const throwPastHorizon) pure
 where
  throwPastHorizon =
    throwGrpcErrorWithMessage GrpcInternal $
      "cannot convert slot "
        <> tshow (unSlotNo slot)
        <> " to timestamp: the slot is past the era history horizon;"
        <> " check that the requested slot is correct and that the node is fully in sync"
