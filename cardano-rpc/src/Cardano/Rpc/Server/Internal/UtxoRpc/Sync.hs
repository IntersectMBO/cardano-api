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

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
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

-- | Handle the @FollowTip@ SyncService RPC method.
-- Streams fully parsed blocks as the chain advances, starting from the
-- first of the request's intersection points found on the chain (client
-- preference order). An intersection block ref with an empty hash denotes
-- origin; when the intersect list is empty, the stream follows from the
-- current tip.
-- The first streamed message is always a @reset@ announcing where the
-- stream starts. Later rollbacks are delivered the same way, as @reset@
-- actions carrying the rollback point's @BlockRef@ (slot and hash only),
-- equivalent to ChainSync's @MsgRollBackward@.
-- Every response also carries the current chain tip.
-- Returns @INVALID_ARGUMENT@ if an intersection block reference has an
-- invalid hash and @NOT_FOUND@ if none of the intersection points are on
-- the chain.
-- Runs until the client disconnects or the stream is otherwise closed; the
-- follower is closed on every exit path by 'withFollower'.
followTipMethod
  :: MonadRpc e m
  => Proto U5c.FollowTipRequest
  -- ^ Request containing optional intersection points (slot + hash)
  -> (NextElem (Proto U5c.FollowTipResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> m ()
followTipMethod request send = do
  nodeKernelAccess@NodeKernelAccess{chainDb, systemStart, readEraHistory} <-
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

-- | The @FollowTip@ streaming loop: given a positioned follower and a
-- resolved, non-empty list of intersection points, finds the intersection
-- and streams chain changes as they arrive.
--
-- Takes its collaborators as plain arguments instead of a 'NodeKernelAccess',
-- so it can be driven by a scripted follower and stub capabilities in tests
-- with no live ChainDB required (see @Test.Cardano.Rpc.FollowTipStream@, the
-- first unit coverage of this loop). Only 'MonadIO' is needed: neither the
-- follower actions, the tip/timestamp readers, nor the gRPC error path
-- ('throwGrpcErrorWithMessage') require anything stronger.
--
-- Rollbacks are delivered as @reset@ actions carrying the rollback point's
-- @BlockRef@ (slot and hash only), equivalent to ChainSync's
-- @MsgRollBackward@. Every emitted message, reset or apply, carries the
-- current tip.
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
  -> (NextElem (Proto U5c.FollowTipResponse) -> IO ())
  -- ^ Callback used to send each streamed response
  -> [ChainPoint]
  -- ^ Resolved, non-empty intersection points, in client preference order
  -> m ()
followTipStream ChainFollower{nextChange, findIntersect} readTip slotTimestamp send startPoints =
  -- freezes the caller's call stack (e.g. 'followTipMethod's) so the
  -- @NOT_FOUND@ thrown below - and any exception raised by the collaborator
  -- actions threaded through the loop - points back to the real call site
  -- rather than somewhere inside this loop; makes the 'HasCallStack'
  -- constraint above load-bearing instead of merely a caller restriction
  withFrozenCallStack $ do
    resolvedIntersection <- findIntersect startPoints
    case resolvedIntersection of
      Nothing ->
        throwGrpcErrorWithMessage GrpcNotFound $
          "no intersection found: none of the "
            <> tshow (length startPoints)
            <> " intersect points are on the chain"
      Just _ ->
        -- after a successful 'findIntersect' the follower's next instruction
        -- is a 'RollBack' to the intersection - the loop reports it as the
        -- initial 'reset' announcing where the stream starts
        go
 where
  sendMessage action = do
    tip <- readTip
    liftIO . send . NextElem $ action & U5c.maybe'tip .~ tip

  go :: m ()
  go = do
    change <- nextChange
    case change of
      ChainApply (rawBytes, blockInMode@(BlockInMode _ block)) -> do
        let BlockHeader slot _ _ = getBlockHeader block
        timestamp <- slotTimestamp slot
        sendMessage $ defMessage & U5c.apply .~ mkAnyChainBlock rawBytes blockInMode timestamp
      ChainRollBack point ->
        sendMessage $ defMessage & U5c.reset .~ chainPointToBlockRef point
    go

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
