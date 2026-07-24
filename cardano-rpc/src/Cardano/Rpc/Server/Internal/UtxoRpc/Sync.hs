{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for the UTxO RPC @SyncService@ - synchronising chain data
-- (fetching blocks, dumping history, following the tip).
module Cardano.Rpc.Server.Internal.UtxoRpc.Sync
  ( fetchBlockMethod
  , followTipMethod
  , readTipMethod
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.Error
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.Internal.UtxoRpc.Type (anyEraTxConstraints, txToUtxoRpcTx)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Byron (byronBlockTxs)
import Cardano.Rpc.Server.NodeKernelAccess

import RIO

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
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
  nodeKernelAccess <- grabNodeKernelAccess
  (slot, headerHash) <- blockRefToPoint (request ^. U5c.ref)
  let throwNotFound =
        throwGrpcErrorWithMessage GrpcNotFound $
          "block not found at slot " <> tshow (unSlotNo slot)
  (rawBytes, blockInMode) <-
    fetchBlock nodeKernelAccess slot headerHash >>= maybe throwNotFound pure
  timestamp <- slotTimestampOrThrow nodeKernelAccess slot
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
  nodeKernelAccess <- grabNodeKernelAccess
  tip <- readTipBlockRef nodeKernelAccess
  pure $ defMessage & U5c.maybe'tip .~ tip

-- | Handle the @FollowTip@ SyncService RPC method.
-- Streams fully parsed blocks as the chain advances, starting from the
-- request's intersection points (or from origin, when none are given or
-- none are found on the current chain).
-- The first streamed message is always a @reset@ announcing where the
-- stream starts: the intersection point when one is found, origin
-- otherwise. Later rollbacks are delivered the same way, as @reset@
-- actions carrying the rollback point's @BlockRef@ (slot and hash only).
-- Every response also carries the current chain tip.
-- Returns @INVALID_ARGUMENT@ if an intersection block reference has an
-- invalid hash.
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
  nodeKernelAccess <- grabNodeKernelAccess
  points <- map (uncurry ChainPoint) <$> traverse blockRefToPoint (request ^. U5c.intersect)
  withFollower nodeKernelAccess $ \ChainFollower{nextChange, findIntersect} -> do
    unless (null points) $ do
      intersection <- findIntersect points
      -- Nothing more to do in either case: after a successful
      -- 'findIntersect' the follower's next instruction is a 'RollBack' to
      -- the intersection, and a follower whose intersection was not moved
      -- ('FollowerInit') yields 'RollBack' to genesis - the loop below
      -- reports both as the initial 'reset'.
      when (isNothing intersection) $
        putTrace TraceRpcFollowTipReset
    forever $ do
      change <- nextChange
      action <- case change of
        ChainApply (rawBytes, blockInMode@(BlockInMode _ block)) -> do
          let BlockHeader slot _ _ = getBlockHeader block
          timestamp <- slotTimestampOrThrow nodeKernelAccess slot
          pure $ defMessage & U5c.apply .~ mkAnyChainBlock rawBytes blockInMode timestamp
        ChainRollBack point ->
          pure $ defMessage & U5c.reset .~ chainPointToBlockRef point
      tip <- readTipBlockRef nodeKernelAccess
      liftIO . send . NextElem $ action & U5c.maybe'tip .~ tip

-- | Assemble the @AnyChainBlock@ proto message: raw CBOR bytes, the cardano
-- header (slot, hash, height - derived from the block itself) and the parsed
-- transactions (all eras), plus the given slot timestamp.
mkAnyChainBlock
  :: ByteString
  -> BlockInMode
  -> UTCTime
  -- ^ Slot wall-clock time; encoded as milliseconds since the Unix epoch
  -> Proto U5c.AnyChainBlock
mkAnyChainBlock rawBytes (BlockInMode _ block) timestamp =
  let BlockHeader slot headerHash (BlockNo height) = getBlockHeader block
      -- Byron transactions are not representable as cardano-api's 'Tx era',
      -- so they are converted straight from the Byron ledger types
      txs = case block of
        ByronBlock consensusBlock ->
          byronBlockTxs (byronBlockRaw consensusBlock)
        ShelleyBlock sbe _ ->
          anyEraTxConstraints sbe $
            getBlockTxs block <&> \(ShelleyTx _ ledgerTx) -> txToUtxoRpcTx ledgerTx
      blockHeader =
        defMessage
          & U5c.slot .~ unSlotNo slot
          & U5c.hash .~ serialiseToRawBytes headerHash
          & U5c.height .~ height
   in defMessage
        & U5c.nativeBytes .~ rawBytes
        & U5c.cardano . U5c.header .~ blockHeader
        & U5c.cardano . U5c.body . U5c.tx .~ txs
        & U5c.cardano . U5c.timestamp .~ utcTimeToMs timestamp

-- | Project a ChainDB header and a slot timestamp into a @BlockRef@: slot,
-- header hash, block height and timestamp.
mkTipBlockRef
  :: Consensus.Header (Consensus.CardanoBlock Consensus.StandardCrypto)
  -> UTCTime
  -- ^ Slot wall-clock time; encoded as milliseconds since the Unix epoch
  -> Proto U5c.BlockRef
mkTipBlockRef header timestamp =
  let slot = Consensus.blockSlot header
      Consensus.OneEraHash tipHash = Consensus.blockHash header
      BlockNo height = Consensus.blockNo header
   in defMessage
        & U5c.slot .~ unSlotNo slot
        & U5c.hash .~ SBS.fromShort tipHash
        & U5c.height .~ height
        & U5c.timestamp .~ utcTimeToMs timestamp

-- | Milliseconds since the Unix epoch, the timestamp encoding UTxO RPC uses.
utcTimeToMs :: UTCTime -> Word64
utcTimeToMs = round . (* 1000) . utcTimeToPOSIXSeconds

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

-- | Project a 'ChainPoint' into a @BlockRef@ with slot and hash only - a
-- rollback reference has no height or timestamp, so those are left at
-- their proto defaults. 'ChainPointAtGenesis' becomes the default message
-- (origin).
chainPointToBlockRef :: ChainPoint -> Proto U5c.BlockRef
chainPointToBlockRef ChainPointAtGenesis = defMessage
chainPointToBlockRef (ChainPoint slot headerHash) =
  defMessage
    & U5c.slot .~ unSlotNo slot
    & U5c.hash .~ serialiseToRawBytes headerHash

-- | Read the current chain tip and project it into a @BlockRef@ via
-- 'mkTipBlockRef', or 'Nothing' at origin.
readTipBlockRef
  :: MonadRpc e m
  => NodeKernelAccess
  -> m (Maybe (Proto U5c.BlockRef))
readTipBlockRef nodeKernelAccess@NodeKernelAccess{chainDb} = do
  tipHeader <- liftIO $ Consensus.getTipHeader chainDb
  forM tipHeader $ \header ->
    mkTipBlockRef header <$> slotTimestampOrThrow nodeKernelAccess (Consensus.blockSlot header)

-- | Convert a slot to its wall-clock timestamp, or throw @INTERNAL@ if the
-- slot is past the era history horizon.
slotTimestampOrThrow
  :: MonadRpc e m
  => NodeKernelAccess
  -> SlotNo
  -> m UTCTime
slotTimestampOrThrow NodeKernelAccess{systemStart, readEraHistory} slot = do
  eraHistory <- readEraHistory
  slotToUTCTime systemStart eraHistory slot
    & either (const throwPastHorizon) pure
 where
  throwPastHorizon =
    throwGrpcErrorWithMessage GrpcInternal $
      "cannot convert slot "
        <> tshow (unSlotNo slot)
        <> " to timestamp: the slot is past the era history horizon;"
        <> " check that the node is fully in sync"
