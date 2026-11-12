{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit coverage for 'followTipStream', the @FollowTip@ streaming loop
-- extracted from 'Cardano.Rpc.Server.Internal.UtxoRpc.Sync.followTipMethod'.
-- The loop takes its collaborators as plain arguments instead of closing over
-- 'Cardano.Rpc.Server.NodeKernelAccess.NodeKernelAccess', so it can be
-- driven here with a scripted 'ChainFollower' and stub tip/timestamp/fetch
-- capabilities, without a live ChainDB.
module Test.Cardano.Rpc.FollowTipStream where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask (FieldPath, resolve)
import Cardano.Rpc.Server.Internal.UtxoRpc.Sync
  ( followTipAppliedRead
  , followTipStream
  , followTipUndoneRead
  )
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Block (CardanoBlockComponent)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (chainPointToBlockRef)
import Cardano.Rpc.Server.NodeKernelAccess (ChainChange (..), ChainFollower (..))

import RIO
import RIO.Writer (runWriter)

import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec
  ( GrpcError (GrpcNotFound)
  , GrpcException (..)
  , NextElem (..)
  , Proto
  , getProto
  )

import Test.Cardano.Rpc.FixtureBlock
  ( boundaryBlockInMode
  , mainBlockInMode
  , oldAnyChainBlock
  , stubTimestamp
  )
import Test.Cardano.Rpc.RecordedQuery

import Hedgehog as H
import Hedgehog.Extras qualified as H

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

-- | The current 'followTipMethod' semantics: the stream opens with the
-- follower's initial rollback reported as a @reset@, then streams 'ChainApply'
-- changes as @apply@ messages in order, every message carrying the current
-- tip as reported by the stub 'readTip'.
hprop_follow_tip_stream_reset_then_applies_in_order :: Property
hprop_follow_tip_stream_reset_then_applies_in_order = H.propertyOnce $ do
  block1@(rawBytes1, blockInMode1) <- mainBlockInMode
  block2@(rawBytes2, blockInMode2) <- boundaryBlockInMode
  let script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        ]
  sent <-
    expectScriptExhausted
      =<< runFollowTipStream script (Just ChainPointAtGenesis) fetchNever testUndoWindow [ChainPointAtGenesis]

  H.note_ "reset(origin), apply(b1), apply(b2), each carrying the stub tip, in order"
  let expectedReset =
        defMessage & U5c.reset .~ chainPointToBlockRef ChainPointAtGenesis & U5c.tip .~ stubTip
      expectedApply1 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
      expectedApply2 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
          & U5c.tip .~ stubTip
  sent === (NextElem <$> [expectedReset, expectedApply1, expectedApply2])

-- | A successful intersection ('findIntersect' returning 'Just') does not
-- cause 'followTipStream' to synthesise an extra @reset@ of its own: the
-- only @reset@ the client sees is the follower's natural post-forward
-- rollback to the intersection point, exactly as the current
-- 'followTipMethod' documentation describes it. This mirrors current
-- semantics exactly - the loop never inspects 'findIntersect's returned
-- point, only whether it found one.
hprop_follow_tip_stream_intersection_success_synthesises_no_extra_reset :: Property
hprop_follow_tip_stream_intersection_success_synthesises_no_extra_reset = H.propertyOnce $ do
  (rawBytes, blockInMode@(BlockInMode _ block)) <- mainBlockInMode
  let BlockHeader slot headerHash _ = getBlockHeader block
      intersectionPoint = ChainPoint slot headerHash
      script = [ChainRollBack intersectionPoint, ChainApply (rawBytes, blockInMode)]
  sent <-
    expectScriptExhausted
      =<< runFollowTipStream script (Just intersectionPoint) fetchNever testUndoWindow [intersectionPoint]

  H.note_ "Exactly the two scripted messages are emitted - no reset beyond the scripted rollback"
  length sent === 2
  firstMessage <- H.nothingFail $ listToMaybe sent
  firstMessage
    === NextElem
      (defMessage & U5c.reset .~ chainPointToBlockRef intersectionPoint & U5c.tip .~ stubTip)

-- | When none of the intersection points are on the chain ('findIntersect'
-- returning 'Nothing'), 'followTipStream' fails the stream with @NOT_FOUND@
-- before sending any message.
hprop_follow_tip_stream_not_found_before_any_send :: Property
hprop_follow_tip_stream_not_found_before_any_send = H.propertyOnce $ do
  let unknownPoint = ChainPoint 100 (HeaderHash (SBS.replicate 32 0xAA))
  (outcome, sent) <- runFollowTipStream [] Nothing fetchNever testUndoWindow [unknownPoint]

  H.note_ "No message is sent before the intersection failure"
  sent === []

  H.note_ "The stream fails with a NOT_FOUND GrpcException"
  case outcome of
    Left e
      | Just GrpcException{grpcError} <- fromException e ->
          grpcError === GrpcNotFound
    _ -> do
      H.note_ "Expected a NOT_FOUND GrpcException"
      H.annotateShow outcome
      H.failure

-- | A rollback whose target is a tracked applied point is delivered as a
-- single @undo@ carrying the re-fetched block, not a @reset@:
-- 'followTipStream' prefers undo delivery whenever the target is within the
-- tracked window. The undo payload is built by the same block read as the
-- original @apply@, so it is identical to it by construction - deterministic
-- re-assembly from the same raw bytes and block.
hprop_follow_tip_stream_rollback_within_window_undoes_via_refetch :: Property
hprop_follow_tip_stream_rollback_within_window_undoes_via_refetch = H.propertyOnce $ do
  block1@(rawBytes1, blockInMode1@(BlockInMode _ rawBlock1)) <- mainBlockInMode
  block2@(rawBytes2, blockInMode2@(BlockInMode _ rawBlock2)) <- boundaryBlockInMode
  let BlockHeader slot1 headerHash1 _ = getBlockHeader rawBlock1
      BlockHeader slot2 headerHash2 _ = getBlockHeader rawBlock2
      point1 = ChainPoint slot1 headerHash1
      point2 = ChainPoint slot2 headerHash2
      script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        , ChainRollBack point1
        ]
      fetchBlockByPoint = fetchKnownBlocks [(point2, block2)]
  sent <-
    expectScriptExhausted
      =<< runFollowTipStream
        script
        (Just ChainPointAtGenesis)
        fetchBlockByPoint
        testUndoWindow
        [ChainPointAtGenesis]

  H.note_ "reset(origin), apply(b1), apply(b2), undo(b2) - b2 re-fetched successfully"
  let block2Payload = oldAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
      expectedReset =
        defMessage & U5c.reset .~ chainPointToBlockRef ChainPointAtGenesis & U5c.tip .~ stubTip
      expectedApply1 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
      expectedApply2 = defMessage & U5c.apply .~ block2Payload & U5c.tip .~ stubTip
      expectedUndo2 = defMessage & U5c.undo .~ block2Payload & U5c.tip .~ stubTip
  sent === (NextElem <$> [expectedReset, expectedApply1, expectedApply2, expectedUndo2])

-- | If the block a rollback would undo can no longer be re-fetched (the
-- VolatileDB has garbage-collected it - simulated here with 'fetchNever'),
-- 'followTipStream' stops the undo sequence and falls back to a single
-- @reset@ at the rollback point rather than emitting a partial or
-- incorrect undo.
hprop_follow_tip_stream_rollback_fetch_miss_falls_back_to_reset :: Property
hprop_follow_tip_stream_rollback_fetch_miss_falls_back_to_reset = H.propertyOnce $ do
  block1@(rawBytes1, blockInMode1@(BlockInMode _ rawBlock1)) <- mainBlockInMode
  block2 <- boundaryBlockInMode
  let BlockHeader slot1 headerHash1 _ = getBlockHeader rawBlock1
      point1 = ChainPoint slot1 headerHash1
      script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        , ChainRollBack point1
        ]
  sent <-
    expectScriptExhausted
      =<< runFollowTipStream script (Just ChainPointAtGenesis) fetchNever testUndoWindow [ChainPointAtGenesis]

  H.note_
    "reset(origin), apply(b1), apply(b2), reset(b1) - re-fetch missed, so undo falls back to reset"
  let (rawBytes2, blockInMode2) = block2
      expectedReset =
        defMessage & U5c.reset .~ chainPointToBlockRef ChainPointAtGenesis & U5c.tip .~ stubTip
      expectedApply1 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
      expectedApply2 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
          & U5c.tip .~ stubTip
      expectedRollbackReset =
        defMessage & U5c.reset .~ chainPointToBlockRef point1 & U5c.tip .~ stubTip
  sent === (NextElem <$> [expectedReset, expectedApply1, expectedApply2, expectedRollbackReset])

-- | A rollback landing exactly on the stream's start point (the window
-- floor) undoes everything currently tracked, newest first: with two
-- applied blocks, that is @undo(b2)@ then @undo(b1)@, in that order,
-- landing back at the start point.
hprop_follow_tip_stream_rollback_to_start_undoes_all_newest_first :: Property
hprop_follow_tip_stream_rollback_to_start_undoes_all_newest_first = H.propertyOnce $ do
  block1@(rawBytes1, blockInMode1@(BlockInMode _ rawBlock1)) <- mainBlockInMode
  block2@(rawBytes2, blockInMode2@(BlockInMode _ rawBlock2)) <- boundaryBlockInMode
  let BlockHeader slot1 headerHash1 _ = getBlockHeader rawBlock1
      BlockHeader slot2 headerHash2 _ = getBlockHeader rawBlock2
      point1 = ChainPoint slot1 headerHash1
      point2 = ChainPoint slot2 headerHash2
      script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        , ChainRollBack ChainPointAtGenesis
        ]
      fetchBlockByPoint = fetchKnownBlocks [(point1, block1), (point2, block2)]
  sent <-
    expectScriptExhausted
      =<< runFollowTipStream
        script
        (Just ChainPointAtGenesis)
        fetchBlockByPoint
        testUndoWindow
        [ChainPointAtGenesis]

  H.note_
    "reset(origin), apply(b1), apply(b2), undo(b2), undo(b1) - newest first, back to the start point"
  let expectedReset =
        defMessage & U5c.reset .~ chainPointToBlockRef ChainPointAtGenesis & U5c.tip .~ stubTip
      expectedApply1 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
      expectedApply2 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
          & U5c.tip .~ stubTip
      expectedUndo2 =
        defMessage
          & U5c.undo .~ oldAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
          & U5c.tip .~ stubTip
      expectedUndo1 =
        defMessage
          & U5c.undo .~ oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
  sent
    === (NextElem <$> [expectedReset, expectedApply1, expectedApply2, expectedUndo2, expectedUndo1])

-- | A rollback target that has fallen out of the tracking cap is treated as
-- outside the window even when the block itself would still be
-- re-fetchable: with @trackingCap = 1@, applying b1 then b2 evicts b1's
-- tracked point before the rollback arrives, so the rollback degrades to a
-- @reset@. The fetch stub below would succeed for b1's point if it were
-- ever asked, proving the fallback here is driven by the cap, not a fetch
-- miss (it is never called: 'followTipStream' recognises the target is
-- outside the window before attempting any re-fetch).
hprop_follow_tip_stream_rollback_beyond_cap_falls_back_to_reset :: Property
hprop_follow_tip_stream_rollback_beyond_cap_falls_back_to_reset = H.propertyOnce $ do
  block1@(rawBytes1, blockInMode1@(BlockInMode _ rawBlock1)) <- mainBlockInMode
  block2 <- boundaryBlockInMode
  let BlockHeader slot1 headerHash1 _ = getBlockHeader rawBlock1
      point1 = ChainPoint slot1 headerHash1
      script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        , ChainRollBack point1
        ]
      fetchBlockByPoint = fetchKnownBlocks [(point1, block1)]
  sent <-
    expectScriptExhausted
      =<< runFollowTipStream script (Just ChainPointAtGenesis) fetchBlockByPoint 1 [ChainPointAtGenesis]

  H.note_ "reset(origin), apply(b1), apply(b2), reset(b1) - b1's tracked point was evicted by the cap"
  let (rawBytes2, blockInMode2) = block2
      expectedReset =
        defMessage & U5c.reset .~ chainPointToBlockRef ChainPointAtGenesis & U5c.tip .~ stubTip
      expectedApply1 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
      expectedApply2 =
        defMessage
          & U5c.apply .~ oldAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
          & U5c.tip .~ stubTip
      expectedRollbackReset =
        defMessage & U5c.reset .~ chainPointToBlockRef point1 & U5c.tip .~ stubTip
  sent === (NextElem <$> [expectedReset, expectedApply1, expectedApply2, expectedRollbackReset])

-- | An absent mask reads the raw and the parsed block of each applied block, and
-- its point, but no separate header.
hprop_field_mask_follow_tip_select_all_reads_no_separate_header :: Property
hprop_field_mask_follow_tip_select_all_reads_no_separate_header = H.propertyOnce $ do
  block1 <- mainBlockInMode
  (_, ran) <- followMasked [] [ChainApply block1] fetchNever
  ran === ["GetSlot", "GetHash", "GetRawBlock", "GetBlock", "GetSlot", "slotTimestamp", "readTip"]

-- | A mask of just the tip keeps one message per change, each with only the tip, and
-- reads each applied block's point alone. The undo re-fetch only checks that the block is there.
hprop_field_mask_follow_tip_tip_only_keeps_every_message_and_reads_no_block :: Property
hprop_field_mask_follow_tip_tip_only_keeps_every_message_and_reads_no_block = H.propertyOnce $ do
  block1 <- mainBlockInMode
  block2 <- boundaryBlockInMode
  let script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        , ChainRollBack $ blockPoint block1
        ]
  (sent, ran) <- followMasked ["tip"] script $ fetchKnownBlocks [(blockPoint block2, block2)]
  sent === replicate 4 (NextElem $ defMessage & U5c.tip .~ stubTip)
  ran
    === [ "readTip"
        , "GetSlot"
        , "GetHash"
        , "readTip"
        , "GetSlot"
        , "GetHash"
        , "readTip"
        , "lookup"
        , "readTip"
        ]

-- | With @undo@ unselected, a block the undo can no longer re-fetch still turns the
-- rollback into a @reset@, and a block it can re-fetch still gives an @undo@ message.
hprop_field_mask_follow_tip_unselected_undo_keeps_the_reset_fallback :: Property
hprop_field_mask_follow_tip_unselected_undo_keeps_the_reset_fallback = H.propertyOnce $ do
  block1 <- mainBlockInMode
  block2 <- boundaryBlockInMode
  let point1 = blockPoint block1
      script =
        [ChainRollBack ChainPointAtGenesis, ChainApply block1, ChainApply block2, ChainRollBack point1]
      resetTo :: ChainPoint -> NextElem (Proto U5c.FollowTipResponse)
      resetTo point = NextElem $ defMessage & U5c.reset .~ chainPointToBlockRef point
      emptied = NextElem defMessage
  (missed, _) <- followMasked ["reset"] script fetchNever
  missed === [resetTo ChainPointAtGenesis, emptied, emptied, resetTo point1]
  (found, _) <- followMasked ["reset"] script $ fetchKnownBlocks [(blockPoint block2, block2)]
  found === [resetTo ChainPointAtGenesis, emptied, emptied, emptied]

-- | The native bytes of applied blocks read only the raw blocks and their points,
-- and leave the @reset@ message empty.
hprop_field_mask_follow_tip_apply_native_bytes_reads_only_raw_blocks :: Property
hprop_field_mask_follow_tip_apply_native_bytes_reads_only_raw_blocks = H.propertyOnce $ do
  block1 <- mainBlockInMode
  block2 <- boundaryBlockInMode
  (sent, ran) <-
    followMasked
      ["apply.native_bytes"]
      [ChainRollBack ChainPointAtGenesis, ChainApply block1, ChainApply block2]
      fetchNever
  let applied :: (ByteString, BlockInMode) -> NextElem (Proto U5c.FollowTipResponse)
      applied (rawBytes, _) = NextElem $ defMessage & U5c.apply . U5c.nativeBytes .~ rawBytes
  sent === [NextElem defMessage, applied block1, applied block2]
  ran === concat (replicate 2 ["GetSlot", "GetHash", "GetRawBlock"])

-- | A path below @undo@ decides what the undo re-fetch reads, and the follower reads
-- only the points of applied blocks.
hprop_field_mask_follow_tip_undo_mask_drives_the_refetch :: Property
hprop_field_mask_follow_tip_undo_mask_drives_the_refetch = H.propertyOnce $ do
  block1 <- mainBlockInMode
  block2 <- boundaryBlockInMode
  let script =
        [ ChainRollBack ChainPointAtGenesis
        , ChainApply block1
        , ChainApply block2
        , ChainRollBack ChainPointAtGenesis
        ]
      fetchBlockByPoint = fetchKnownBlocks [(blockPoint block1, block1), (blockPoint block2, block2)]
  (sent, ran) <- followMasked ["undo.cardano.header"] script fetchBlockByPoint
  let undone :: (ByteString, BlockInMode) -> NextElem (Proto U5c.FollowTipResponse)
      undone (rawBytes, blockInMode) =
        NextElem $
          defMessage
            & U5c.undo . U5c.cardano . U5c.header
              .~ oldAnyChainBlock rawBytes blockInMode stubTimestamp ^. U5c.cardano . U5c.header
      emptied = NextElem defMessage
  sent === [emptied, emptied, emptied, undone block2, undone block1]
  ran === ["GetSlot", "GetHash", "GetSlot", "GetHash", "lookup", "GetHeader", "lookup", "GetHeader"]

-- | Nested paths trim every message, and keep which action it is.
hprop_field_mask_follow_tip_nested_paths_trim_every_message :: Property
hprop_field_mask_follow_tip_nested_paths_trim_every_message = H.propertyOnce $ do
  block1@(rawBytes1, blockInMode1) <- mainBlockInMode
  (sent, _) <-
    followMasked
      ["apply.cardano.header.height", "reset.hash", "tip.slot"]
      [ChainRollBack ChainPointAtGenesis, ChainApply block1]
      fetchNever
  let tipSlot :: Proto U5c.BlockRef
      tipSlot = defMessage & U5c.slot .~ stubTip ^. U5c.slot
      height = oldAnyChainBlock rawBytes1 blockInMode1 stubTimestamp ^. U5c.cardano . U5c.header . U5c.height
  H.assertWith height (/= 0)
  sent
    === [ NextElem $ defMessage & U5c.reset .~ defMessage & U5c.tip .~ tipSlot
        , NextElem $
            defMessage
              & U5c.apply . U5c.cardano . U5c.header . U5c.height .~ height
              & U5c.tip .~ tipSlot
        ]

-- | The parts of a block that the mask leaves out are never evaluated.
hprop_field_mask_follow_tip_unselected_block_parts_are_not_evaluated :: Property
hprop_field_mask_follow_tip_unselected_block_parts_are_not_evaluated = H.propertyOnce $ do
  stored <- toStoredBlock <$> mainBlockInMode
  let unparsed = stored{storedBlockInMode = error "forced"}
      unread = unparsed{storedBlockRawBytes = error "forced"}
      fieldMask = resolve $ getProto (followTipRequest ["apply.native_bytes"]) ^. U5c.fieldMask . FieldMask.paths
      doSlotTimestamp = const $ pure @Identity stubTimestamp
      ((point, doBuildApplied), appliedRan) =
        runWriter . readRecordedComponent unparsed $ followTipAppliedRead doSlotTimestamp fieldMask
      (doBuildUndone, undoneRan) =
        runWriter . readRecordedComponent unread $ followTipUndoneRead doSlotTimestamp fieldMask
  appliedRan === ["GetSlot", "GetHash", "GetRawBlock"]
  point === storedBlockPoint stored
  applied <- H.evalNF $ runIdentity doBuildApplied
  applied === (defMessage & U5c.nativeBytes .~ storedBlockRawBytes stored)
  undoneRan === []
  undone <- H.evalNF $ runIdentity doBuildUndone
  undone === defMessage

-- | Run 'followTipStream' against a 'scriptedFollower' and the stub tip and
-- timestamp above, capturing every message sent until the run ends - either
-- because the script was exhausted ('ScriptExhausted') or because
-- 'followTipStream' itself threw (e.g. the @NOT_FOUND@ 'GrpcException' from a
-- failed intersection). Messages captured before an exception are returned
-- alongside it, so callers can assert "nothing was sent before this error"
-- as well as "these messages were sent, in this order".
runFollowTipStream
  :: MonadIO m
  => [ChainChange (ByteString, BlockInMode)]
  -- ^ Script 'scriptedFollower's @nextChange@ plays back
  -> Maybe ChainPoint
  -- ^ Fixed @findIntersect@ result
  -> (ChainPoint -> IO (Maybe (ByteString, BlockInMode)))
  -- ^ Block-by-point fetch stub for undo re-fetch, e.g. 'fetchNever' or
  -- 'fetchKnownBlocks'
  -> Int
  -- ^ Tracking cap passed to 'followTipStream'
  -> [ChainPoint]
  -- ^ Start points passed to 'followTipStream'
  -> m (Either SomeException (), [NextElem (Proto U5c.FollowTipResponse)])
runFollowTipStream script intersectResult fetchBlockByPoint trackingCap startPoints = do
  (outcome, sent, _) <-
    runMaskedFollowTipStream [] script intersectResult fetchBlockByPoint trackingCap startPoints
  pure (outcome, sent)

-- | 'runFollowTipStream' for a request with the mask paths, from origin, asserting
-- that the script ran out, and returning the messages sent and the log.
followMasked
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => [FieldPath]
  -- ^ Field mask paths, rooted at each response
  -> [ChainChange (ByteString, BlockInMode)]
  -- ^ Script 'scriptedFollower's @nextChange@ plays back
  -> (ChainPoint -> IO (Maybe (ByteString, BlockInMode)))
  -- ^ Block-by-point fetch stub for undo re-fetch
  -> m ([NextElem (Proto U5c.FollowTipResponse)], [Text])
followMasked paths script fetchBlockByPoint = withFrozenCallStack $ do
  (outcome, sent, ran) <-
    runMaskedFollowTipStream
      paths
      script
      (Just ChainPointAtGenesis)
      fetchBlockByPoint
      testUndoWindow
      [ChainPointAtGenesis]
  sentUntilExhausted <- expectScriptExhausted (outcome, sent)
  pure (sentUntilExhausted, ran)

-- | 'runFollowTipStream' for a request with the mask paths, reading each block
-- through the handler's own block reads. Also returns the log of each ChainDB
-- lookup, block component read, timestamp conversion and tip read.
runMaskedFollowTipStream
  :: MonadIO m
  => [FieldPath]
  -- ^ Field mask paths, rooted at each response
  -> [ChainChange (ByteString, BlockInMode)]
  -- ^ Script 'scriptedFollower's @nextChange@ plays back
  -> Maybe ChainPoint
  -- ^ Fixed @findIntersect@ result
  -> (ChainPoint -> IO (Maybe (ByteString, BlockInMode)))
  -- ^ Block-by-point fetch stub for undo re-fetch
  -> Int
  -- ^ Tracking cap passed to 'followTipStream'
  -> [ChainPoint]
  -- ^ Start points passed to 'followTipStream'
  -> m (Either SomeException (), [NextElem (Proto U5c.FollowTipResponse)], [Text])
runMaskedFollowTipStream paths script intersectResult fetchBlockByPoint trackingCap startPoints = liftIO $ do
  logRef <- newIORef []
  let fieldMask = resolve $ getProto (followTipRequest paths) ^. U5c.fieldMask . FieldMask.paths
      doSlotTimestamp = const . logRecorded logRef $ recorded "slotTimestamp" stubTimestamp
      doRefetch point = do
        logRecorded logRef $ recorded "lookup" ()
        fetched <- fetchBlockByPoint point
        forM fetched . readFixture logRef $ followTipUndoneRead doSlotTimestamp fieldMask
  follower <-
    scriptedFollower
      (readFixture logRef $ followTipAppliedRead doSlotTimestamp fieldMask)
      script
      intersectResult
  sentRef <- newIORef []
  outcome <-
    try @IO @SomeException $
      followTipStream
        follower
        (logRecorded logRef . recorded "readTip" . Just $ getProto stubTip)
        doRefetch
        trackingCap
        fieldMask
        (\nextElem -> modifyIORef' sentRef (nextElem :))
        startPoints
  sent <- reverse <$> readIORef sentRef
  ran <- readIORef logRef
  pure (outcome, sent, ran)

-- | Assert that a 'runFollowTipStream' outcome ended because the script was
-- exhausted - the expected, clean termination for a script that runs out
-- rather than one that hit an error - and return the messages sent.
expectScriptExhausted
  :: HasCallStack
  => MonadTest m
  => (Either SomeException (), [NextElem (Proto U5c.FollowTipResponse)])
  -> m [NextElem (Proto U5c.FollowTipResponse)]
expectScriptExhausted (outcome, sent) = withFrozenCallStack $ do
  case outcome of
    Left e | Just ScriptExhausted <- fromException e -> pure ()
    _ -> do
      H.note_ "Expected the run to end with ScriptExhausted"
      H.annotateShow outcome
      H.failure
  pure sent

-- | A 'ChainFollower' whose @nextChange@ pops messages off the given script
-- in order, held in a mutable ref (@tryNextChange@ pops the same script,
-- with 'Nothing' once it is empty), reading the payload of each popped
-- 'ChainApply' with the given action, and whose @findIntersect@ always returns
-- the given fixed result regardless of the points it is asked about - the
-- properties below assert on the points passed to 'followTipStream' and on
-- the streamed messages, not on what @findIntersect@ does with its argument.
scriptedFollower
  :: MonadIO m
  => (fixture -> IO a)
  -- ^ Read the payload of an applied block from its fixture
  -> [ChainChange fixture]
  -> Maybe ChainPoint
  -> m (ChainFollower a)
scriptedFollower doReadPayload script intersectResult = do
  scriptRef <- liftIO $ newIORef script
  pure
    ChainFollower
      { nextChange =
          liftIO $
            readIORef scriptRef >>= \case
              [] -> throwIO ScriptExhausted
              change : rest -> writeIORef scriptRef rest >> readChangePayload doReadPayload change
      , tryNextChange =
          liftIO $
            readIORef scriptRef >>= \case
              [] -> pure Nothing
              change : rest ->
                writeIORef scriptRef rest >> Just <$> readChangePayload doReadPayload change
      , findIntersect = const $ pure intersectResult
      }

-- | Read the payload of a 'ChainApply' with the given action.
readChangePayload
  :: Applicative f
  => (fixture -> f a)
  -- ^ Read the payload of an applied block from its fixture
  -> ChainChange fixture
  -> f (ChainChange a)
readChangePayload doReadPayload = \case
  ChainApply fixture -> ChainApply <$> doReadPayload fixture
  ChainRollBack point -> pure $ ChainRollBack point

-- | Read block components from a fixture block, appending each component read to the log.
readFixture
  :: IORef [Text]
  -- ^ The log
  -> CardanoBlockComponent a
  -- ^ The components to read
  -> (ByteString, BlockInMode)
  -- ^ The raw block and the same block parsed
  -> IO a
readFixture logRef component = logRecorded logRef . (`readRecordedComponent` component) . toStoredBlock

-- | A block-by-point fetch stub that never finds anything - the simplest
-- 'followTipStream' fetch parameter for scripts that either never roll back
-- into the tracked window, or where the fetch-miss (@reset@ fallback) path
-- is exactly what is under test.
fetchNever :: MonadIO m => ChainPoint -> m (Maybe (ByteString, BlockInMode))
fetchNever = const $ pure Nothing

-- | A block-by-point fetch stub backed by a fixed association list of known
-- blocks (keyed by their own 'ChainPoint'), used to simulate a successful
-- ChainDB re-fetch for undo delivery. Looks up 'mempty' (i.e. 'Nothing') for
-- any point not in the list.
fetchKnownBlocks
  :: MonadIO m
  => [(ChainPoint, (ByteString, BlockInMode))]
  -- ^ Known blocks table, mapping chain points to their raw bytes and parsed block wrappers
  -> ChainPoint
  -- ^ Chain point to fetch the block for
  -> m (Maybe (ByteString, BlockInMode))
  -- ^ Block data (raw bytes and parsed block) for the given point if found, 'Nothing' otherwise
fetchKnownBlocks table point = pure $ lookup point table

-- | A fixed stub tip, returned regardless of stream position: the properties
-- below only assert that every message carries it, not on tip content.
stubTip :: Proto U5c.BlockRef
stubTip = defMessage & U5c.slot .~ 999 & U5c.hash .~ SBS.fromShort (SBS.replicate 32 0x99)

-- | A tracking cap comfortably above the two-block scripts used below, for
-- tests that are not themselves about cap-overflow behaviour.
testUndoWindow :: Int
testUndoWindow = 100

-- | A @FollowTip@ request carrying the mask paths.
followTipRequest
  :: [FieldPath]
  -- ^ Field mask paths, rooted at each response
  -> Proto U5c.FollowTipRequest
followTipRequest paths = defMessage & U5c.fieldMask . FieldMask.paths .~ paths

-- | The chain point of a fixture block.
blockPoint :: (ByteString, BlockInMode) -> ChainPoint
blockPoint = uncurry ChainPoint . storedBlockPoint . toStoredBlock

-- | Sentinel exception 'scriptedFollower's @nextChange@ throws once its
-- script is exhausted. 'followTipStream' loops via 'forever' and never
-- returns normally, so a distinctive exception is the simplest deterministic
-- way to end a scripted run: 'runFollowTipStream' catches exactly this
-- exception (and no other) to recognise "the script played out cleanly, with
-- no unexpected error along the way".
data ScriptExhausted = ScriptExhausted
  deriving Show

instance Exception ScriptExhausted
