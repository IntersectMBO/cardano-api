{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit coverage for 'followTipStream', the @FollowTip@ streaming loop
-- extracted from 'Cardano.Rpc.Server.Internal.UtxoRpc.Sync.followTipMethod'.
-- The loop takes its collaborators as plain arguments instead of closing over
-- 'Cardano.Rpc.Server.NodeKernelAccess.NodeKernelAccess', so it can be
-- driven here with a scripted 'ChainFollower' and stub tip/timestamp
-- capabilities, without a live ChainDB.
module Test.Cardano.Rpc.FollowTipStream where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Sync (followTipStream)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Block (mkAnyChainBlock)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.ChainPoint (chainPointToBlockRef)
import Cardano.Rpc.Server.NodeKernelAccess (ChainChange (..), ChainFollower (..))

import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Ouroboros.Consensus.Byron.Ledger (mkByronBlock)

import RIO

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (GrpcError (GrpcNotFound), GrpcException (..), NextElem (..), Proto)

import Test.Cardano.Rpc.ByronTx (decodeBlockOrBoundaryFixture)

import Hedgehog as H
import Hedgehog.Extras qualified as H

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
      =<< runFollowTipStream script (Just ChainPointAtGenesis) [ChainPointAtGenesis]

  H.note_ "reset(origin), apply(b1), apply(b2), each carrying the stub tip, in order"
  let expectedReset =
        defMessage & U5c.reset .~ chainPointToBlockRef ChainPointAtGenesis & U5c.tip .~ stubTip
      expectedApply1 =
        defMessage
          & U5c.apply .~ mkAnyChainBlock rawBytes1 blockInMode1 stubTimestamp
          & U5c.tip .~ stubTip
      expectedApply2 =
        defMessage
          & U5c.apply .~ mkAnyChainBlock rawBytes2 blockInMode2 stubTimestamp
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
      =<< runFollowTipStream script (Just intersectionPoint) [intersectionPoint]

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
  (outcome, sent) <- runFollowTipStream [] Nothing [unknownPoint]

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

-- | Run 'followTipStream' against a 'scriptedFollower' and the stub tip and
-- timestamp above, capturing every message sent until the run ends - either
-- because the script was exhausted ('ScriptExhausted') or because
-- 'followTipStream' itself threw (e.g. the @NOT_FOUND@ 'GrpcException' from a
-- failed intersection). Messages captured before an exception are returned
-- alongside it, so callers can assert "nothing was sent before this error"
-- as well as "these messages were sent, in this order".
runFollowTipStream
  :: MonadIO m
  => [ChainChange]
  -- ^ Script 'scriptedFollower's @nextChange@ plays back
  -> Maybe ChainPoint
  -- ^ Fixed @findIntersect@ result
  -> [ChainPoint]
  -- ^ Start points passed to 'followTipStream'
  -> m (Either SomeException (), [NextElem (Proto U5c.FollowTipResponse)])
runFollowTipStream script intersectResult startPoints = liftIO $ do
  follower <- scriptedFollower script intersectResult
  sentRef <- newIORef []
  outcome <-
    try @IO @SomeException $
      followTipStream
        follower
        (pure (Just stubTip))
        (const (pure stubTimestamp))
        (\nextElem -> modifyIORef' sentRef (nextElem :))
        startPoints
  sent <- reverse <$> readIORef sentRef
  pure (outcome, sent)

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
-- in order, held in a mutable ref, and whose @findIntersect@ always returns
-- the given fixed result regardless of the points it is asked about - the
-- properties below assert on the points passed to 'followTipStream' and on
-- the streamed messages, not on what @findIntersect@ does with its argument.
scriptedFollower :: MonadIO m => [ChainChange] -> Maybe ChainPoint -> m ChainFollower
scriptedFollower script intersectResult = do
  scriptRef <- liftIO $ newIORef script
  pure
    ChainFollower
      { nextChange =
          liftIO $
            readIORef scriptRef >>= \case
              [] -> throwIO ScriptExhausted
              change : rest -> writeIORef scriptRef rest $> change
      , findIntersect = const $ pure intersectResult
      }

-- | A fixed stub tip, returned regardless of stream position: the properties
-- below only assert that every message carries it, not on tip content.
stubTip :: Proto U5c.BlockRef
stubTip = defMessage & U5c.slot .~ 999 & U5c.hash .~ SBS.fromShort (SBS.replicate 32 0x99)

-- | A fixed stub slot timestamp, returned regardless of the slot asked for.
stubTimestamp :: UTCTime
stubTimestamp = posixSecondsToUTCTime 1700000000

-- | A genuine Byron 'BlockInMode' decoded from the mainnet main-block golden
-- fixture also used by 'Test.Cardano.Rpc.ByronTx'. 'followTipStream'
-- assembles the full @AnyChainBlock@ via 'mkAnyChainBlock' for every
-- 'ChainApply', so the scripted blocks must be real, valid blocks rather
-- than placeholders; decoding an existing fixture through the same
-- 'mkByronBlock' smart constructor the node's ChainDB uses is the cheapest
-- way to get one, and reuses a fixture already checked in for this purpose.
mainBlockInMode :: HasCallStack => MonadTest m => MonadIO m => m (ByteString, BlockInMode)
mainBlockInMode = do
  rawBytes <-
    liftIO $ LBS.toStrict <$> LBS.readFile "test/cardano-rpc-test/files/golden/byron-main-block.cbor"
  blockOrBoundary <- decodeBlockOrBoundaryFixture (LBS.fromStrict rawBytes)
  pure (rawBytes, BlockInMode ByronEra (ByronBlock (mkByronBlock mainnetEpochSlots blockOrBoundary)))

-- | A genuine Byron 'BlockInMode' decoded from the mainnet epoch-boundary
-- golden fixture also used by 'Test.Cardano.Rpc.ByronTx'; see
-- 'mainBlockInMode'. Distinct from it (a boundary block instead of a main
-- block, at a different slot and hash), so a two-block script has two
-- genuinely different blocks to distinguish, for free.
boundaryBlockInMode :: HasCallStack => MonadTest m => MonadIO m => m (ByteString, BlockInMode)
boundaryBlockInMode = do
  compressedBytes <- liftIO $ LBS.readFile "test/cardano-rpc-test/files/golden/byron-ebb.cbor.gz"
  let rawBytes = LBS.toStrict $ GZip.decompress compressedBytes
  blockOrBoundary <- decodeBlockOrBoundaryFixture (LBS.fromStrict rawBytes)
  pure (rawBytes, BlockInMode ByronEra (ByronBlock (mkByronBlock mainnetEpochSlots blockOrBoundary)))

-- | Sentinel exception 'scriptedFollower's @nextChange@ throws once its
-- script is exhausted. 'followTipStream' loops via 'forever' and never
-- returns normally, so a distinctive exception is the simplest deterministic
-- way to end a scripted run: 'runFollowTipStream' catches exactly this
-- exception (and no other) to recognise "the script played out cleanly, with
-- no unexpected error along the way".
data ScriptExhausted = ScriptExhausted
  deriving Show

instance Exception ScriptExhausted
