{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit coverage for 'waitForTxStream', the @WaitForTx@ per-ref state
-- machine extracted from
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.WaitForTx.waitForTxMethod'. The loop
-- takes its merged mempool\/follower event source as a plain argument, so it
-- can be driven here with a scripted event list, no live mempool or ChainDB
-- required (mirrors @Test.Cardano.Rpc.FollowTipStream@).
module Test.Cardano.Rpc.WaitForTxStream where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.WaitForTx (WaitForTxEvent (..), waitForTxStream)

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (NextElem (..), Proto (Proto))

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | The normal happy path: a ref is seen in the mempool, then confirmed in a
-- block. The stream closes ('NoNextElem') right after the confirming event
-- - it never calls 'nextEvent' a third time, since the script here has
-- exactly two events and 'runWaitForTxStream' asserts a clean, non-exhausted
-- return.
hprop_wait_for_tx_stream_mempool_then_confirmed :: Property
hprop_wait_for_tx_stream_mempool_then_confirmed = H.propertyOnce $ do
  let script =
        [ MempoolSnapshotChanged (Set.singleton txIdA)
        , BlockApplied (Set.singleton txIdA)
        ]
  sent <- expectScriptNotExhausted =<< runWaitForTxStream script (Set.singleton txIdA)

  H.note_ "mempool(A), confirmed(A), then the stream closes on its own"
  sent
    === (NextElem <$> [response txIdA U5c.STAGE_MEMPOOL, response txIdA U5c.STAGE_CONFIRMED])
      <> [NoNextElem]

-- | A ref can be confirmed without ever being observed in the mempool (e.g.
-- it was included before the mempool watcher's first read landed) - no
-- 'STAGE_MEMPOOL' is required before 'STAGE_CONFIRMED'.
hprop_wait_for_tx_stream_confirmation_without_mempool_sighting :: Property
hprop_wait_for_tx_stream_confirmation_without_mempool_sighting = H.propertyOnce $ do
  let script = [BlockApplied (Set.singleton txIdA)]
  sent <- expectScriptNotExhausted =<< runWaitForTxStream script (Set.singleton txIdA)

  H.note_ "confirmed(A) with no preceding mempool(A)"
  sent === [NextElem (response txIdA U5c.STAGE_CONFIRMED), NoNextElem]

-- | A ref that is evicted from the mempool (absent from a later snapshot)
-- and then re-added does not get a second 'STAGE_MEMPOOL' - dedupe is keyed
-- on the ref ever having been seen, not on its latest membership.
hprop_wait_for_tx_stream_eviction_then_readd_does_not_redeliver_mempool_stage :: Property
hprop_wait_for_tx_stream_eviction_then_readd_does_not_redeliver_mempool_stage = H.propertyOnce $ do
  let script =
        [ MempoolSnapshotChanged (Set.singleton txIdA)
        , MempoolSnapshotChanged Set.empty
        , MempoolSnapshotChanged (Set.singleton txIdA)
        , BlockApplied (Set.singleton txIdA)
        ]
  sent <- expectScriptNotExhausted =<< runWaitForTxStream script (Set.singleton txIdA)

  H.note_ "exactly one mempool(A), despite two mempool-changed events reporting A present"
  sent
    === [ NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]

-- | With two requested refs, the stream keeps waiting until *both* reach
-- @STAGE_CONFIRMED@ - confirming only one does not close it.
hprop_wait_for_tx_stream_multiple_refs_close_only_when_all_confirmed :: Property
hprop_wait_for_tx_stream_multiple_refs_close_only_when_all_confirmed = H.propertyOnce $ do
  let script =
        [ BlockApplied (Set.singleton txIdA)
        , MempoolSnapshotChanged (Set.singleton txIdB)
        , BlockApplied (Set.singleton txIdB)
        ]
  sent <- expectScriptNotExhausted =<< runWaitForTxStream script (Set.fromList [txIdA, txIdB])

  H.note_ "confirmed(A) alone does not close the stream; it closes only once B is confirmed too"
  sent
    === [ NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NextElem (response txIdB U5c.STAGE_MEMPOOL)
        , NextElem (response txIdB U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]

-- | An empty ref set closes the stream immediately, without ever calling
-- 'nextEvent' - proven here by giving it an empty script: if the loop tried
-- to block for an event it would hit 'ScriptExhausted' instead of returning
-- cleanly.
hprop_wait_for_tx_stream_empty_refs_closes_without_blocking :: Property
hprop_wait_for_tx_stream_empty_refs_closes_without_blocking = H.propertyOnce $ do
  sent <- expectScriptNotExhausted =<< runWaitForTxStream [] Set.empty

  H.note_ "no events consumed, a single NoNextElem sent"
  sent === [NoNextElem]

-- | Two distinct fixture transaction ids, for scripts that need to
-- distinguish which ref an event is about.
txIdA, txIdB :: TxId
txIdA = mkTxId 0xAA
txIdB = mkTxId 0xBB

-- | Build a fixture 'TxId' from a repeated byte, at the width 'TxId's
-- underlying hash expects.
mkTxId :: Word8 -> TxId
mkTxId byte =
  either (error . show) id . deserialiseFromRawBytes AsTxId $ BS.replicate 32 byte

-- | The @WaitForTxResponse@ 'waitForTxStream' sends for a ref reaching a
-- stage.
response :: TxId -> U5c.Stage -> Proto U5c.WaitForTxResponse
response ref stage =
  defMessage
    & U5c.ref .~ serialiseToRawBytes ref
    & U5c.stage .~ Proto stage

-- | Run 'waitForTxStream' against a scripted event source, capturing every
-- message sent until the run ends - either because 'waitForTxStream'
-- returned on its own (all refs confirmed, or an empty ref set) or because
-- the script ran out ('ScriptExhausted').
runWaitForTxStream
  :: MonadIO m
  => [WaitForTxEvent]
  -- ^ Script the scripted 'nextEvent' plays back, in order
  -> Set TxId
  -- ^ Refs passed to 'waitForTxStream'
  -> m (Either SomeException (), [NextElem (Proto U5c.WaitForTxResponse)])
runWaitForTxStream script refs = liftIO $ do
  scriptRef <- newIORef script
  let nextEvent =
        readIORef scriptRef >>= \case
          [] -> throwIO ScriptExhausted
          event : rest -> writeIORef scriptRef rest $> event
  sentRef <- newIORef []
  outcome <-
    try @IO @SomeException $
      waitForTxStream nextEvent (\nextElem -> modifyIORef' sentRef (nextElem :)) refs
  sent <- reverse <$> readIORef sentRef
  pure (outcome, sent)

-- | Assert that a 'runWaitForTxStream' outcome ended because
-- 'waitForTxStream' returned normally - not because the script ran out -
-- and return the messages sent.
expectScriptNotExhausted
  :: HasCallStack
  => MonadTest m
  => (Either SomeException (), [NextElem (Proto U5c.WaitForTxResponse)])
  -> m [NextElem (Proto U5c.WaitForTxResponse)]
expectScriptNotExhausted (outcome, sent) = withFrozenCallStack $ do
  case outcome of
    Right () -> pure ()
    Left e -> do
      H.note_ "Expected waitForTxStream to return normally, not throw"
      H.annotateShow e
      H.failure
  pure sent

-- | Sentinel exception the scripted 'nextEvent' throws once its script is
-- exhausted, so a test whose script is deliberately too short to reach
-- completion fails loudly instead of hanging.
data ScriptExhausted = ScriptExhausted
  deriving Show

instance Exception ScriptExhausted
