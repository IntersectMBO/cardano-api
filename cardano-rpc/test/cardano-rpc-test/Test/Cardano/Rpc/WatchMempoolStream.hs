{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit coverage for 'watchMempoolStream', the @WatchMempool@ streaming
-- loop extracted from
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.Mempool.watchMempoolMethod'. The
-- loop takes its collaborators as plain arguments instead of closing over
-- 'Cardano.Rpc.Server.NodeKernelAccess.NodeKernelAccess', so it can be
-- driven here with scripted mempool snapshots, without a live mempool -
-- mirroring 'Test.Cardano.Rpc.FollowTipStream'.
module Test.Cardano.Rpc.WatchMempoolStream where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Mempool (watchMempoolStream)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool
  ( txInMempoolMaskTable
  , txInModeToTxInMempool
  )
import Cardano.Rpc.Server.NodeKernelAccess (MempoolWatchSnapshot (..))

import Ouroboros.Consensus.Mempool.API qualified as Consensus (TicketNo)

import RIO

import Data.List (sort)
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Message (fieldsByTextFormatName)
import Data.Text qualified as Text
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (NextElem (..), Proto (..))

import Test.Gen.Cardano.Api.Typed (genTx)

import Hedgehog as H
import Hedgehog.Extras qualified as H

-- | An empty predicate matches everything ('matchesTxPredicate's default),
-- and there is no field mask, so every mempool entry newer than the last
-- one seen is streamed, oldest first, across as many snapshot transitions
-- as it takes to see them all - one message per new entry, in ticket order.
hprop_watch_mempool_stream_emits_new_entries_in_order :: Property
hprop_watch_mempool_stream_emits_new_entries_in_order = H.property $ do
  tx1 <- txInModeFixture
  tx2 <- txInModeFixture
  let script =
        [ mkSnapshot [(tx1, ticket 1)]
        , mkSnapshot [(tx1, ticket 1), (tx2, ticket 2)]
        ]
  sent <- expectScriptExhausted =<< runWatchMempoolStream script defMessage []

  expected1 <- expectedResponse tx1
  expected2 <- expectedResponse tx2
  H.note_ "tx1 from the initial snapshot, then tx2 once it appears - each exactly once, in order"
  sent === (NextElem <$> [expected1, expected2])

-- | A predicate that matches nothing ('not' wrapping the always-true
-- default match) drops every entry, but ticket tracking still advances -
-- no entry is re-offered on the next snapshot transition either.
hprop_watch_mempool_stream_predicate_filters_all_entries :: Property
hprop_watch_mempool_stream_predicate_filters_all_entries = H.property $ do
  tx1 <- txInModeFixture
  tx2 <- txInModeFixture
  let script =
        [ mkSnapshot [(tx1, ticket 1)]
        , mkSnapshot [(tx1, ticket 1), (tx2, ticket 2)]
        ]
      matchesNothing = defMessage & U5c.not .~ [defMessage]
  sent <- expectScriptExhausted =<< runWatchMempoolStream script matchesNothing []

  H.note_ "Nothing is ever sent: the predicate rejects every entry"
  sent === []

-- | Requesting a field mask of just @stage@ prunes every other top-level
-- field of the sent 'U5c.TxInMempool' back to its default.
hprop_watch_mempool_stream_prunes_by_field_mask :: Property
hprop_watch_mempool_stream_prunes_by_field_mask = H.property $ do
  tx1 <- txInModeFixture
  let script = [mkSnapshot [(tx1, ticket 1)]]
  sent <- expectScriptExhausted =<< runWatchMempoolStream script defMessage ["stage"]

  H.note_ "Only 'stage' survives pruning; 'ref'/'native_bytes'/'cardano' are back to their defaults"
  let expectedPruned = defMessage & U5c.stage .~ Proto U5c.STAGE_MEMPOOL
  sent === [NextElem (defMessage & U5c.tx .~ expectedPruned)]

-- | Field names in 'txInMempoolMaskTable' must match the proto descriptor
-- names for 'U5c.TxInMempool', and the count must remain in sync (a new
-- proto field becomes a compile error, not a silent omission).
hprop_tx_in_mempool_mask_table_matches_proto :: Property
hprop_tx_in_mempool_mask_table_matches_proto = H.propertyOnce $ do
  let descriptorNames = Map.keys $ fieldsByTextFormatName @U5c.TxInMempool
      tableNames = Text.unpack . fst <$> txInMempoolMaskTable
  length tableNames === length descriptorNames
  sort tableNames === sort descriptorNames

-- | A removal (nothing newer than the last seen ticket) produces no message,
-- and no entry is re-sent across it: the loop only ever sees tx1 once, from
-- the initial snapshot.
hprop_watch_mempool_stream_no_duplicates_across_removal :: Property
hprop_watch_mempool_stream_no_duplicates_across_removal = H.property $ do
  tx1 <- txInModeFixture
  let script =
        [ mkSnapshot [(tx1, ticket 1)]
        , mkSnapshot []
        , mkSnapshot []
        ]

  sent <- expectScriptExhausted =<< runWatchMempoolStream script defMessage []

  expected1 <- expectedResponse tx1
  H.note_ "tx1 exactly once, from the initial snapshot - the later transitions send nothing"
  sent === [NextElem expected1]

-- | A fixed Conway 'TxInMode', freshly generated per use - any two calls in
-- the same property are independent random samples of 'genTx', so in
-- practice distinct, giving each test case genuinely different fixtures
-- without needing checked-in golden transactions (unlike
-- 'Test.Cardano.Rpc.FollowTipStream's block fixtures, which must be
-- byte-identical to their own golden files).
txInModeFixture :: PropertyT IO TxInMode
txInModeFixture = do
  tx <- forAll $ genTx ShelleyBasedEraConway
  pure $ TxInMode ShelleyBasedEraConway tx

-- | The 'U5c.WatchMempoolResponse' 'watchMempoolStream' is expected to send
-- for a given fixture, via the same conversion the loop itself uses.
expectedResponse :: HasCallStack => MonadTest m => TxInMode -> m (Proto U5c.WatchMempoolResponse)
expectedResponse txInMode = do
  txInMempool <- H.nothingFail $ txInModeToTxInMempool txInMode
  pure $ defMessage & U5c.tx .~ txInMempool

-- | Build a scripted snapshot whose 'mempoolWatchTxsAfter' filters the given
-- entries, like a real 'Ouroboros.Consensus.Mempool.API.snapshotTxsAfter'
-- after conversion.
mkSnapshot :: [(TxInMode, Consensus.TicketNo)] -> MempoolWatchSnapshot
mkSnapshot entries =
  MempoolWatchSnapshot
    { mempoolWatchTxsAfter = \lastSeenTicket -> filter ((> lastSeenTicket) . snd) entries
    }

-- | A ticket number built via the 'Enum' instance derived from 'Word64' -
-- 'Consensus.TicketNo's constructor isn't exported, but tests never need
-- to construct anything past what scripted snapshots use it for.
ticket :: Int -> Consensus.TicketNo
ticket = toEnum

-- | Run 'watchMempoolStream' against a scripted snapshot sequence,
-- capturing every message sent until the script is exhausted ('ScriptExhausted').
runWatchMempoolStream
  :: MonadIO m
  => [MempoolWatchSnapshot]
  -- ^ Script: the first snapshot is the initial read, each subsequent one
  -- is what the next blocking wait returns, in order
  -> Proto U5c.TxPredicate
  -> [Text]
  -- ^ Field mask paths
  -> m (Either SomeException (), [NextElem (Proto U5c.WatchMempoolResponse)])
runWatchMempoolStream script predicate fieldMaskPaths = liftIO $ do
  scriptRef <- newIORef script
  let pop =
        readIORef scriptRef >>= \case
          [] -> throwIO ScriptExhausted
          snapshot : rest -> writeIORef scriptRef rest $> snapshot
  sentRef <- newIORef []
  outcome <-
    try @IO @SomeException $
      watchMempoolStream
        pop
        (const pop)
        predicate
        fieldMaskPaths
        (\nextElem -> modifyIORef' sentRef (nextElem :))
  sent <- reverse <$> readIORef sentRef
  pure (outcome, sent)

-- | Assert that a 'runWatchMempoolStream' outcome ended because the script
-- was exhausted, and return the messages sent.
expectScriptExhausted
  :: HasCallStack
  => MonadTest m
  => (Either SomeException (), [NextElem (Proto U5c.WatchMempoolResponse)])
  -> m [NextElem (Proto U5c.WatchMempoolResponse)]
expectScriptExhausted (outcome, sent) = withFrozenCallStack $ do
  case outcome of
    Left e | Just ScriptExhausted <- fromException e -> pure ()
    _ -> do
      H.note_ "Expected the run to end with ScriptExhausted"
      H.annotateShow outcome
      H.failure
  pure sent

data ScriptExhausted = ScriptExhausted
  deriving Show

instance Exception ScriptExhausted
