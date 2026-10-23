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
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as Query
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.FieldMask
import Cardano.Rpc.Server.Internal.UtxoRpc.Mempool (watchMempoolFieldMask, watchMempoolStream)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Mempool
  ( TxInMempoolFields (..)
  , mkWatchMempoolResponse
  , txInModeToTxInMempool
  , txInModeToTxInMempoolFields
  )
import Cardano.Rpc.Server.NodeKernelAccess (MempoolWatchSnapshot (..))

import Ouroboros.Consensus.Mempool.API qualified as Consensus (TicketNo)

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (NextElem (..), Proto (..), getProto)

import Test.Gen.Cardano.Api.Typed (genTx)

import Hedgehog as H
import Hedgehog.Extras qualified as H

import Proto.Google.Protobuf.FieldMask_Fields qualified as FieldMask (paths)

-- | An empty predicate matches everything ('matchesTxPredicate's default),
-- so every mempool entry newer than the last one seen is streamed, oldest
-- first, across as many snapshot transitions as it takes to see them all -
-- one message per new entry, in ticket order.
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

-- | A mask of just @tx.native_bytes@ prunes every other field of the sent
-- response back to its default.
hprop_watch_mempool_stream_prunes_by_field_mask :: Property
hprop_watch_mempool_stream_prunes_by_field_mask = H.property $ do
  tx1 <- txInModeFixture
  let script = [mkSnapshot [(tx1, ticket 1)]]
  sent <- expectScriptExhausted =<< runWatchMempoolStream script defMessage ["tx.native_bytes"]

  full <- expectedResponse tx1
  let expected = defMessage & U5c.tx . U5c.nativeBytes .~ getProto full ^. U5c.tx . U5c.nativeBytes
  sent === [NextElem expected]

-- | A path below @tx.cardano@ keeps only that part of the converted transaction.
hprop_watch_mempool_stream_trims_nested_cardano_path :: Property
hprop_watch_mempool_stream_trims_nested_cardano_path = H.property $ do
  tx1 <- txInModeFixture
  let script = [mkSnapshot [(tx1, ticket 1)]]
  sent <- expectScriptExhausted =<< runWatchMempoolStream script defMessage ["tx.cardano.fee"]

  full <- expectedResponse tx1
  let expected =
        defMessage
          & U5c.tx . U5c.cardano . Query.fee .~ full ^. U5c.tx . U5c.cardano . Query.fee
  sent === [NextElem expected]

-- | A mask that selects only the ref never evaluates the serialisation or the conversion.
hprop_field_mask_watch_mempool_ref_only_forces_nothing_else :: Property
hprop_field_mask_watch_mempool_ref_only_forces_nothing_else = H.propertyOnce $ do
  let fields = TxInMempoolFields (BS.pack [1, 2, 3]) (error "forced") (error "forced")
  response <- H.evalNF $ mkWatchMempoolResponse (maskOf ["tx.ref"]) fields
  response === (defMessage & U5c.tx . U5c.ref .~ BS.pack [1, 2, 3])

-- | A mask that selects only the native bytes never evaluates the conversion.
hprop_field_mask_watch_mempool_native_bytes_only_forces_no_conversion :: Property
hprop_field_mask_watch_mempool_native_bytes_only_forces_no_conversion = H.propertyOnce $ do
  let fields = TxInMempoolFields (error "forced") (BS.pack [4, 5]) (error "forced")
  response <- H.evalNF $ mkWatchMempoolResponse (maskOf ["tx.native_bytes"]) fields
  response === (defMessage & U5c.tx . U5c.nativeBytes .~ BS.pack [4, 5])

-- | Each field path alone sets its own field and no other.
hprop_field_mask_watch_mempool_gated_paths_set_their_field :: Property
hprop_field_mask_watch_mempool_gated_paths_set_their_field = H.property $ do
  tx1 <- txInModeFixture
  fields <- H.nothingFail $ txInModeToTxInMempoolFields tx1
  let build path = mkWatchMempoolResponse (maskOf [path]) fields
  build "tx.ref" === (defMessage & U5c.tx . U5c.ref .~ txInMempoolFieldsRef fields)
  build "tx.native_bytes"
    === (defMessage & U5c.tx . U5c.nativeBytes .~ txInMempoolFieldsNativeBytes fields)
  build "tx.stage" === (defMessage & U5c.tx . U5c.stage .~ U5c.STAGE_MEMPOOL)
  build "tx.cardano" === (defMessage & U5c.tx . U5c.cardano .~ txInMempoolFieldsCardano fields)

-- | An absent mask sets every field.
hprop_field_mask_watch_mempool_select_all_sets_every_field :: Property
hprop_field_mask_watch_mempool_select_all_sets_every_field = H.property $ do
  tx1 <- txInModeFixture
  fields <- H.nothingFail $ txInModeToTxInMempoolFields tx1
  let response = mkWatchMempoolResponse (maskOf []) fields
  response ^. U5c.tx . U5c.ref === txInMempoolFieldsRef fields
  response ^. U5c.tx . U5c.nativeBytes === txInMempoolFieldsNativeBytes fields
  response ^. U5c.tx . U5c.stage === U5c.STAGE_MEMPOOL
  response ^. U5c.tx . U5c.cardano === txInMempoolFieldsCardano fields

-- | Resolve field mask paths rooted at the response.
maskOf :: [Text] -> FieldMaskTree U5c.WatchMempoolResponse
maskOf = resolve

-- | The unmasked response for a fixture.
expectedFull :: HasCallStack => MonadTest m => TxInMode -> m U5c.WatchMempoolResponse
expectedFull = fmap getProto . expectedResponse

-- | The predicate sees the unmasked transaction: a mask of just @tx.ref@ still
-- drops the entry the predicate rejects, and trims the one it accepts.
hprop_watch_mempool_stream_predicate_runs_before_mask :: Property
hprop_watch_mempool_stream_predicate_runs_before_mask = H.property $ do
  tx1 <- txInModeFixture
  tx2 <- txInModeFixture
  fields1 <- H.nothingFail $ txInModeToTxInMempoolFields tx1
  fields2 <- H.nothingFail $ txInModeToTxInMempoolFields tx2
  let addressesOf = map (view Query.address) . view Query.outputs . txInMempoolFieldsCardano
  address <- case addressesOf fields1 of
    a : _ | a `notElem` addressesOf fields2 -> pure a
    _ -> H.discard
  let producesAddress = defMessage & Query.address . Query.exactAddress .~ address
      predicate =
        defMessage
          & U5c.match . U5c.cardano . Query.produces .~ producesAddress
      script = [mkSnapshot [(tx1, ticket 1), (tx2, ticket 2)]]
  sent <- expectScriptExhausted =<< runWatchMempoolStream script predicate ["tx.ref"]

  sent === [NextElem $ defMessage & U5c.tx . U5c.ref .~ txInMempoolFieldsRef fields1]

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
  -- ^ Field mask paths, rooted at the response
  -> m (Either SomeException (), [NextElem (Proto U5c.WatchMempoolResponse)])
runWatchMempoolStream script predicate fieldMaskPaths = liftIO $ do
  scriptRef <- newIORef script
  let pop =
        readIORef scriptRef >>= \case
          [] -> throwIO ScriptExhausted
          snapshot : rest -> writeIORef scriptRef rest $> snapshot
  sentRef <- newIORef []
  let request = defMessage & U5c.fieldMask . FieldMask.paths .~ fieldMaskPaths
  outcome <-
    try @IO @SomeException $
      watchMempoolStream
        pop
        (const pop)
        predicate
        (watchMempoolFieldMask request)
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
