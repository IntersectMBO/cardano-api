{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit coverage for the @WaitForTx@ state machine 'stepWaitForTx' and the
-- pure decisions around it, extracted from
-- 'Cardano.Rpc.Server.Internal.UtxoRpc.WaitForTx.waitForTxMethod'. The
-- tests fold the core over scripted event lists, no live mempool or ChainDB
-- required.
module Test.Cardano.Rpc.WaitForTxStream where

import Cardano.Api
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.WaitForTx
  ( DrainedChange (..)
  , WaitForTxEvent (..)
  , WaitForTxState (..)
  , drainedBlocks
  , idleDeadlineAfterStep
  , idleTimeLeft
  , initialWaitForTxState
  , maxWaitForTxRefs
  , nextIdleDeadline
  , parseRequestRefs
  , stepWaitForTx
  , waitForTxIdleTimeout
  )

import RIO

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import Data.Time.Clock (DiffTime)
import Network.GRPC.Spec
  ( GrpcError (GrpcInvalidArgument)
  , GrpcException (..)
  , NextElem (..)
  , Proto (Proto)
  )

import Test.Gen.Cardano.Api.Typed (genTxId)

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | The normal happy path: a ref is seen in the mempool, then confirmed in a
-- block, and the stream closes ('NoNextElem') right after the confirming
-- event.
hprop_wait_for_tx_step_mempool_then_confirmed :: Property
hprop_wait_for_tx_step_mempool_then_confirmed = H.propertyOnce $ do
  let run =
        runSteps
          (Set.singleton txIdA)
          [ MempoolMembership (Set.singleton txIdA)
          , TipBlocks [Set.singleton txIdA]
          ]

  H.note_ "mempool(A), confirmed(A), then the stream closes on its own"
  runSent run
    === [ NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]

-- | A ref can be confirmed without ever being observed in the mempool (e.g.
-- it entered and left the mempool between two wake-ups) - no
-- 'STAGE_MEMPOOL' is required before 'STAGE_CONFIRMED'.
hprop_wait_for_tx_step_confirmation_without_mempool_sighting :: Property
hprop_wait_for_tx_step_confirmation_without_mempool_sighting = H.propertyOnce $ do
  let run = runSteps (Set.singleton txIdA) [TipBlocks [Set.singleton txIdA]]

  H.note_ "confirmed(A) with no preceding mempool(A)"
  runSent run === [NextElem (response txIdA U5c.STAGE_CONFIRMED), NoNextElem]

-- | A ref that is evicted from the mempool (absent from a later membership
-- set) and then re-added gets 'STAGE_MEMPOOL' again - dedupe covers only
-- refs that stay in the mempool.
hprop_wait_for_tx_step_mempool_again_after_re_add :: Property
hprop_wait_for_tx_step_mempool_again_after_re_add = H.propertyOnce $ do
  let run =
        runSteps
          (Set.singleton txIdA)
          [ MempoolMembership (Set.singleton txIdA)
          , MempoolMembership Set.empty
          , MempoolMembership (Set.singleton txIdA)
          , TipBlocks [Set.singleton txIdA]
          ]

  H.note_ "mempool(A) on each entry, then confirmed(A)"
  runSent run
    === [ NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]

-- | A ref that stays in consecutive membership sets gets only one
-- 'STAGE_MEMPOOL', and ids that are not requested are ignored.
hprop_wait_for_tx_step_consecutive_snapshots_one_mempool_stage :: Property
hprop_wait_for_tx_step_consecutive_snapshots_one_mempool_stage = H.propertyOnce $ do
  let run =
        runSteps
          (Set.singleton txIdA)
          [ MempoolMembership (Set.singleton txIdA)
          , MempoolMembership (Set.fromList [txIdA, txIdB])
          , TipBlocks [Set.singleton txIdA]
          ]

  H.note_ "exactly one mempool(A), despite two membership sets reporting A present"
  runSent run
    === [ NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]

-- | With two requested refs, the stream keeps waiting until *both* reach
-- @STAGE_CONFIRMED@ - confirming only one does not close it.
hprop_wait_for_tx_step_multiple_refs_close_only_when_all_confirmed :: Property
hprop_wait_for_tx_step_multiple_refs_close_only_when_all_confirmed = H.propertyOnce $ do
  let run =
        runSteps
          (Set.fromList [txIdA, txIdB])
          [ TipBlocks [Set.singleton txIdA]
          , MempoolMembership (Set.singleton txIdB)
          , TipBlocks [Set.singleton txIdB]
          ]

  H.note_ "confirmed(A) alone does not close the stream; it closes only once B is confirmed too"
  runSent run
    === [ NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NextElem (response txIdB U5c.STAGE_MEMPOOL)
        , NextElem (response txIdB U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]

-- | An empty ref set closes the stream at once, before any event is taken.
hprop_wait_for_tx_step_empty_refs_close_at_once :: Property
hprop_wait_for_tx_step_empty_refs_close_at_once = H.propertyOnce $ do
  let script = [MempoolMembership (Set.singleton txIdA)]
      run = runSteps Set.empty script

  H.note_ "no state to wait with, a single NoNextElem sent, no event taken"
  initialWaitForTxState Set.empty === Nothing
  runSent run === [NoNextElem]
  runUnused run === script

-- | 'STAGE_MEMPOOL' is sent on every entry into the mempool and once while a
-- ref stays, per ref. Each step starts from exactly the refs announced and
-- still in the mempool, which the production wait compares against.
hprop_wait_for_tx_step_mempool_stage_per_entry :: Property
hprop_wait_for_tx_step_mempool_stage_per_entry = H.propertyOnce $ do
  let run =
        runSteps
          (Set.fromList [txIdA, txIdB])
          [ MempoolMembership (Set.singleton txIdA)
          , MempoolMembership (Set.fromList [txIdA, txIdB])
          , MempoolMembership (Set.singleton txIdB)
          , MempoolMembership (Set.fromList [txIdA, txIdB])
          , TipBlocks [Set.fromList [txIdA, txIdB]]
          ]

  H.note_ "mempool(A), mempool(B) while A stays, mempool(A) again after A leaves and re-enters"
  runSent run
    === [ NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdB U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NextElem (response txIdB U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]
  H.note_ "the announced set tracks the membership"
  runSeen run
    === [ waitForTxState [txIdA, txIdB] []
        , waitForTxState [txIdA, txIdB] [txIdA]
        , waitForTxState [txIdA, txIdB] [txIdA, txIdB]
        , waitForTxState [txIdA, txIdB] [txIdB]
        , waitForTxState [txIdA, txIdB] [txIdA, txIdB]
        ]

-- | A confirmed ref leaves both the pending and the announced set: a later
-- membership set or block that still holds it sends nothing more for it.
hprop_wait_for_tx_step_confirmation_removes_ref :: Property
hprop_wait_for_tx_step_confirmation_removes_ref = H.propertyOnce $ do
  let run =
        runSteps
          (Set.fromList [txIdA, txIdB])
          [ MempoolMembership (Set.singleton txIdA)
          , TipBlocks [Set.singleton txIdA]
          , MempoolMembership (Set.fromList [txIdA, txIdB])
          , TipBlocks [Set.fromList [txIdA, txIdB]]
          ]

  H.note_ "no second mempool(A) or confirmed(A) after A is confirmed"
  runSent run
    === [ NextElem (response txIdA U5c.STAGE_MEMPOOL)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NextElem (response txIdB U5c.STAGE_MEMPOOL)
        , NextElem (response txIdB U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]
  H.note_ "the state drops A once it is confirmed"
  runSeen run
    === [ waitForTxState [txIdA, txIdB] []
        , waitForTxState [txIdA, txIdB] [txIdA]
        , waitForTxState [txIdB] []
        , waitForTxState [txIdB] [txIdB]
        ]

-- | One tip move that confirms every pending ref closes the stream at once,
-- with confirmations in block order. An empty tip move keeps it open, ids
-- that are not pending are ignored, and no event is taken after the close.
hprop_wait_for_tx_step_closes_when_all_confirmed :: Property
hprop_wait_for_tx_step_closes_when_all_confirmed = H.propertyOnce $ do
  let afterClose = [MempoolMembership (Set.singleton txIdA)]
      run =
        runSteps (Set.fromList [txIdA, txIdB]) $
          [ TipBlocks []
          , TipBlocks [Set.singleton txIdB, Set.singleton txIdC, Set.singleton txIdA]
          ]
            <> afterClose

  H.note_ "confirmed(B), then confirmed(A), then the stream closes without another wait"
  runSent run
    === [ NextElem (response txIdB U5c.STAGE_CONFIRMED)
        , NextElem (response txIdA U5c.STAGE_CONFIRMED)
        , NoNextElem
        ]
  length (runSeen run) === 2
  runUnused run === afterClose

-- | A request at 'maxWaitForTxRefs' refs is accepted, and one more ref is
-- rejected with @INVALID_ARGUMENT@. Refs are counted before duplicates are
-- dropped, like @FetchBlock@'s cap.
hprop_wait_for_tx_parse_refs_cap_counts_duplicates :: Property
hprop_wait_for_tx_parse_refs_cap_counts_duplicates = H.propertyOnce $ do
  let refBytes = serialiseToRawBytes txIdA
  atCap <- parseRequestRefs $ replicate maxWaitForTxRefs refBytes

  H.note_ "a request at the cap is accepted, and its duplicates collapse to one ref"
  atCap === Set.singleton txIdA

  overCap <-
    liftIO . try @IO @SomeException . parseRequestRefs $ replicate (maxWaitForTxRefs + 1) refBytes
  H.note_ "one ref over the cap is rejected, even though every ref is the same"
  H.assertWith overCap $ failedWith GrpcInvalidArgument

-- | The loop waits for the microseconds left until the idle deadline, and
-- ends the stream with @DEADLINE_EXCEEDED@ once there are none: at the
-- deadline and after it, never before.
hprop_wait_for_tx_idle_time_left_until_deadline :: Property
hprop_wait_for_tx_idle_time_left_until_deadline = H.property $ do
  deadline <- forAll $ fromMicros <$> Gen.int (Range.linear 0 (2 ^ (50 :: Int)))
  before <- forAll $ Gen.int (Range.linear 1 timeoutMicros)
  after <- forAll $ Gen.int (Range.linear 0 timeoutMicros)

  H.note_ "before the deadline, the loop waits for exactly the time left"
  idleTimeLeft deadline (deadline - fromMicros before) === Just before

  H.note_ "at or after the deadline, the loop times out"
  idleTimeLeft deadline (deadline + fromMicros after) === Nothing

-- | A step that sends a response restarts the idle deadline: the old
-- deadline then leaves the time since the send, and the stream times out one
-- full timeout after it. A step that sends nothing keeps the deadline.
hprop_wait_for_tx_idle_deadline_restarts_after_send :: Property
hprop_wait_for_tx_idle_deadline_restarts_after_send = H.property $ do
  opened <- forAll $ fromMicros <$> Gen.int (Range.linear 0 (2 ^ (50 :: Int)))
  sentAfter <- forAll $ Gen.int (Range.linear 1 (timeoutMicros - 1))
  let deadline = opened + waitForTxIdleTimeout
      sentAt = opened + fromMicros sentAfter
      restarted = nextIdleDeadline sentAt deadline [response txIdA U5c.STAGE_MEMPOOL]

  H.note_ "a step that sends nothing keeps the deadline"
  nextIdleDeadline sentAt deadline [] === deadline

  H.note_ "after a send, the old deadline leaves the time since the send"
  idleTimeLeft restarted deadline === Just sentAfter

  H.note_ "the stream times out one full timeout after the send"
  idleTimeLeft restarted (sentAt + waitForTxIdleTimeout) === Nothing

-- | A change taken past the idle deadline that sends no response ends the
-- stream, so repeated silent changes cannot keep it alive. With responses
-- the deadline restarts, and a change taken before the deadline keeps it.
hprop_wait_for_tx_idle_deadline_after_expired_step :: Property
hprop_wait_for_tx_idle_deadline_after_expired_step = H.property $ do
  deadline <- forAll $ fromMicros <$> Gen.int (Range.linear 0 (2 ^ (50 :: Int)))
  sentAfter <- forAll $ Gen.int (Range.linear 0 timeoutMicros)
  let sentAt = deadline + fromMicros sentAfter
      responses = [response txIdA U5c.STAGE_MEMPOOL]

  H.note_ "expired and silent: the stream times out"
  idleDeadlineAfterStep True sentAt deadline [] === Nothing

  H.note_ "expired with responses: the deadline restarts from the send"
  idleDeadlineAfterStep True sentAt deadline responses === Just (sentAt + waitForTxIdleTimeout)

  H.note_ "not expired and silent: the deadline is unchanged"
  idleDeadlineAfterStep False sentAt deadline [] === Just deadline

-- | A rollback within one drain undoes the drained blocks after its target,
-- so a block orphaned before the drain ends is never confirmed.
hprop_wait_for_tx_drained_blocks_drop_rolled_back :: Property
hprop_wait_for_tx_drained_blocks_drop_rolled_back = H.propertyOnce $ do
  H.note_ "B's block is rolled back and replaced by C's; A's block stays"
  drainedBlocks
    [ DrainedRollBack (testPoint 10 0x10)
    , DrainedBlock (testPoint 11 0x11) (Set.singleton txIdA)
    , DrainedBlock (testPoint 12 0x12) (Set.singleton txIdB)
    , DrainedRollBack (testPoint 11 0x11)
    , DrainedBlock (testPoint 12 0x22) (Set.singleton txIdC)
    ]
    === [Set.singleton txIdA, Set.singleton txIdC]

  H.note_ "a rollback to before every drained block undoes them all"
  drainedBlocks
    [ DrainedBlock (testPoint 11 0x11) (Set.singleton txIdA)
    , DrainedRollBack (testPoint 10 0x10)
    ]
    === []

-- | Consensus compares transaction ids by hash only, ignoring the era tag.
-- @WaitForTx@ relies on this to find a ref of any era with one id built in
-- the ledger's current era.
hprop_wait_for_tx_consensus_tx_id_ignores_era :: Property
hprop_wait_for_tx_consensus_tx_id_ignores_era = H.property $ do
  ref <- forAll genTxId
  AnyCardanoEra era1 <- forAll $ Gen.element [minBound .. maxBound]
  AnyCardanoEra era2 <- forAll $ Gen.element [minBound .. maxBound]
  compare (toConsensusTxId $ TxIdInMode era1 ref) (toConsensusTxId $ TxIdInMode era2 ref) === EQ

-- | What the @WaitForTx@ loop does with a scripted event list, minus the
-- waiting.
data StepRun = StepRun
  { runSent :: [NextElem (Proto U5c.WaitForTxResponse)]
  -- ^ Every message sent, ending with 'NoNextElem' if the stream closed
  , runSeen :: [WaitForTxState]
  -- ^ The state each event was taken in
  , runUnused :: [WaitForTxEvent]
  -- ^ The events left when the stream closed
  }

-- | Fold 'stepWaitForTx' over the events from the initial state of the given
-- refs, as the loop does: no event is taken once the stream has closed.
runSteps :: Set TxId -> [WaitForTxEvent] -> StepRun
runSteps refs = go $ initialWaitForTxState refs
 where
  go :: Maybe WaitForTxState -> [WaitForTxEvent] -> StepRun
  go Nothing events = StepRun{runSent = [NoNextElem], runSeen = [], runUnused = events}
  go (Just _) [] = StepRun{runSent = [], runSeen = [], runUnused = []}
  go (Just state) (event : rest) =
    let (next, responses) = stepWaitForTx state event
        StepRun{runSent, runSeen, runUnused} = go next rest
     in StepRun
          { runSent = map NextElem responses <> runSent
          , runSeen = state : runSeen
          , runUnused
          }

-- | 'waitForTxIdleTimeout' in microseconds.
timeoutMicros :: Int
timeoutMicros = ceiling $ waitForTxIdleTimeout * 1000000

-- | A 'DiffTime' of the given microseconds.
fromMicros :: Int -> DiffTime
fromMicros micros = fromIntegral micros / 1000000

-- | A fixture 'ChainPoint' at the given slot, with a header hash of one
-- repeated byte.
testPoint :: SlotNo -> Word8 -> ChainPoint
testPoint slot byte = ChainPoint slot (HeaderHash (SBS.replicate 32 byte))

-- | Whether an outcome is a 'GrpcException' with the given error code.
failedWith :: GrpcError -> Either SomeException a -> Bool
failedWith expected = \case
  Left e | Just GrpcException{grpcError} <- fromException e -> grpcError == expected
  _ -> False

-- | Distinct fixture transaction ids, for scripts that need to
-- distinguish which ref an event is about.
txIdA, txIdB, txIdC :: TxId
txIdA = mkTxId 0xAA
txIdB = mkTxId 0xBB
txIdC = mkTxId 0xCC

-- | Build a fixture 'TxId' from a repeated byte, at the width 'TxId's
-- underlying hash expects.
mkTxId :: Word8 -> TxId
mkTxId byte =
  either (error . show) id . deserialiseFromRawBytes AsTxId $ BS.replicate 32 byte

-- | The @WaitForTxResponse@ 'stepWaitForTx' returns for a ref reaching a
-- stage.
response :: TxId -> U5c.Stage -> Proto U5c.WaitForTxResponse
response ref stage =
  defMessage
    & U5c.ref .~ serialiseToRawBytes ref
    & U5c.stage .~ Proto stage

-- | The 'WaitForTxState' with the given pending and announced refs.
waitForTxState :: [TxId] -> [TxId] -> WaitForTxState
waitForTxState pending announced =
  WaitForTxState{pendingRefs = Set.fromList pending, mempoolNotifiedRefs = Set.fromList announced}
