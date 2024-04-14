{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Redundant fmap" -}
{- HLINT ignore "Use fmap" -}

module Cardano.Api.LedgerState.ChainSync
  ( chainSyncClientWithLedgerState
  , chainSyncClientPipelinedWithLedgerState
  ) where

import           Cardano.Api.Block
import           Cardano.Api.LedgerEvents.ConvertLedgerEvent
import           Cardano.Api.LedgerState.Core

import           Cardano.Slotting.Slot (WithOrigin (Origin))
import qualified Ouroboros.Network.Protocol.ChainSync.Client as CS
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as CSP

import qualified Data.Sequence as Seq
import           Network.TypedProtocol.Pipelined (Nat (..))

-- | Wrap a 'ChainSyncClient' with logic that tracks the ledger state.
chainSyncClientWithLedgerState
  :: forall m a.
     Monad m
  => Env
  -> LedgerState
  -- ^ Initial ledger state
  -> ValidationMode
  -> CS.ChainSyncClient (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client to wrap. The block is annotated with a 'Either LedgerStateError
  -- LedgerState'. This is either an error from validating a block or
  -- the current 'LedgerState' from applying the current block. If we
  -- trust the node, then we generally expect blocks to validate. Also note that
  -- after a block fails to validate we may still roll back to a validated
  -- block, in which case the valid 'LedgerState' will be passed here again.
  -> CS.ChainSyncClient BlockInMode
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client that acts just like the wrapped client but doesn't require the
  -- 'LedgerState' annotation on the block type.
chainSyncClientWithLedgerState env ledgerState0 validationMode (CS.ChainSyncClient clientTop)
  = CS.ChainSyncClient (goClientStIdle (Right initialLedgerStateHistory) <$> clientTop)
  where
    goClientStIdle
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> CS.ClientStIdle (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CS.ClientStIdle  BlockInMode                                                        ChainPoint ChainTip m a
    goClientStIdle history client = case client of
      CS.SendMsgRequestNext a b -> CS.SendMsgRequestNext a (goClientStNext history b)
      CS.SendMsgFindIntersect ps a -> CS.SendMsgFindIntersect ps (goClientStIntersect history a)
      CS.SendMsgDone a -> CS.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> CS.ClientStNext (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CS.ClientStNext  BlockInMode                                                        ChainPoint ChainTip m a
    goClientStNext (Left err) (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) = CS.ClientStNext
      (\blkInMode tip -> CS.ChainSyncClient $
            goClientStIdle (Left err) <$> CS.runChainSyncClient
                (recvMsgRollForward (blkInMode, Left err) tip)
      )
      (\point tip -> CS.ChainSyncClient $
            goClientStIdle (Left err) <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )
    goClientStNext (Right history) (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) = CS.ClientStNext
      (\blkInMode@(BlockInMode _ blk@(Block (BlockHeader slotNo _ _) _)) tip -> CS.ChainSyncClient $ let
          newLedgerStateE = case Seq.lookup 0 history of
            Nothing -> error "Impossible! History should always be non-empty"
            Just (_, Left err, _) -> Left err
            Just (_, Right (oldLedgerState, _), _) -> applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blk
          (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
          in goClientStIdle (Right history') <$> CS.runChainSyncClient
                (recvMsgRollForward (blkInMode, newLedgerStateE) tip)
      )
      (\point tip -> let
          oldestSlot = case history of
            _ Seq.:|> (s, _, _) -> s
            Seq.Empty -> error "Impossible! History should always be non-empty"
          history' = (\h -> if Seq.null h
                              then Left (InvalidRollback oldestSlot point)
                              else Right h)
                  $ case point of
                        ChainPointAtGenesis -> initialLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
        in CS.ChainSyncClient $ goClientStIdle history' <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )

    goClientStIntersect
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> CS.ClientStIntersect (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CS.ClientStIntersect  BlockInMode                                                        ChainPoint ChainTip m a
    goClientStIntersect history (CS.ClientStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CS.ClientStIntersect
      (\point tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectFound point tip)))
      (\tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectNotFound tip)))

    initialLedgerStateHistory :: History (Either LedgerStateError LedgerStateEvents)
    initialLedgerStateHistory = Seq.singleton (0, Right (ledgerState0, []), Origin)

-- | See 'chainSyncClientWithLedgerState'.
chainSyncClientPipelinedWithLedgerState
  :: forall m a.
     Monad m
  => Env
  -> LedgerState
  -> ValidationMode
  -> CSP.ChainSyncClientPipelined
                        (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
                        ChainPoint
                        ChainTip
                        m
                        a
  -> CSP.ChainSyncClientPipelined
                        BlockInMode
                        ChainPoint
                        ChainTip
                        m
                        a
chainSyncClientPipelinedWithLedgerState env ledgerState0 validationMode (CSP.ChainSyncClientPipelined clientTop)
  = CSP.ChainSyncClientPipelined (goClientPipelinedStIdle (Right initialLedgerStateHistory) Zero <$> clientTop)
  where
    goClientPipelinedStIdle
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> Nat n
      -> CSP.ClientPipelinedStIdle n (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIdle n  BlockInMode                                                        ChainPoint ChainTip m a
    goClientPipelinedStIdle history n client = case client of
      CSP.SendMsgRequestNext a b -> CSP.SendMsgRequestNext a (goClientStNext history n b)
      CSP.SendMsgRequestNextPipelined m a ->  CSP.SendMsgRequestNextPipelined m (goClientPipelinedStIdle history (Succ n) a)
      CSP.SendMsgFindIntersect ps a -> CSP.SendMsgFindIntersect ps (goClientPipelinedStIntersect history n a)
      CSP.CollectResponse a b -> case n of
        Succ nPrev -> CSP.CollectResponse ((fmap . fmap) (goClientPipelinedStIdle history n) a) (goClientStNext history nPrev b)
      CSP.SendMsgDone a -> CSP.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> Nat n
      -> CSP.ClientStNext n (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CSP.ClientStNext n  BlockInMode                                                        ChainPoint ChainTip m a
    goClientStNext (Left err) n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) = CSP.ClientStNext
      (\blkInMode tip ->
          goClientPipelinedStIdle (Left err) n <$> recvMsgRollForward
            (blkInMode, Left err) tip
      )
      (\point tip ->
          goClientPipelinedStIdle (Left err) n <$> recvMsgRollBackward point tip
      )
    goClientStNext (Right history) n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) = CSP.ClientStNext
      (\blkInMode@(BlockInMode _ blk@(Block (BlockHeader slotNo _ _) _)) tip -> let
          newLedgerStateE = case Seq.lookup 0 history of
            Nothing -> error "Impossible! History should always be non-empty"
            Just (_, Left err, _) -> Left err
            Just (_, Right (oldLedgerState, _), _) -> applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blk
          (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
        in goClientPipelinedStIdle (Right history') n <$> recvMsgRollForward
              (blkInMode, newLedgerStateE) tip
      )
      (\point tip -> let
          oldestSlot = case history of
            _ Seq.:|> (s, _, _) -> s
            Seq.Empty -> error "Impossible! History should always be non-empty"
          history' = (\h -> if Seq.null h
                              then Left (InvalidRollback oldestSlot point)
                              else Right h)
                  $ case point of
                        ChainPointAtGenesis -> initialLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
        in goClientPipelinedStIdle history' n <$> recvMsgRollBackward point tip
      )

    goClientPipelinedStIntersect
      :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
      -> Nat n
      -> CSP.ClientPipelinedStIntersect (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent])) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIntersect  BlockInMode                                                        ChainPoint ChainTip m a
    goClientPipelinedStIntersect history _ (CSP.ClientPipelinedStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CSP.ClientPipelinedStIntersect
      (\point tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectFound point tip)
      (\tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectNotFound tip)

    initialLedgerStateHistory :: History (Either LedgerStateError LedgerStateEvents)
    initialLedgerStateHistory = Seq.singleton (0, Right (ledgerState0, []), Origin)

data LedgerStateCondition
  = ConditionMet
  | ConditionNotMet
  deriving (Show, Eq)
