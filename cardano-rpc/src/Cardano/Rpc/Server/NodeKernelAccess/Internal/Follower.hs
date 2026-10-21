{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | Chain followers over the node's ChainDB: the follower handle and the
-- brackets that open one. Re-exported from "Cardano.Rpc.Server.NodeKernelAccess".
module Cardano.Rpc.Server.NodeKernelAccess.Internal.Follower
  ( ChainChange (..)
  , ChainFollower (..)
  , withFollower
  , withBlockFollower
  , positionFollowerAtCurrentTip
  , readChainTipPoint
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Server.NodeKernelAccess.Internal.Type qualified as Type

import Ouroboros.Consensus.Storage.ChainDB.API qualified as Consensus (getTipPoint)

import RIO (MonadUnliftIO, atomically, bracket, withRunInIO)

import Control.Monad.STM (STM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL

-- | A handle to a running chain follower. The payload is selectable because
-- consensus reads each requested block component separately.
data ChainFollower a = ChainFollower
  { nextChange :: forall m. MonadIO m => m (ChainChange a)
  -- ^ Block until the next chain update is available.
  , tryNextChange :: forall m. MonadIO m => m (Maybe (ChainChange a))
  -- ^ The next chain update, or 'Nothing' if there is none yet. Does not
  -- block, but may read the block from disk.
  , findIntersect :: forall m. MonadIO m => [ChainPoint] -> m (Maybe ChainPoint)
  -- ^ Move the follower to the first of the given points found on the
  -- current chain, returning that point, or 'Nothing' if none of them are
  -- on the chain.
  }

-- | A single instruction produced by a chain follower.
--
-- 'ChainApply' carries the per-block payload the follower was opened for
-- (see 'withFollower' and 'withBlockFollower').
-- Consensus rollbacks are point-only: 'ChainRollBack' never carries the
-- blocks being rolled back, only the point to roll back to.
data ChainChange a
  = ChainApply a
  | ChainRollBack ChainPoint

-- | Run an action with a 'ChainFollower' tracking the selected chain, whose
-- 'ChainApply' payload is the raw CBOR block bytes together with the same
-- block parsed into its era context - exactly the pair 'fetchBlock' returns.
--
-- The follower and the resource registry backing it are closed on every
-- exit path, including exceptions. The follower itself runs in 'IO',
-- because the ChainDB handle is monomorphic, so the bracket runs there and
-- the action is unlifted into it.
--
-- Creating a follower is cheap: a few in-memory STM operations, nothing
-- proportional to chain length. The costs are steady-state instead. A
-- caught-up follower receives an O(1) notification per adopted block. A
-- follower catching up streams blocks from the ImmutableDB, paying a disk
-- read and a deserialisation per block, with file handles owned by the
-- registry. The node already runs one such follower per connected N2C
-- ChainSync client, so one follower per stream scales the same way.
withFollower
  :: MonadUnliftIO m
  => Type.NodeKernelAccess
  -> (ChainFollower (ByteString, BlockInMode) -> m a)
  -> m a
withFollower =
  withFollowerReading $
    (,) <$> fmap BSL.toStrict Consensus.GetRawBlock <*> fmap fromConsensusBlock Consensus.GetBlock

-- | 'withFollower' with the parsed block alone as the 'ChainApply' payload,
-- reading each block once instead of twice.
withBlockFollower
  :: MonadUnliftIO m
  => Type.NodeKernelAccess
  -> (ChainFollower BlockInMode -> m a)
  -> m a
withBlockFollower = withFollowerReading $ fromConsensusBlock <$> Consensus.GetBlock

-- | Run an action with a follower that reads the given block component.
withFollowerReading
  :: MonadUnliftIO m
  => Consensus.BlockComponent (Consensus.CardanoBlock Consensus.StandardCrypto) b
  -> Type.NodeKernelAccess
  -> (ChainFollower b -> m a)
  -> m a
withFollowerReading component Type.NodeKernelAccess{Type.chainDb = chainDb} action =
  withRunInIO $ \runInIO ->
    Consensus.withRegistry $ \registry ->
      bracket
        (Consensus.newFollower chainDb registry Consensus.SelectedChain component)
        Consensus.followerClose
        (runInIO . action . toChainFollower)
 where
  toChainFollower
    :: Consensus.Follower IO (Consensus.CardanoBlock Consensus.StandardCrypto) b
    -> ChainFollower b
  toChainFollower follower =
    ChainFollower
      { nextChange = liftIO $ toChainChange <$> Consensus.followerInstructionBlocking follower
      , tryNextChange = liftIO $ fmap toChainChange <$> Consensus.followerInstruction follower
      , findIntersect = \points ->
          liftIO $
            fmap fromConsensusPointHF
              <$> Consensus.followerForward follower (map toConsensusPointHF points)
      }

  toChainChange
    :: Consensus.ChainUpdate (Consensus.CardanoBlock Consensus.StandardCrypto) b
    -> ChainChange b
  toChainChange = \case
    Consensus.AddBlock payload -> ChainApply payload
    Consensus.RollBack point -> ChainRollBack (fromConsensusPointHF point)

-- | Position a freshly opened chain follower at the current tip (origin, at
-- genesis), so it reports only blocks applied from now on, and return that
-- point. A fork switch between the tip read and the intersection takes the
-- old tip off the chain; then read the new tip and try again, rather than
-- fall back to origin and replay the whole chain on the first drain.
positionFollowerAtCurrentTip
  :: MonadIO m
  => Type.NodeKernelAccess
  -> ChainFollower a
  -> m ChainPoint
positionFollowerAtCurrentTip nodeKernelAccess ChainFollower{findIntersect} = go
 where
  go = do
    tipPoint <- atomically $ readChainTipPoint nodeKernelAccess
    findIntersect [tipPoint] >>= maybe go pure

-- | Read the current chain tip point from ChainDB, or 'ChainPointAtGenesis'
-- at origin. An STM read, so a caller can wait for the tip to move.
readChainTipPoint :: Type.NodeKernelAccess -> STM ChainPoint
readChainTipPoint Type.NodeKernelAccess{Type.chainDb = chainDb} =
  fromConsensusPointHF <$> Consensus.getTipPoint chainDb
