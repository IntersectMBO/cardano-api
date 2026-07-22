{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Rpc.Server.NodeKernelAccess
  ( NodeKernelAccess (..)
  , mkNodeKernelAccess
  , fetchBlock
  , grabNodeKernelAccess
  , ChainChange (..)
  , ChainFollower (..)
  , withFollower
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Server.Internal.Monad (MonadRpc, grab)
import Cardano.Rpc.Server.NodeKernelAccess.Type

import RIO (MonadUnliftIO, atomically, bracket, throwIO, withRunInIO)

import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Text (pack)
import Network.GRPC.Spec

-- | Construct 'NodeKernelAccess' from a consensus 'Consensus.NodeKernel'.
-- Returns 'Nothing' and traces the block type for non-Cardano block types.
mkNodeKernelAccess
  :: Monad m
  => Tracer m Text
  -- ^ Tracer for unsupported block type warnings
  -> Consensus.BlockType blk
  -- ^ Block type witness
  -> Consensus.TopLevelConfig blk
  -- ^ Top-level consensus config (for system start and era history)
  -> Consensus.NodeKernel IO addrNTN addrNTC blk
  -- ^ Consensus node kernel
  -> m (Maybe NodeKernelAccess)
mkNodeKernelAccess tracer blockType topLevelConfig kernel = case blockType of
  Consensus.CardanoBlockType ->
    pure $ Just NodeKernelAccess{chainDb, systemStart, readEraHistory}
   where
    chainDb = Consensus.getChainDB kernel
    ledgerConfig = Consensus.configLedger topLevelConfig
    systemStart = Consensus.nodeSystemStart topLevelConfig
    -- Read the current ledger state (cheap STM TVar read) and recompute
    -- the era summary on every call - O(number_of_eras).
    -- This is the same approach consensus uses for GetInterpreter queries
    -- (interpretQueryHardFork); neither path caches the summary.
    -- RunWithCachedSummary exists but is private to the blockchain time thread.
    readEraHistory :: MonadIO n => n EraHistory
    readEraHistory = liftIO $ do
      extLedger <- atomically $ Consensus.getCurrentLedger chainDb
      pure . EraHistory . Consensus.mkInterpreter $
        Consensus.hardForkSummary ledgerConfig (Consensus.ledgerState extLedger)
  _ -> do
    -- unsupported block type
    traceWith tracer $ pack (show blockType)
    pure Nothing

-- | Grab the current 'NodeKernelAccess' from the environment, or throw
-- gRPC UNAVAILABLE if the node kernel has not yet initialised.
grabNodeKernelAccess
  :: MonadRpc e m
  => m NodeKernelAccess
grabNodeKernelAccess =
  grab >>= liftIO . readIORef >>= \case
    Nothing ->
      throwIO
        GrpcException
          { grpcError = GrpcUnavailable
          , grpcErrorMessage = Just "Node kernel not yet initialised"
          , grpcErrorDetails = Nothing
          , grpcErrorMetadata = []
          }
    Just nodeKernelAccess ->
      pure nodeKernelAccess

-- | Fetch a raw block and its parsed era-contextualised form from ChainDB
-- by slot and header hash.
fetchBlock
  :: MonadIO m
  => NodeKernelAccess
  -- ^ Node kernel access handle
  -> SlotNo
  -- ^ Block slot number
  -> Hash BlockHeader
  -- ^ Block header hash
  -> m (Maybe (ByteString, BlockInMode))
  -- ^ Raw CBOR bytes and the block in era context, or 'Nothing' if not found
fetchBlock NodeKernelAccess{chainDb} slot (HeaderHash shortHash) = do
  let point = Consensus.RealPoint slot (Consensus.OneEraHash shortHash)
      component = (,) <$> fmap BSL.toStrict Consensus.GetRawBlock <*> fmap fromConsensusBlock Consensus.GetBlock
  liftIO $ Consensus.getBlockComponent chainDb component point

-- | A single instruction produced by a chain follower.
--
-- 'ChainApply' carries the raw CBOR block bytes together with the same block
-- parsed into its era context - exactly the pair 'fetchBlock' returns.
-- Consensus rollbacks are point-only: 'ChainRollBack' never carries the
-- blocks being rolled back, only the point to roll back to.
data ChainChange
  = ChainApply (ByteString, BlockInMode)
  | ChainRollBack ChainPoint

-- | A handle to a running chain follower.
data ChainFollower = ChainFollower
  { nextChange :: forall m. MonadIO m => m ChainChange
  -- ^ Block until the next chain update is available.
  , findIntersect :: forall m. MonadIO m => [ChainPoint] -> m (Maybe ChainPoint)
  -- ^ Move the follower to the first of the given points found on the
  -- current chain, returning that point, or 'Nothing' if none of them are
  -- on the chain.
  }

-- | Run an action with a 'ChainFollower' tracking the selected chain.
--
-- The follower and the resource registry backing it are closed on every
-- exit path, including exceptions. The follower itself runs in 'IO' - the
-- ChainDB handle is monomorphic - so the bracket runs there and the action
-- is unlifted into it.
--
-- Creating a follower is cheap: a few in-memory STM operations, nothing
-- proportional to chain length. The costs are steady-state instead - a
-- caught-up follower receives an O(1) notification per adopted block, while
-- a follower catching up streams blocks from the ImmutableDB (disk reads
-- plus deserialisation per block, with file handles owned by the registry).
-- The node already runs one such follower per connected N2C ChainSync
-- client, so one follower per stream scales the same way.
withFollower
  :: MonadUnliftIO m
  => NodeKernelAccess
  -> (ChainFollower -> m a)
  -> m a
withFollower NodeKernelAccess{chainDb} action =
  withRunInIO $ \runInIO ->
    Consensus.withRegistry $ \registry ->
      bracket
        (Consensus.newFollower chainDb registry Consensus.SelectedChain component)
        Consensus.followerClose
        (runInIO . action . toChainFollower)
 where
  component
    :: Consensus.BlockComponent
         (Consensus.CardanoBlock Consensus.StandardCrypto)
         (ByteString, BlockInMode)
  component =
    (,) <$> fmap BSL.toStrict Consensus.GetRawBlock <*> fmap fromConsensusBlock Consensus.GetBlock

  toChainFollower
    :: Consensus.Follower
         IO
         (Consensus.CardanoBlock Consensus.StandardCrypto)
         (ByteString, BlockInMode)
    -> ChainFollower
  toChainFollower follower =
    ChainFollower
      { nextChange = liftIO $ toChainChange <$> Consensus.followerInstructionBlocking follower
      , findIntersect = \points ->
          liftIO $
            fmap fromConsensusPointHF
              <$> Consensus.followerForward follower (map toConsensusPointHF points)
      }

  toChainChange
    :: Consensus.ChainUpdate
         (Consensus.CardanoBlock Consensus.StandardCrypto)
         (ByteString, BlockInMode)
    -> ChainChange
  toChainChange = \case
    Consensus.AddBlock rawBlock -> ChainApply rawBlock
    Consensus.RollBack point -> ChainRollBack (fromConsensusPointHF point)
