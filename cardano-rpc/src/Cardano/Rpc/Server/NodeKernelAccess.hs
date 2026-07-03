{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Rpc.Server.NodeKernelAccess
  ( NodeKernelAccess (..)
  , mkNodeKernelAccess
  , fetchBlock
  , grabNodeKernelAccess
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Server.Internal.Monad (MonadRpc, grab)
import Cardano.Rpc.Server.NodeKernelAccess.Type

import RIO (atomically, throwIO)

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

-- | Fetch a raw block and its block number from ChainDB by slot and header hash.
fetchBlock
  :: MonadIO m
  => NodeKernelAccess
  -- ^ Node kernel access handle
  -> SlotNo
  -- ^ Block slot number
  -> Hash BlockHeader
  -- ^ Block header hash
  -> m (Maybe (ByteString, BlockNo))
  -- ^ Raw CBOR bytes and block number, or 'Nothing' if not found
fetchBlock NodeKernelAccess{chainDb} slot (HeaderHash shortHash) = do
  let point = Consensus.RealPoint slot (Consensus.OneEraHash shortHash)
      component = (,) <$> fmap BSL.toStrict Consensus.GetRawBlock <*> fmap Consensus.blockNo Consensus.GetBlock
  liftIO $ Consensus.getBlockComponent chainDb component point
