{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.NodeKernelAccess
  ( Type.NodeKernelAccess
  , nodeKernelSystemStart
  , securityParam
  , genesisConfig
  , readEraHistory
  , readChainTipHeader
  , GenesisBundle (..)
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
import Cardano.Rpc.Server.Internal.TimedCache (newTimedCache)
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.NodeKernelAccess.Internal.Type (GenesisBundle (..))
import Cardano.Rpc.Server.NodeKernelAccess.Internal.Type qualified as Type

import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History qualified as History

import RIO (MonadUnliftIO, atomically, bracket, throwIO, withRunInIO)

import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.SOP.Strict (NP (..))
import Data.Text (pack)
import Data.Time.Clock (DiffTime)
-- Imported narrowly: grpc-spec exports an unrelated ':*' which would otherwise
-- make the 'NP' pattern match in 'readGenesisBundle' ambiguous.
import Network.GRPC.Spec (GrpcError (..), GrpcException (..))

-- | Construct 'NodeKernelAccess' from a consensus 'Consensus.NodeKernel'.
-- Returns 'Nothing' and traces the block type for non-Cardano block types.
mkNodeKernelAccess
  :: MonadIO m
  => Tracer m TraceRpc
  -- ^ Tracer for RPC events
  -> GenesisHashShelley
  -- ^ Boot-time Shelley genesis hash
  -> ShelleyGenesisFile In
  -- ^ Path to the Shelley genesis file the node was configured with
  -> Consensus.BlockType blk
  -- ^ Block type witness
  -> Consensus.NodeKernel IO addrNTN addrNTC blk
  -- ^ Consensus node kernel
  -> m (Maybe Type.NodeKernelAccess)
mkNodeKernelAccess tracer shelleyGenesisHash shelleyGenesisFile blockType kernel = case blockType of
  Consensus.CardanoBlockType -> do
    genesisBundle <- readGenesisBundle shelleyGenesisHash shelleyGenesisFile topLevelConfig
    pure $
      Just
        Type.NodeKernelAccess
          { Type.chainDb = chainDb
          , Type.systemStart = Consensus.nodeSystemStart topLevelConfig
          , Type.readHardForkSummary = readHardForkSummary'
          , Type.securityParam = Consensus.configSecurityParam topLevelConfig
          , Type.genesisConfig = genesisBundle
          }
   where
    chainDb = Consensus.getChainDB kernel
    topLevelConfig = Consensus.getTopLevelConfig kernel
    ledgerConfig = Consensus.configLedger topLevelConfig
    -- Primed because 'Cardano.Rpc.Server.NodeKernelAccess' also exports an
    -- accessor of the same name; this is the local action that feeds the
    -- corresponding record field above.
    --
    -- Read the current ledger state (cheap STM TVar read) and recompute
    -- the era summary on every call - O(number_of_eras).
    -- This is the same approach consensus uses for GetInterpreter queries
    -- (interpretQueryHardFork); neither path caches the summary.
    -- RunWithCachedSummary exists but is private to the blockchain time thread.
    readHardForkSummary'
      :: MonadIO n
      => n (History.Summary (CardanoEras Consensus.StandardCrypto))
    readHardForkSummary' = liftIO $ do
      extLedger <- atomically $ Consensus.getCurrentLedger chainDb
      pure $ Consensus.hardForkSummary ledgerConfig (Consensus.ledgerState extLedger)
  _ -> do
    -- unsupported block type
    traceWith tracer . inject . TraceRpcUnsupportedBlockType . pack $ show blockType
    pure Nothing

-- | How long the resolved Shelley genesis is kept after the request that last
-- needed it: five minutes.
--
-- Long enough that a client walking through several genesis queries pays the
-- re-read once, short enough that an idle node is back to retaining nothing
-- soon after being left alone.
shelleyGenesisExpiryTimeout :: DiffTime
shelleyGenesisExpiryTimeout = 5 * 60

-- | Gather the network's genesis configuration out of the node kernel's ledger
-- config, so that the RPC server shares the node's own genesis values instead of
-- holding a second copy alive for the lifetime of the process.
--
-- The per-era ledger configs are matched positionally and exhaustively, so a new
-- Cardano era is a compile error here rather than a silently misread genesis.
-- The Shelley slot is matched but not read. The node only has a compacted copy
-- with the initial funds erased, so the file is the only useful source and the
-- cache reads it when a caller asks.
--
-- The only thing allocated here is that empty cache. Nothing is read from disk
-- and no thread is started.
readGenesisBundle
  :: MonadIO m
  => GenesisHashShelley
  -> ShelleyGenesisFile In
  -> Consensus.TopLevelConfig (Consensus.CardanoBlock Consensus.StandardCrypto)
  -> m GenesisBundle
readGenesisBundle shelleyGenesisHash shelleyGenesisFile topLevelConfig =
  case Consensus.getPerEraLedgerConfig perEraLedgerConfig of
    Consensus.WrapPartialLedgerConfig byron
      :* _shelley
      :* _allegra
      :* _mary
      :* Consensus.WrapPartialLedgerConfig alonzo
      :* _babbage
      :* Consensus.WrapPartialLedgerConfig conway
      :* _dijkstra
      :* Nil -> do
        shelleyGenesisCache <- newTimedCache shelleyGenesisExpiryTimeout
        pure
          GenesisBundle
            { byronConfig = Consensus.byronLedgerConfig byron
            , shelleyGenesisHash
            , shelleyGenesis = (shelleyGenesisFile, shelleyGenesisCache)
            , alonzoGenesis =
                Consensus.shelleyLedgerTranslationContext $ Consensus.shelleyLedgerConfig alonzo
            , conwayGenesis =
                Consensus.shelleyLedgerTranslationContext $ Consensus.shelleyLedgerConfig conway
            }
 where
  perEraLedgerConfig = Consensus.hardForkLedgerConfigPerEra $ Consensus.configLedger topLevelConfig

-- | Grab the current 'NodeKernelAccess' from the environment, or throw
-- gRPC UNAVAILABLE if the node kernel has not yet initialised.
grabNodeKernelAccess
  :: MonadRpc e m
  => m Type.NodeKernelAccess
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

-- | The network's system start time, extracted from genesis config.
-- Used together with 'readEraHistory' to convert slots to wall-clock time.
nodeKernelSystemStart :: Type.NodeKernelAccess -> SystemStart
nodeKernelSystemStart Type.NodeKernelAccess{Type.systemStart = value} = value

-- | The protocol security parameter /k/: consensus never rolls back more
-- than /k/ blocks.
securityParam :: Type.NodeKernelAccess -> Consensus.SecurityParam
securityParam Type.NodeKernelAccess{Type.securityParam = value} = value

-- | The network's genesis configuration.
genesisConfig :: Type.NodeKernelAccess -> GenesisBundle
genesisConfig Type.NodeKernelAccess{Type.genesisConfig = value} = value

-- | Read the raw hard-fork era summary from the current ledger state, with
-- the era boundaries directly accessible.
readHardForkSummary
  :: MonadIO m
  => Type.NodeKernelAccess
  -> m (History.Summary (CardanoEras Consensus.StandardCrypto))
readHardForkSummary Type.NodeKernelAccess{Type.readHardForkSummary = action} = action

-- | Read current era history from the ledger state: the hard-fork era
-- summary wrapped into the opaque interpreter used for slot/time conversion
-- queries.
readEraHistory :: MonadIO m => Type.NodeKernelAccess -> m EraHistory
readEraHistory access = EraHistory . Consensus.mkInterpreter <$> readHardForkSummary access

-- | Read the current chain tip header from ChainDB, or 'Nothing' at origin.
readChainTipHeader
  :: MonadIO m
  => Type.NodeKernelAccess
  -> m (Maybe (Consensus.Header (Consensus.CardanoBlock Consensus.StandardCrypto)))
readChainTipHeader Type.NodeKernelAccess{Type.chainDb = chainDb} = liftIO $ Consensus.getTipHeader chainDb

-- | Fetch a raw block and its parsed era-contextualised form from ChainDB
-- by slot and header hash.
fetchBlock
  :: MonadIO m
  => Type.NodeKernelAccess
  -- ^ Node kernel access handle
  -> SlotNo
  -- ^ Block slot number
  -> Hash BlockHeader
  -- ^ Block header hash
  -> m (Maybe (ByteString, BlockInMode))
  -- ^ Raw CBOR bytes and the block in era context, or 'Nothing' if not found
fetchBlock Type.NodeKernelAccess{Type.chainDb = chainDb} slot (HeaderHash shortHash) = do
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
  -> (ChainFollower -> m a)
  -> m a
withFollower Type.NodeKernelAccess{Type.chainDb = chainDb} action =
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
