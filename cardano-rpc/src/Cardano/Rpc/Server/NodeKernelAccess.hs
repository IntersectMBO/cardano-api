{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | In-process access to the node kernel. The RPC server reads chain
-- state, genesis and mempool straight from the running node, with no
-- Node-to-Client round trip.
module Cardano.Rpc.Server.NodeKernelAccess
  ( -- * Access handle
    Type.NodeKernelAccess
  , mkNodeKernelAccess
  , grabNodeKernelAccess

    -- * Values fixed at startup
  , nodeKernelSystemStart
  , securityParam
  , genesisConfig
  , GenesisBundle (..)

    -- * Era history
  , readHardForkSummary
  , readEraHistory
  , readCurrentEra

    -- * Chain reads
  , readChainTipHeader
  , readChainTipPoint
  , fetchBlock
  , fetchBlockReading

    -- * Mempool
  , readMempoolTxs
  , readMempoolSnapshot
  , MempoolWatchSnapshot (..)
  , watchMempoolSnapshot
  , nextMempoolWatchSnapshot

    -- * Chain follower
  , ChainChange (..)
  , ChainFollower (..)
  , withFollower
  , withBlockFollower
  , positionFollowerAtCurrentTip
  )
where

import Cardano.Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Server.Internal.Monad (MonadRpc, grab)
import Cardano.Rpc.Server.Internal.TimedCache (newTimedCache)
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.NodeKernelAccess.Internal.Follower
import Cardano.Rpc.Server.NodeKernelAccess.Internal.Type (GenesisBundle (..))
import Cardano.Rpc.Server.NodeKernelAccess.Internal.Type qualified as Type

import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
  ( LedgerState (hardForkLedgerStatePerEra)
  , eraIndexFromNS
  )
import Ouroboros.Consensus.HardFork.Combinator.State qualified as HardForkState (tip)
import Ouroboros.Consensus.HardFork.History qualified as History
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Consensus (txForgetValidated)
import Ouroboros.Consensus.Mempool.API qualified as Consensus
  ( MempoolSnapshot (snapshotTxs, snapshotTxsAfter)
  , TicketNo
  , getSnapshot
  )

import RIO (atomically, throwIO)

import Control.Monad.STM (STM, check)
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
          , Type.mempool = Consensus.getMempool kernel
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
    readHardForkSummary' = do
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

-- | Read the era of the current ledger state, the era the mempool stores
-- transactions in. An STM read, so a caller can combine it with others.
readCurrentEra :: Type.NodeKernelAccess -> STM AnyCardanoEra
readCurrentEra Type.NodeKernelAccess{Type.chainDb = chainDb} =
  fromConsensusEraIndex
    . Consensus.eraIndexFromNS
    . HardForkState.tip
    . Consensus.hardForkLedgerStatePerEra
    . Consensus.ledgerState
    <$> Consensus.getCurrentLedger chainDb

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
fetchBlock nodeKernelAccess =
  fetchBlockReading nodeKernelAccess $
    (,) <$> fmap BSL.toStrict Consensus.GetRawBlock <*> fmap fromConsensusBlock Consensus.GetBlock

-- | Read the given components of a block from ChainDB by slot and header hash.
-- ChainDB reads each component on its own, so a caller reads only what it uses.
fetchBlockReading
  :: MonadIO m
  => Type.NodeKernelAccess
  -- ^ Node kernel access handle
  -> Consensus.BlockComponent (Consensus.CardanoBlock Consensus.StandardCrypto) b
  -- ^ The block components to read
  -> SlotNo
  -- ^ Block slot number
  -> Hash BlockHeader
  -- ^ Block header hash
  -> m (Maybe b)
  -- ^ The block components, or 'Nothing' if the block is not found
fetchBlockReading Type.NodeKernelAccess{Type.chainDb = chainDb} component slot (HeaderHash shortHash) =
  liftIO . Consensus.getBlockComponent chainDb component $
    Consensus.RealPoint slot (Consensus.OneEraHash shortHash)

-- | Read the current mempool contents as a point-in-time snapshot:
-- transactions added or removed after the read are not reflected.
-- Ticket numbers and mempool capacity accounting are dropped here, since
-- 'readMempoolTxs' only serves the one-shot @ReadMempool@ method.
readMempoolTxs
  :: MonadIO m
  => Type.NodeKernelAccess
  -> m [Consensus.GenTx (Consensus.CardanoBlock Consensus.StandardCrypto)]
readMempoolTxs nodeKernelAccess = do
  snapshot <- atomically $ readMempoolSnapshot nodeKernelAccess
  pure
    [Consensus.txForgetValidated tx | (tx, _ticketNo, _txMeasure) <- Consensus.snapshotTxs snapshot]

-- | Read the current mempool snapshot. An STM read, so a caller can wait for
-- the mempool to change.
readMempoolSnapshot
  :: Type.NodeKernelAccess
  -> STM (Consensus.MempoolSnapshot (Consensus.CardanoBlock Consensus.StandardCrypto))
readMempoolSnapshot Type.NodeKernelAccess{Type.mempool = mempool} = Consensus.getSnapshot mempool

-- | A point-in-time view of the mempool for @WatchMempool@: a pure projection
-- of the entries newer than a given ticket, already unwrapped into 'TxInMode'
-- so tests can build one from 'TxInMode' fixtures without a real mempool.
newtype MempoolWatchSnapshot = MempoolWatchSnapshot
  { mempoolWatchTxsAfter :: Consensus.TicketNo -> [(TxInMode, Consensus.TicketNo)]
  -- ^ Entries with a ticket number greater than the given one, oldest to
  -- newest.
  }

-- | Read the current mempool watch snapshot, without blocking.
watchMempoolSnapshot
  :: MonadIO m
  => Type.NodeKernelAccess
  -> m MempoolWatchSnapshot
watchMempoolSnapshot nodeKernelAccess =
  toMempoolWatchSnapshot <$> atomically (readMempoolSnapshot nodeKernelAccess)

-- | Block until the mempool holds an entry newer than the given ticket, then
-- return the snapshot. Tickets are never reused, so this sees every addition;
-- removals and slot changes do not wake the caller.
nextMempoolWatchSnapshot
  :: MonadIO m
  => Type.NodeKernelAccess
  -> Consensus.TicketNo
  -> m MempoolWatchSnapshot
nextMempoolWatchSnapshot nodeKernelAccess lastSeenTicket =
  atomically $ do
    candidate <- readMempoolSnapshot nodeKernelAccess
    check . not . null $ Consensus.snapshotTxsAfter candidate lastSeenTicket
    pure $ toMempoolWatchSnapshot candidate

toMempoolWatchSnapshot
  :: Consensus.MempoolSnapshot (Consensus.CardanoBlock Consensus.StandardCrypto)
  -> MempoolWatchSnapshot
toMempoolWatchSnapshot snapshot =
  MempoolWatchSnapshot
    { mempoolWatchTxsAfter = \ticketNo ->
        [ (fromConsensusGenTx (Consensus.txForgetValidated tx), ticketNo')
        | (tx, ticketNo', _txMeasure) <- Consensus.snapshotTxsAfter snapshot ticketNo
        ]
    }
