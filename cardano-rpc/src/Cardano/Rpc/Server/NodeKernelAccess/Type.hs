{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.NodeKernelAccess.Type
  ( NodeKernelAccess (..)
  , GenesisBundle (..)
  )
where

import Cardano.Api
  ( EraHistory
  , FileDirection (In)
  , GenesisHashShelley
  , ShelleyGenesisFile
  , SystemStart
  )
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Rpc.Server.Internal.TimedCache (TimedCache)

import Cardano.Chain.Genesis qualified as Byron (Config)
import Cardano.Ledger.Alonzo.Genesis qualified as L (AlonzoGenesis)
import Cardano.Ledger.Conway.Genesis qualified as L (ConwayGenesis)
import Cardano.Ledger.Shelley.Genesis qualified as L (ShelleyGenesis)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History qualified as History

import Control.Monad.IO.Class (MonadIO)

-- | In-process access to the node kernel.
-- Constructed by cardano-node once consensus initialisation completes.
data NodeKernelAccess = NodeKernelAccess
  { chainDb :: Consensus.ChainDB IO (Consensus.CardanoBlock Consensus.StandardCrypto)
  -- ^ Handle to the consensus chain database
  , systemStart :: SystemStart
  -- ^ Network system start time, extracted from genesis config.
  -- Used together with 'readEraHistory' to convert slots to wall-clock time.
  , readEraHistory :: forall m. MonadIO m => m EraHistory
  -- ^ Read current era history from the ledger state.
  -- This is a separate read from 'chainDb', but the inconsistency is
  -- always safe: the ledger state is at or ahead of any block in ChainDB,
  -- and era summaries only grow, so the returned history always covers the
  -- slot of any block fetched from ChainDB.
  , readHardForkSummary
      :: forall m
       . MonadIO m
      => m (History.Summary (CardanoEras Consensus.StandardCrypto))
  -- ^ Read the raw hard-fork era summary 'readEraHistory' wraps into an
  -- opaque 'EraHistory' interpreter. Exposed separately because the
  -- @ReadEraSummary@ RPC method needs the era boundaries themselves, not an
  -- interpreter that only answers slot/time conversion queries.
  , securityParam :: Consensus.SecurityParam
  -- ^ The protocol security parameter /k/: consensus never rolls back more
  -- than /k/ blocks.
  , genesisConfig :: GenesisBundle
  -- ^ The network's genesis configuration.
  -- Genesis data never changes after startup. Most of it is shared with the
  -- running node. The Shelley genesis is read from its file when a caller
  -- asks for it, and dropped again afterwards.
  }

-- | The per-era genesis configuration of the network the node is running on.
--
-- Gathered once, when the node kernel hook fires, by walking the per-era ledger
-- configs of the node kernel's 'Consensus.TopLevelConfig'. The Byron, Alonzo and
-- Conway genesis values are the ones the running node holds, shared with it
-- rather than copied. Nothing is kept from cardano-node's boot-time
-- 'Consensus.ProtocolInfoArgs', whose Shelley genesis reaches gigabytes on
-- networks with large initial fund sets.
--
-- The Shelley genesis is not kept here at all. All the node has is a compacted
-- copy with the initial funds erased, which is no use to a caller, so
-- 'shelleyGenesis' holds the file and a cache instead and the genesis is read
-- from disk when someone asks for it.
--
-- The Shelley genesis hash and file path come from cardano-node's own boot-time
-- genesis parsing, because the ledger config carries neither (see
-- 'Cardano.Rpc.Server.NodeKernelAccess.mkNodeKernelAccess').
data GenesisBundle = GenesisBundle
  { byronConfig :: !Byron.Config
  -- ^ The Byron genesis configuration, which bundles the genesis data with
  -- the hash the Byron ledger computed when it parsed the file.
  , shelleyGenesisHash :: !GenesisHashShelley
  -- ^ Blake2b-256 hash of the raw Shelley genesis file bytes.
  , shelleyGenesis :: !(ShelleyGenesisFile In, TimedCache L.ShelleyGenesis)
  -- ^ The Shelley genesis file the node booted from, and a cache of that file
  -- parsed in full, with the initial funds resolved. The two belong together:
  -- the path is what the cache loads from. The cache starts empty, is filled by
  -- the first request that needs the genesis, and empties itself once five
  -- minutes have passed without another one. A node whose genesis nobody asks
  -- about therefore keeps none of it in memory (issue #1314).
  , alonzoGenesis :: !L.AlonzoGenesis
  -- ^ The Alonzo genesis, which the ledger keeps as the Alonzo translation
  -- context.
  , conwayGenesis :: !L.ConwayGenesis
  -- ^ The Conway genesis, which the ledger keeps as the Conway translation
  -- context.
  }
