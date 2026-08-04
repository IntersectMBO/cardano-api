{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.NodeKernelAccess.Type
  ( NodeKernelAccess (..)
  , GenesisBundle (..)
  )
where

import Cardano.Api (EraHistory, GenesisHashShelley, SystemStart)
import Cardano.Api.Consensus qualified as Consensus

import Cardano.Chain.Genesis qualified as Byron (Config)
import Cardano.Ledger.Api.Era qualified as L (LatestKnownEra)
import Cardano.Ledger.Api.Transition qualified as L (TransitionConfig)

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
  , securityParam :: Consensus.SecurityParam
  -- ^ The protocol security parameter /k/: consensus never rolls back more
  -- than /k/ blocks.
  , genesisConfig :: GenesisBundle
  -- ^ The network's genesis configuration.
  -- Genesis data never changes after startup, so it is read once and stored
  -- as a pure value.
  }

-- | The per-era genesis configuration of the network the node is running on.
--
-- Gathered once, when the node kernel hook fires. The Byron genesis and the
-- Shelley-onwards transition config are both read straight off
-- 'Consensus.CardanoProtocolParams', part of cardano-node's boot-time
-- 'Consensus.ProtocolInfoArgs'. No hard-fork navigation is needed.
--
-- The Shelley genesis hash is the exception: 'Consensus.ProtocolInfoArgs'
-- does not carry it, so it is threaded in separately from cardano-node's own
-- boot-time genesis parsing (see
-- 'Cardano.Rpc.Server.NodeKernelAccess.mkNodeKernelAccess').
data GenesisBundle = GenesisBundle
  { byronConfig :: !Byron.Config
  -- ^ The Byron genesis configuration, which bundles the genesis data with
  -- the hash the Byron ledger computed when it parsed the file.
  , shelleyGenesisHash :: !GenesisHashShelley
  -- ^ Blake2b-256 hash of the raw Shelley genesis file bytes.
  , transitionConfig :: !(L.TransitionConfig L.LatestKnownEra)
  -- ^ The Shelley-onwards genesis configuration, in the same representation
  -- 'Cardano.Api.LedgerState.GenesisConfig' uses.
  -- It retains the full parsed Shelley genesis, including @sgInitialFunds@
  -- and @sgStaking@; consensus keeps only a compacted copy with those fields
  -- erased.
  }
