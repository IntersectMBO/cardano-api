{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.NodeKernelAccess.Type
  ( NodeKernelAccess (..)
  )
where

import Cardano.Api (EraHistory, SystemStart)
import Cardano.Api.Consensus qualified as Consensus

import Control.Monad.IO.Class (MonadIO)

-- | In-process access to the node kernel.
-- Constructed by cardano-node once consensus initialisation completes.
data NodeKernelAccess = NodeKernelAccess
  { chainDb :: Consensus.ChainDB IO (Consensus.CardanoBlock Consensus.StandardCrypto)
  -- ^ Handle to the consensus chain database
  , systemStart :: SystemStart
  -- ^ Network system start time, extracted from genesis config
  , readEraHistory :: forall m. MonadIO m => m EraHistory
  -- ^ Read current era history from the ledger state.
  -- This is a separate read from 'chainDb', but the inconsistency is
  -- always safe: the ledger state is at or ahead of any block in ChainDB,
  -- and era summaries only grow, so the returned history always covers the
  -- slot of any block fetched from ChainDB.
  }
