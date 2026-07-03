{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.NodeKernelAccess.Type
  ( NodeKernelAccess (..)
  )
where

import Cardano.Api.Consensus qualified as Consensus

-- | In-process access to the node kernel.
-- Constructed by cardano-node once consensus initialisation completes.
data NodeKernelAccess = NodeKernelAccess
  { nkaChainDB :: Consensus.ChainDB IO (Consensus.CardanoBlock Consensus.StandardCrypto)
  -- ^ Handle to the consensus chain database
  }
