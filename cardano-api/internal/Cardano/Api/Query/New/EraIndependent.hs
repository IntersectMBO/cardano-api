{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Query.New.EraIndependent where

import           Cardano.Api.Block
import           Cardano.Api.Eras.Core
import           Cardano.Api.IPC hiding (QueryInMode (..))
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Api.Query hiding (QueryInMode (..))

import qualified Cardano.Ledger.Api as L
import           Cardano.Slotting.Slot (WithOrigin (..))
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))


data IndependentEraQueryError
  = IndependentEraQueryAcquiringFail AcquiringFailure
  | IndependentEraQueryUnsupportedVersion UnsupportedNtcVersionError
  deriving Show

-- | QueryIndependent queries are era independent
data QueryIndependent result where
  QueryCurrentEra
    :: QueryIndependent AnyCardanoEra

  QueryEraHistory
    :: QueryIndependent EraHistory

  QuerySystemStart
    :: QueryIndependent SystemStart

  QueryChainBlockNo
    :: QueryIndependent (WithOrigin BlockNo)

  QueryChainPoint
    :: QueryIndependent ChainPoint


instance NodeToClientVersionOf (QueryIndependent result) where
  nodeToClientVersionOf :: QueryIndependent result -> NodeToClientVersion
  nodeToClientVersionOf QueryCurrentEra = NodeToClientV_9
  nodeToClientVersionOf QueryEraHistory = NodeToClientV_9
  nodeToClientVersionOf QuerySystemStart = NodeToClientV_9
  nodeToClientVersionOf QueryChainBlockNo = NodeToClientV_10
  nodeToClientVersionOf QueryChainPoint = NodeToClientV_10


deriving instance Show (QueryIndependent result)


toConsensusQuery :: forall block result. ()
                 => Consensus.CardanoBlock L.StandardCrypto ~ block
                 => QueryIndependent result
                 -> Some (Consensus.Query block)
toConsensusQuery QueryCurrentEra =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery QueryEraHistory =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery QuerySystemStart = Some Consensus.GetSystemStart

toConsensusQuery QueryChainBlockNo = Some Consensus.GetChainBlockNo

toConsensusQuery QueryChainPoint = Some Consensus.GetChainPoint



fromConsensusQueryResult
  :: forall block result result'. ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => QueryIndependent result
  -> Consensus.Query block result'
  -> result'
  -> result
fromConsensusQueryResult QueryEraHistory q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter)
        -> EraHistory r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QuerySystemStart q' r' =
    case q' of
      Consensus.GetSystemStart
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryChainBlockNo q' r' =
    case q' of
      Consensus.GetChainBlockNo
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryChainPoint q' r' =
    case q' of
      Consensus.GetChainPoint
        -> fromConsensusPointHF r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryCurrentEra q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra)
        -> fromConsensusEraIndex r'
      _ -> fromConsensusQueryResultMismatch

