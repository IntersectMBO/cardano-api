{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module Cardano.Api.Query.New.IPC.Generic where

import           Cardano.Api.Block
import           Cardano.Api.InMode
import           Cardano.Api.IO.Base
import           Cardano.Api.IPC hiding (mkLocalNodeClientParams)
import           Cardano.Api.Protocol

import qualified Cardano.Ledger.Api as L
import qualified Ouroboros.Consensus.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import qualified Ouroboros.Network.NodeToClient as Net
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

import           Control.Tracer (nullTracer)

-- Left off here: LocalNodeClientProtocolsInMode is defined
-- to handle QueryInMode. We want to expose a generic function
-- that lets you specify the query you want. Maybe something like

type LocalNodeClientProtocolsQuery query =
  LocalNodeClientProtocols
    BlockInMode
    ChainPoint
    ChainTip
    SlotNo
    TxInMode
    TxIdInMode
    TxValidationErrorInCardanoMode
    query
    IO

-- | Convert from the mode-parametrised style to the block-parametrised style.
--
mkLocalNodeClientParams
  :: ConsensusModeParams
  -> (forall result. query result -> Net.Query.Some (Consensus.Query (Consensus.CardanoBlock L.StandardCrypto)))
  -> (forall result result'. query result -> Consensus.Query (Consensus.CardanoBlock L.StandardCrypto) result' -> result' -> result)
  -> (NodeToClientVersion -> LocalNodeClientProtocolsQuery query)
  -> LocalNodeClientParams
mkLocalNodeClientParams modeparams toConQuery fromConQuery clients =
    -- For each of the possible consensus modes we pick the concrete block type
    -- (by picking the appropriate 'ProtocolClient' value).
    --
    -- Though it is not immediately visible, this point where we use
    -- 'LocalNodeClientParams' is also where we pick up the necessary class
    -- instances. This works because in each case we have a monomorphic block
    -- type and the instances are all in scope. This is why the use of
    -- LocalNodeClientParams is repeated within each branch of the case:
    -- because it is only within each branch that the GADT match makes the
    -- block type monomorphic.
    --
    case modeparams of
      CardanoModeParams epochSlots ->
       LocalNodeClientParamsCardano
         (ProtocolClientInfoArgsCardano epochSlots)
         (convLocalNodeClientProtocolsGeneric toConQuery fromConQuery . clients)

convLocalNodeClientProtocolsGeneric :: ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => (forall result. query result -> Net.Query.Some (Consensus.Query block))
  -> (forall result result'. query result -> Consensus.Query block result' -> result' -> result)
  -> LocalNodeClientProtocolsQuery query
  -> LocalNodeClientProtocolsForBlock block
convLocalNodeClientProtocolsGeneric toConQuery fromConQuery
    LocalNodeClientProtocols {
      localChainSyncClient,
      localTxSubmissionClient,
      localStateQueryClient,
      localTxMonitoringClient
    } =
    LocalNodeClientProtocolsForBlock {
      localChainSyncClientForBlock    = case localChainSyncClient of
        NoLocalChainSyncClient -> NoLocalChainSyncClient
        LocalChainSyncClientPipelined clientPipelined -> LocalChainSyncClientPipelined $ convLocalChainSyncClientPipelined clientPipelined
        LocalChainSyncClient client -> LocalChainSyncClient $ convLocalChainSyncClient client,

      localTxSubmissionClientForBlock = convLocalTxSubmissionClient <$> localTxSubmissionClient,
      localStateQueryClientForBlock   =
        convLocalStateQueryClientGeneric toConQuery fromConQuery <$> localStateQueryClient,
      localTxMonitoringClientForBlock = convLocalTxMonitoringClient <$> localTxMonitoringClient
    }

-- The queries in consensus are parameterized over the block type (Consensus.Query block)
-- However in order to expose an easier to consume query api, we need to convert from our
-- query types to consensus's types and back:
-- (forall result. query result -> Net.Query.Some (Consensus.Query block)) represents
-- a function that converts our queries to consensus's queries.
--
-- (forall result result'. query result -> Consensus.Query block result' -> result' -> result)
-- represents a function that  converts the result' of the consensus query to the result of
-- our query type.
convLocalStateQueryClientGeneric
  :: forall block query m a. ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => Functor m
  => (forall result. query result -> Net.Query.Some (Consensus.Query block))
  -> (forall result result'. query result -> Consensus.Query block result' -> result' -> result)
  -> LocalStateQueryClient BlockInMode ChainPoint query m a
  -> LocalStateQueryClient block (Consensus.Point block) (Consensus.Query block) m a
convLocalStateQueryClientGeneric toConQuery fromConQuery =
    Net.Query.mapLocalStateQueryClient
      toConsensusPointHF
      toConQuery
      fromConQuery


--------------------------------------------------------------------

connectToLocalNodeWithVersionGeneric :: ()
  => LocalNodeConnectInfo
  -> (forall result. query result -> Net.Query.Some (Consensus.Query (Consensus.CardanoBlock Consensus.StandardCrypto)))
  -> (forall result result'. query result -> Consensus.Query (Consensus.CardanoBlock Consensus.StandardCrypto) result' -> result' -> result)
  -> (NodeToClientVersion -> LocalNodeClientProtocolsQuery query)
  -> IO ()
connectToLocalNodeWithVersionGeneric LocalNodeConnectInfo {
                     localNodeSocketPath,
                     localNodeNetworkId,
                     localConsensusModeParams
                   } toConQuery fromConQuery clients =
    Net.withIOManager $ \iomgr ->
      Net.connectTo
        (Net.localSnocket iomgr)
        Net.NetworkConnectTracers {
          Net.nctMuxTracer       = nullTracer,
          Net.nctHandshakeTracer = nullTracer
        }
        versionedProtocls
        (unFile localNodeSocketPath)
  where
    versionedProtocls =
      -- First convert from the mode-parametrised view of things to the
      -- block-parametrised view and then do the final setup for the versioned
      -- bundles of mini-protocols.
      case mkLocalNodeClientParams localConsensusModeParams toConQuery fromConQuery  clients of
        LocalNodeClientParamsSingleBlock ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'
        LocalNodeClientParamsCardano ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'
