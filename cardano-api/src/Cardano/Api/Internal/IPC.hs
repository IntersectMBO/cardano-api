{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Node IPC protocols
--
-- This module provides the client side of the node-to-client IPC protocol
-- used to communicate with a local Cardano node. This can be used to
-- query the node for information, to submit transactions, and even to
-- get historical information aobut the chain by using the
-- 'ChainSync' protocol.
module Cardano.Api.Internal.IPC
  ( -- * Examples

    -- | In this section, we show two examples: one for querying the node
    -- and obtain some basic information, and another one for submitting a
    -- transaction to the node.
    --
    -- To find out about how to create a transaction, see the documentation
    -- in "Cardano.Api.Internal.Tx.Body".
    --
    -- For the following examples we will use the following qualified imports
    -- from @cardano-api@:
    --
    -- @
    -- import qualified Cardano.Api as Api
    -- import qualified Cardano.Api.Consensus as Consensus
    -- import qualified Cardano.Api.Network as Network
    -- import qualified Cardano.Api.Shelley as Shelley
    -- @
    --
    -- We will also use the following explicit import from @base@:
    --
    -- @
    -- import Control.Monad.Except (runExceptT)
    -- @
    --
    -- And we will assume we are working on top of the @IO@ monad and that
    -- we have unqualified access to the @Prelude@ module.

    -- ** Constructing connection information

    -- | Independently of whether we want to query the node or submit transactions,
    -- the first thing we need to do is to gather the connection information.
    --
    -- We need three pieces of information:
    --
    --    * The number of slots per epoch for the network the node is connected to.
    --      We can obtain this information by looking for the @epochLength@ key in
    --      the @shelley-genesis.json@ file that the node is using to connect to the
    --      network. For the preview network, this is currently @86_400@.
    --    * In the case we are connecting to a testnet, we also need to find out
    --      the magic number for the network the node is connected to. This can be
    --      obtained by looking for the @networkMagic@ key in the @shelley-genesis.json@
    --      file that the node is using to connect to the network. For the preview
    --      network, the magic number is currently @2@.
    --    * The path to the socket file of the node. This can be set when starting the
    --      node by using the @--socket-path@ parameter. By default it can usually be
    --      found in the @db@ subfolder of the node's working directory.
    --
    --  Then, we gather all the information into a 'LocalNodeConnectInfo' value.
    --
    --  For example, let's assume we are connecting to the preview network, and that the
    --  socket file is located at @\/home\/user\/cardano-node\/db\/node.socket@. We could
    --  then create the 'LocalNodeConnectInfo' value as follows:
    --
    -- @
    -- let connectionInfo =
    --       Api.LocalNodeConnectInfo
    --          { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 86_400)
    --          , Api.localNodeNetworkId = Api.Testnet (Api.NetworkMagic 2)
    --          , Api.localNodeSocketPath = Api.File "\/home\/user\/cardano-node\/db\/node.socket"
    --          }
    -- @

    -- ** Querying the node for the UTxO set

    -- | Let's imagine we want to obtain the set of transaction outputs that
    -- are currently unspent in the network (UTxO set).

    -- *** Obtaining the current era

    -- | Depending on the type of query we want to perform, we usually need
    -- to know what era the node is currently in. We can hardcode this
    -- information by using one of the constructors of the 'ShelleyBasedEra' type.
    -- But we can also obtain this information from the node, as follows:
    --
    -- @
    -- eEra <- runExceptT $ Api.queryNodeLocalState connectionInfo Network.VolatileTip Api.QueryCurrentEra
    -- @
    --
    -- Here, 'VolatileTip' means we want to get the information out of the most recent node that the
    -- node is aware of. The disadvantage is that the information we get may potentially be rolled back
    -- and stop being valid. We need to account for this. Alternatively, we can use 'ImmutableTip' to
    -- obtain the information from the most recent block that is assumed by the consensus algorithm
    -- to be final, and that won't be rolled back. But this information is not so recent, in mainnet
    -- this is about 36 hours in the past.
    --
    -- 'QueryCurrentEra' is the constructor of the query we want to run. In this case, we want to
    -- obtain the current era of the node.
    --
    -- The result of the query is an 'ExceptT' monad, which we can run by using the 'runExceptT'
    -- function, which in turn gives us a @eEra@ value of type @Either AcquiringFailure AnyCardanoEra@.
    --
    -- This is an example of how to unwrap this value into a 'ShelleyBasedEra' based era, assuming the node
    -- is not still running Byron:
    --
    -- @
    -- Api.AnyShelleyBasedEra sbe :: Api.AnyShelleyBasedEra <- case eEra of
    --   Right (Api.AnyCardanoEra era) ->
    --     Api.caseByronOrShelleyBasedEra
    --       (error "Error, we are in Byron era")
    --       (return . Api.AnyShelleyBasedEra)
    --       era
    --   Left Shelley.AFPointTooOld -> error "Error, point queried in the chain is too old!"
    --   Left Shelley.AFPointNotOnChain -> error "Error, point queried is not on chain!"
    -- @
    --
    -- 'AFPointToolOld' and 'AFPointNotOnChain' should not happen either with 'VolatileTip' or 'ImmutableTip'.

    -- *** Obtaining the UTxO set

    -- | Now that we know the current era, we can query the node for the UTxO set similarly
    -- by using the 'QueryUTxO' query as follow:
    --
    -- @
    -- eUtxo <-
    --   runExceptT $
    --     Api.queryNodeLocalState
    --       connectionInfo
    --       Network.VolatileTip
    --       (Api.QueryInEra (Api.QueryInShelleyBasedEra sbe (Api.QueryUTxO Api.QueryUTxOWhole)))
    -- @
    --
    -- This time, @eUtxo@ has a nested type of @Either AcquiringFailure (Either EraMismatch (UTxO era))@.
    -- So we can unwrap it as follows:
    --
    -- @
    -- utxo <- case eUtxo of
    --   Right (Right (Api.UTxO utxo)) -> do
    --     return utxo
    --   Right (Left (Consensus.EraMismatch{Consensus.ledgerEraName, Consensus.otherEraName})) ->
    --     error
    --       ( "Error, we assumed era was "
    --           ++ show otherEraName
    --           ++ " but it was "
    --           ++ show ledgerEraName
    --       )
    --   Left Shelley.AFPointTooOld -> error "Error, point queried in the chain is too old!"
    --   Left Shelley.AFPointNotOnChain -> error "Error, point queried is not on chain!"
    -- @
    --
    -- The obtained @utxo@ is a standard @Map@ of type @Map TxIn (TxOut CtxUTxO era)@.

    -- ** Submitting a transaction

    -- | Let's assume we have a signed transaction of the right era that we want to submit to the node,
    -- and that it is in the variable @signedTx@ of type @Tx era@.
    --
    -- You can find out how to make such a transaction by looking at the documentation of the
    -- "Cardano.Api.Internal.Tx.Body" module.
    --
    -- We can send it to the node by using the 'submitTxToNodeLocal' function as follows:
    --
    -- @
    -- result <- Api.submitTxToNodeLocal connectionInfo (Api.TxInMode sbe signedTx)
    -- @
    --
    -- The result of the submission is a 'SubmitResult' value, which can be inspected as follows:
    --
    -- @
    -- case result of
    --   Api.SubmitSuccess -> putStrLn "Transaction submitted successfully!"
    --   Api.SubmitFail reason -> error $ "Error submitting transaction: " ++ show reason
    -- @
    --
    -- If the command succeeds, then the transaction will be on the node's mempool ready
    -- to be included in a block.

    -- * Node interaction

    -- | Operations that involve talking to a local Cardano node.
    connectToLocalNode
  , connectToLocalNodeWithVersion
  , LocalNodeConnectInfo (..)
  , LocalNodeClientParams (..)
  , mkLocalNodeClientParams
  , LocalNodeClientProtocols (..)
  , LocalChainSyncClient (..)
  , LocalNodeClientProtocolsInMode

    -- ** Modes

    -- | TODO move to Cardano.Api
  , ConsensusModeParams (..)
  , EpochSlots (..)
  --  connectToRemoteNode,

    -- *** Chain sync protocol
  , ChainSyncClient (..)
  , ChainSyncClientPipelined (..)
  , BlockInMode (..)

    -- *** Local tx submission
  , LocalTxSubmissionClient (..)
  , TxInMode (..)
  , TxValidationErrorInCardanoMode
  , TxValidationError
  , submitTxToNodeLocal
  , SubmitResult (..)

    -- *** Local state query
  , LocalStateQueryClient (..)
  , AcquiringFailure (..)
  , QueryInMode (..)
  , QueryInEra (..)
  , QueryInShelleyBasedEra (..)
  , queryNodeLocalState

    -- *** Local tx monitoring
  , LocalTxMonitorClient (..)
  , LocalTxMonitoringQuery (..)
  , LocalTxMonitoringResult (..)
  , Consensus.MempoolSizeAndCapacity (..)
  , queryTxMonitoringLocal
  , EraHistory (..)
  , getProgress

    -- *** Common queries
  , getLocalChainTip

    -- *** Helpers

  -- TODO: These should be exported via Cardano.Api.Mode
  , toAcquiringFailure
  , NodeToClientVersion (..)
  , UnsupportedNtcVersionError (..)
  )
where

import Cardano.Api.Internal.Block
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.IO
import Cardano.Api.Internal.IPC.Version
import Cardano.Api.Internal.InMode
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Monad.Error (ExceptT (..))
import Cardano.Api.Internal.NetworkId
import Cardano.Api.Internal.Protocol
import Cardano.Api.Internal.Query
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.Sign

import Cardano.Ledger.Api qualified as L
import Ouroboros.Consensus.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.Ledger.Query qualified as Consensus
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Consensus
import Ouroboros.Consensus.Ledger.SupportsProtocol qualified as Consensus
import Ouroboros.Consensus.Network.NodeToClient qualified as Consensus
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as Consensus
import Ouroboros.Consensus.Node.ProtocolInfo qualified as Consensus
import Ouroboros.Consensus.Protocol.TPraos qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Net
import Ouroboros.Network.Mux qualified as Net
import Ouroboros.Network.NodeToClient
  ( NodeToClientProtocols (..)
  , NodeToClientVersionData (..)
  )
import Ouroboros.Network.NodeToClient qualified as Net
import Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))
import Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined as Net.SyncP
import Ouroboros.Network.Protocol.LocalStateQuery.Client (LocalStateQueryClient (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as Net.Query
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
  ( LocalTxMonitorClient (..)
  , localTxMonitorClientPeer
  )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client qualified as CTxMon
import Ouroboros.Network.Protocol.LocalTxMonitor.Type qualified as Consensus
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( LocalTxSubmissionClient (..)
  , SubmitResult (..)
  )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx

import Control.Concurrent.STM
  ( TMVar
  , atomically
  , newEmptyTMVarIO
  , putTMVar
  , takeTMVar
  , tryPutTMVar
  )
import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Tracer (nullTracer)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Void (Void)
import GHC.Exts (IsList (..))
import Network.Mux qualified as Net

-- ----------------------------------------------------------------------------
-- The types for the client side of the node-to-client IPC protocols
--

-- | The protocols we can use with a local node. Use in conjunction with
-- 'connectToLocalNode'.
--
-- These protocols use the types from the rest of this API. The conversion
-- to\/from the types used by the underlying wire formats is handled by
-- 'connectToLocalNode'.
data LocalNodeClientProtocols block point tip slot tx txid txerr query m
  = LocalNodeClientProtocols
  { localChainSyncClient :: LocalChainSyncClient block point tip m
  , localTxSubmissionClient :: Maybe (LocalTxSubmissionClient tx txerr m ())
  , localStateQueryClient :: Maybe (LocalStateQueryClient block point query m ())
  , localTxMonitoringClient :: Maybe (LocalTxMonitorClient txid tx slot m ())
  }

data LocalChainSyncClient block point tip m
  = NoLocalChainSyncClient
  | LocalChainSyncClientPipelined (ChainSyncClientPipelined block point tip m ())
  | LocalChainSyncClient (ChainSyncClient block point tip m ())

-- public, exported
type LocalNodeClientProtocolsInMode =
  LocalNodeClientProtocols
    BlockInMode
    ChainPoint
    ChainTip
    SlotNo
    TxInMode
    TxIdInMode
    TxValidationErrorInCardanoMode
    QueryInMode
    IO

data LocalNodeConnectInfo
  = LocalNodeConnectInfo
  { localConsensusModeParams :: ConsensusModeParams
  , localNodeNetworkId :: NetworkId
  , localNodeSocketPath :: SocketPath
  }
  deriving Show

-- ----------------------------------------------------------------------------
-- Actually connect to the node
--

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers.
connectToLocalNode
  :: MonadIO m
  => LocalNodeConnectInfo
  -> LocalNodeClientProtocolsInMode
  -> m ()
connectToLocalNode localNodeConnectInfo handlers =
  connectToLocalNodeWithVersion localNodeConnectInfo (const handlers)

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers parameterized on the negotiated node-to-client protocol
-- version.
connectToLocalNodeWithVersion
  :: MonadIO m
  => LocalNodeConnectInfo
  -> (NodeToClientVersion -> LocalNodeClientProtocolsInMode)
  -> m ()
connectToLocalNodeWithVersion
  LocalNodeConnectInfo
    { localNodeSocketPath
    , localNodeNetworkId
    , localConsensusModeParams
    }
  clients =
    liftIO $ Net.withIOManager $ \iomgr -> do
      r <-
        Net.connectTo
          (Net.localSnocket iomgr)
          Net.NetworkConnectTracers
            { Net.nctMuxTracer = nullTracer
            , Net.nctHandshakeTracer = nullTracer
            }
          versionedProtocls
          (unFile localNodeSocketPath)
      case r of
        Left e -> throwIO e
        Right _ -> pure ()
   where
    versionedProtocls =
      -- First convert from the mode-parametrised view of things to the
      -- block-parametrised view and then do the final setup for the versioned
      -- bundles of mini-protocols.
      case mkLocalNodeClientParams localConsensusModeParams clients of
        LocalNodeClientParamsSingleBlock ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'
        LocalNodeClientParamsCardano ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'

mkVersionedProtocols
  :: forall block
   . ( Consensus.ShowQuery (Consensus.Query block)
     , ProtocolClient block
     )
  => NetworkId
  -> ProtocolClientInfoArgs block
  -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
  -> Net.Versions
       Net.NodeToClientVersion
       Net.NodeToClientVersionData
       ( Net.OuroborosApplicationWithMinimalCtx
           Net.InitiatorMode
           Net.LocalAddress
           LBS.ByteString
           IO
           ()
           Void
       )
mkVersionedProtocols networkid ptcl unversionedClients =
  -- TODO: really we should construct specific combinations of
  -- protocols for the versions we know about, with different protocol
  -- versions taking different sets of typed client protocols.
  Net.foldMapVersions
    ( \(ptclVersion, ptclBlockVersion) ->
        Net.versionedNodeToClientProtocols
          ptclVersion
          NodeToClientVersionData
            { networkMagic = toNetworkMagic networkid
            , query = False
            }
          (protocols (unversionedClients ptclVersion) ptclBlockVersion ptclVersion)
    )
    (toList (Consensus.supportedNodeToClientVersions proxy))
 where
  proxy :: Proxy block
  proxy = Proxy

  protocols
    :: LocalNodeClientProtocolsForBlock block
    -> Consensus.BlockNodeToClientVersion block
    -> NodeToClientVersion
    -> NodeToClientProtocols Net.InitiatorMode Net.LocalAddress LBS.ByteString IO () Void
  protocols
    LocalNodeClientProtocolsForBlock
      { localChainSyncClientForBlock
      , localTxSubmissionClientForBlock
      , localStateQueryClientForBlock
      , localTxMonitoringClientForBlock
      }
    ptclBlockVersion
    ptclVersion =
      NodeToClientProtocols
        { localChainSyncProtocol =
            Net.InitiatorProtocolOnly $ case localChainSyncClientForBlock of
              NoLocalChainSyncClient ->
                Net.mkMiniProtocolCbFromPeer $
                  const
                    (nullTracer, cChainSyncCodec, Net.chainSyncPeerNull)
              LocalChainSyncClient client ->
                Net.mkMiniProtocolCbFromPeer $
                  const
                    (nullTracer, cChainSyncCodec, Net.Sync.chainSyncClientPeer client)
              LocalChainSyncClientPipelined clientPipelined ->
                Net.mkMiniProtocolCbFromPeerPipelined $
                  const
                    (nullTracer, cChainSyncCodec, Net.SyncP.chainSyncClientPeerPipelined clientPipelined)
        , localTxSubmissionProtocol =
            Net.InitiatorProtocolOnly $
              Net.mkMiniProtocolCbFromPeer $
                const
                  ( nullTracer
                  , cTxSubmissionCodec
                  , maybe
                      Net.localTxSubmissionPeerNull
                      Net.Tx.localTxSubmissionClientPeer
                      localTxSubmissionClientForBlock
                  )
        , localStateQueryProtocol =
            Net.InitiatorProtocolOnly $
              Net.mkMiniProtocolCbFromPeerSt $
                const
                  ( nullTracer
                  , cStateQueryCodec
                  , Net.Query.StateIdle
                  , maybe
                      Net.localStateQueryPeerNull
                      Net.Query.localStateQueryClientPeer
                      localStateQueryClientForBlock
                  )
        , localTxMonitorProtocol =
            Net.InitiatorProtocolOnly $
              Net.mkMiniProtocolCbFromPeer $
                const
                  ( nullTracer
                  , cTxMonitorCodec
                  , maybe
                      Net.localTxMonitorPeerNull
                      localTxMonitorClientPeer
                      localTxMonitoringClientForBlock
                  )
        }
     where
      Consensus.Codecs
        { Consensus.cChainSyncCodec
        , Consensus.cTxMonitorCodec
        , Consensus.cTxSubmissionCodec
        , Consensus.cStateQueryCodec
        } = Consensus.clientCodecs codecConfig ptclBlockVersion ptclVersion

  codecConfig :: Consensus.CodecConfig block
  codecConfig =
    Consensus.pClientInfoCodecConfig
      (protocolClientInfo ptcl)

-- | This type defines the boundary between the mode-parametrised style used in
-- this API and the block-parametrised style used by the underlying network
-- and consensus libraries.
--
-- This interface itself is in the block-parametrised style, with the block
-- type itself being an hidden\/existential type.
--
-- It bundles together all the necessary class instances, the consensus
-- protocol client identifier, and the set of client side mini-protocol
-- handlers for the node-to-client protocol.
data LocalNodeClientParams where
  LocalNodeClientParamsSingleBlock
    :: ( ProtocolClient block
       , Consensus.LedgerSupportsProtocol
           ( Consensus.ShelleyBlock
               (Consensus.TPraos Consensus.StandardCrypto)
               (Consensus.ShelleyEra Consensus.StandardCrypto)
           )
       )
    => ProtocolClientInfoArgs block
    -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
    -> LocalNodeClientParams
  LocalNodeClientParamsCardano
    :: (ProtocolClient block, CardanoHardForkConstraints (ConsensusCryptoForBlock block))
    => ProtocolClientInfoArgs block
    -> (NodeToClientVersion -> LocalNodeClientProtocolsForBlock block)
    -> LocalNodeClientParams

data LocalNodeClientProtocolsForBlock block
  = LocalNodeClientProtocolsForBlock
  { localChainSyncClientForBlock
      :: LocalChainSyncClient
           block
           (Consensus.Point block)
           (Net.Tip block)
           IO
  , localStateQueryClientForBlock
      :: Maybe
           ( LocalStateQueryClient
               block
               (Consensus.Point block)
               (Consensus.Query block)
               IO
               ()
           )
  , localTxSubmissionClientForBlock
      :: Maybe
           ( LocalTxSubmissionClient
               (Consensus.GenTx block)
               (Consensus.ApplyTxErr block)
               IO
               ()
           )
  , localTxMonitoringClientForBlock
      :: Maybe
           ( LocalTxMonitorClient
               (Consensus.TxId (Consensus.GenTx block))
               (Consensus.GenTx block)
               SlotNo
               IO
               ()
           )
  }

-- | Convert from the mode-parametrised style to the block-parametrised style.
mkLocalNodeClientParams
  :: ConsensusModeParams
  -> (NodeToClientVersion -> LocalNodeClientProtocolsInMode)
  -> LocalNodeClientParams
mkLocalNodeClientParams modeparams clients =
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
        (convLocalNodeClientProtocols . clients)

convLocalNodeClientProtocols
  :: ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => LocalNodeClientProtocolsInMode
  -> LocalNodeClientProtocolsForBlock block
convLocalNodeClientProtocols
  LocalNodeClientProtocols
    { localChainSyncClient
    , localTxSubmissionClient
    , localStateQueryClient
    , localTxMonitoringClient
    } =
    LocalNodeClientProtocolsForBlock
      { localChainSyncClientForBlock = case localChainSyncClient of
          NoLocalChainSyncClient -> NoLocalChainSyncClient
          LocalChainSyncClientPipelined clientPipelined -> LocalChainSyncClientPipelined $ convLocalChainSyncClientPipelined clientPipelined
          LocalChainSyncClient client -> LocalChainSyncClient $ convLocalChainSyncClient client
      , localTxSubmissionClientForBlock = convLocalTxSubmissionClient <$> localTxSubmissionClient
      , localStateQueryClientForBlock = convLocalStateQueryClient <$> localStateQueryClient
      , localTxMonitoringClientForBlock = convLocalTxMonitoringClient <$> localTxMonitoringClient
      }

convLocalTxMonitoringClient
  :: forall block m a
   . ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => Functor m
  => LocalTxMonitorClient TxIdInMode TxInMode SlotNo m a
  -> LocalTxMonitorClient (Consensus.TxId (Consensus.GenTx block)) (Consensus.GenTx block) SlotNo m a
convLocalTxMonitoringClient =
  mapLocalTxMonitoringClient
    toConsensusTxId
    fromConsensusGenTx

convLocalChainSyncClient
  :: forall block m a
   . ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => Functor m
  => ChainSyncClient BlockInMode ChainPoint ChainTip m a
  -> ChainSyncClient block (Net.Point block) (Net.Tip block) m a
convLocalChainSyncClient =
  Net.Sync.mapChainSyncClient
    toConsensusPointHF
    fromConsensusPointHF
    fromConsensusBlock
    fromConsensusTip

convLocalChainSyncClientPipelined
  :: forall block m a
   . ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => Functor m
  => ChainSyncClientPipelined BlockInMode ChainPoint ChainTip m a
  -> ChainSyncClientPipelined block (Net.Point block) (Net.Tip block) m a
convLocalChainSyncClientPipelined =
  mapChainSyncClientPipelined
    toConsensusPointHF
    fromConsensusPointHF
    fromConsensusBlock
    fromConsensusTip

convLocalTxSubmissionClient
  :: forall block m a
   . ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => Functor m
  => LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode m a
  -> LocalTxSubmissionClient (Consensus.GenTx block) (Consensus.ApplyTxErr block) m a
convLocalTxSubmissionClient =
  Net.Tx.mapLocalTxSubmissionClient toConsensusGenTx fromConsensusApplyTxErr

convLocalStateQueryClient
  :: forall block m a
   . ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => Functor m
  => LocalStateQueryClient BlockInMode ChainPoint QueryInMode m a
  -> LocalStateQueryClient block (Consensus.Point block) (Consensus.Query block) m a
convLocalStateQueryClient =
  Net.Query.mapLocalStateQueryClient
    toConsensusPointHF
    toConsensusQuery
    fromConsensusQueryResult

-- TODO: Move to consensus
mapLocalTxMonitoringClient
  :: forall txid txid' tx tx' m a
   . ()
  => Functor m
  => (txid -> txid')
  -> (tx' -> tx)
  -> LocalTxMonitorClient txid tx SlotNo m a
  -> LocalTxMonitorClient txid' tx' SlotNo m a
mapLocalTxMonitoringClient convTxid convTx ltxmc =
  let LocalTxMonitorClient idleEff = ltxmc
   in LocalTxMonitorClient (fmap convClientStateIdle idleEff)
 where
  convClientStateIdle
    :: CTxMon.ClientStIdle txid tx SlotNo m a
    -> CTxMon.ClientStIdle txid' tx' SlotNo m a
  convClientStateIdle (CTxMon.SendMsgAcquire f) =
    CTxMon.SendMsgAcquire $ (fmap . fmap) convClientStateAcquired f
  convClientStateIdle (CTxMon.SendMsgDone a) = CTxMon.SendMsgDone a

  convClientStateAcquired
    :: CTxMon.ClientStAcquired txid tx SlotNo m a
    -> CTxMon.ClientStAcquired txid' tx' SlotNo m a
  convClientStateAcquired (CTxMon.SendMsgNextTx f) =
    CTxMon.SendMsgNextTx (\mTx -> convClientStateAcquired <$> f (convTx <$> mTx))
  convClientStateAcquired (CTxMon.SendMsgHasTx txid f) =
    CTxMon.SendMsgHasTx (convTxid txid) ((fmap . fmap) convClientStateAcquired f)
  convClientStateAcquired (CTxMon.SendMsgGetSizes f) =
    CTxMon.SendMsgGetSizes $ (fmap . fmap) convClientStateAcquired f
  convClientStateAcquired (CTxMon.SendMsgAwaitAcquire f) =
    CTxMon.SendMsgAwaitAcquire $ (fmap . fmap) convClientStateAcquired f
  convClientStateAcquired (CTxMon.SendMsgRelease eff) =
    CTxMon.SendMsgRelease (convClientStateIdle <$> eff)

-- ----------------------------------------------------------------------------
-- Wrappers for specific protocol use-cases
--

-- TODO: change this query to be just a protocol client handler to be used with
-- connectToLocalNode. This would involve changing connectToLocalNode to be
-- able to return protocol handler results properly.

-- | Establish a connection to a node and execute a single query using the
-- local state query protocol.
data AcquiringFailure
  = AFPointTooOld
  | AFPointNotOnChain
  deriving (Eq, Show)

toAcquiringFailure :: Net.Query.AcquireFailure -> AcquiringFailure
toAcquiringFailure AcquireFailurePointTooOld = AFPointTooOld
toAcquiringFailure AcquireFailurePointNotOnChain = AFPointNotOnChain

queryNodeLocalState
  :: forall result
   . ()
  => LocalNodeConnectInfo
  -> Net.Query.Target ChainPoint
  -> QueryInMode result
  -> ExceptT AcquiringFailure IO result
queryNodeLocalState connctInfo mpoint query = do
  resultVar <- liftIO newEmptyTMVarIO
  connectToLocalNode
    connctInfo
    LocalNodeClientProtocols
      { localChainSyncClient = NoLocalChainSyncClient
      , localStateQueryClient = Just (singleQuery mpoint resultVar)
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
  ExceptT $ atomically (takeTMVar resultVar)
 where
  singleQuery
    :: Net.Query.Target ChainPoint
    -> TMVar (Either AcquiringFailure result)
    -> Net.Query.LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
  singleQuery mPointVar' resultVar' =
    LocalStateQueryClient $ do
      pure $
        Net.Query.SendMsgAcquire mPointVar' $
          Net.Query.ClientStAcquiring
            { Net.Query.recvMsgAcquired =
                pure $
                  Net.Query.SendMsgQuery query $
                    Net.Query.ClientStQuerying
                      { Net.Query.recvMsgResult = \result -> do
                          atomically $ putTMVar resultVar' (Right result)

                          pure $
                            Net.Query.SendMsgRelease $
                              pure $
                                Net.Query.SendMsgDone ()
                      }
            , Net.Query.recvMsgFailure = \failure -> do
                atomically $ putTMVar resultVar' (Left (toAcquiringFailure failure))
                pure $ Net.Query.SendMsgDone ()
            }

submitTxToNodeLocal
  :: MonadIO m
  => LocalNodeConnectInfo
  -> TxInMode
  -> m (Net.Tx.SubmitResult TxValidationErrorInCardanoMode)
submitTxToNodeLocal connctInfo tx = do
  resultVar <- liftIO newEmptyTMVarIO
  connectToLocalNode
    connctInfo
    LocalNodeClientProtocols
      { localChainSyncClient = NoLocalChainSyncClient
      , localTxSubmissionClient = Just (localTxSubmissionClientSingle resultVar)
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }
  liftIO $ atomically (takeTMVar resultVar)
 where
  localTxSubmissionClientSingle
    :: ()
    => TMVar (Net.Tx.SubmitResult TxValidationErrorInCardanoMode)
    -> Net.Tx.LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode IO ()
  localTxSubmissionClientSingle resultVar =
    LocalTxSubmissionClient $
      pure $
        Net.Tx.SendMsgSubmitTx tx $ \result -> do
          atomically $ putTMVar resultVar result
          pure (Net.Tx.SendMsgDone ())

data LocalTxMonitoringResult
  = -- | Slot number at which the mempool snapshot was taken
    LocalTxMonitoringTxExists
      TxId
      SlotNo
  | -- | Slot number at which the mempool snapshot was taken
    LocalTxMonitoringTxDoesNotExist
      TxId
      SlotNo
  | -- | Slot number at which the mempool snapshot was taken
    LocalTxMonitoringNextTx
      (Maybe TxInMode)
      SlotNo
  | -- | Slot number at which the mempool snapshot was taken
    LocalTxMonitoringMempoolSizeAndCapacity
      Consensus.MempoolSizeAndCapacity
      SlotNo

instance ToJSON LocalTxMonitoringResult where
  toJSON result =
    object $ case result of
      LocalTxMonitoringTxExists tx slot ->
        [ "exists" .= True
        , "txId" .= tx
        , "slot" .= slot
        ]
      LocalTxMonitoringTxDoesNotExist tx slot ->
        [ "exists" .= False
        , "txId" .= tx
        , "slot" .= slot
        ]
      LocalTxMonitoringNextTx txInMode slot ->
        [ "nextTx" .= txId
        , "slot" .= slot
        ]
       where
        txId = case txInMode of
          Just (TxInMode _ tx) -> Just $ getTxId $ getTxBody tx
          -- TODO: support fetching the ID of a Byron Era transaction
          _ -> Nothing
      LocalTxMonitoringMempoolSizeAndCapacity mempool slot ->
        [ "capacityInBytes" .= Consensus.capacityInBytes mempool
        , "sizeInBytes" .= Consensus.sizeInBytes mempool
        , "numberOfTxs" .= Consensus.numberOfTxs mempool
        , "slot" .= slot
        ]

data LocalTxMonitoringQuery
  = -- | Query if a particular tx exists in the mempool. Note that, the absence
    -- of a transaction does not imply anything about how the transaction was
    -- processed: it may have been dropped, or inserted in a block.
    LocalTxMonitoringQueryTx TxIdInMode
  | -- | The mempool is modeled as an ordered list of transactions and thus, can
    -- be traversed linearly. 'LocalTxMonitoringSendNextTx' requests the next transaction from the
    -- current list. This must be a transaction that was not previously sent to
    -- the client for this particular snapshot.
    LocalTxMonitoringSendNextTx
  | -- | Ask the server about the current mempool's capacity and sizes. This is
    -- fixed in a given snapshot.
    LocalTxMonitoringMempoolInformation

queryTxMonitoringLocal
  :: MonadIO m
  => LocalNodeConnectInfo
  -> LocalTxMonitoringQuery
  -> m LocalTxMonitoringResult
queryTxMonitoringLocal connectInfo localTxMonitoringQuery = do
  resultVar <- liftIO newEmptyTMVarIO

  let client = case localTxMonitoringQuery of
        LocalTxMonitoringQueryTx txidInMode ->
          localTxMonitorClientTxExists txidInMode resultVar
        LocalTxMonitoringSendNextTx ->
          localTxMonitorNextTx resultVar
        LocalTxMonitoringMempoolInformation ->
          localTxMonitorMempoolInfo resultVar

  connectToLocalNode
    connectInfo
    LocalNodeClientProtocols
      { localChainSyncClient = NoLocalChainSyncClient
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Just client
      }
  liftIO $ atomically (takeTMVar resultVar)
 where
  localTxMonitorClientTxExists
    :: ()
    => TxIdInMode
    -> TMVar LocalTxMonitoringResult
    -> LocalTxMonitorClient TxIdInMode TxInMode SlotNo IO ()
  localTxMonitorClientTxExists tIdInMode@(TxIdInMode _ txid) resultVar = do
    LocalTxMonitorClient $
      return $
        CTxMon.SendMsgAcquire $ \slt -> do
          return $ CTxMon.SendMsgHasTx tIdInMode $ \txPresentBool -> do
            if txPresentBool
              then atomically . putTMVar resultVar $ LocalTxMonitoringTxExists txid slt
              else atomically . putTMVar resultVar $ LocalTxMonitoringTxDoesNotExist txid slt
            return $ CTxMon.SendMsgRelease $ return $ CTxMon.SendMsgDone ()

  localTxMonitorNextTx
    :: ()
    => TMVar LocalTxMonitoringResult
    -> LocalTxMonitorClient TxIdInMode TxInMode SlotNo IO ()
  localTxMonitorNextTx resultVar =
    LocalTxMonitorClient $ return $ do
      CTxMon.SendMsgAcquire $ \slt -> do
        return $ CTxMon.SendMsgNextTx $ \mTx -> do
          atomically $ putTMVar resultVar $ LocalTxMonitoringNextTx mTx slt
          return $ CTxMon.SendMsgRelease $ return $ CTxMon.SendMsgDone ()

  localTxMonitorMempoolInfo
    :: ()
    => TMVar LocalTxMonitoringResult
    -> LocalTxMonitorClient TxIdInMode TxInMode SlotNo IO ()
  localTxMonitorMempoolInfo resultVar =
    LocalTxMonitorClient $ return $ do
      CTxMon.SendMsgAcquire $ \slt -> do
        return $ CTxMon.SendMsgGetSizes $ \mempoolCapacity -> do
          atomically $ putTMVar resultVar $ LocalTxMonitoringMempoolSizeAndCapacity mempoolCapacity slt
          return $ CTxMon.SendMsgRelease $ return $ CTxMon.SendMsgDone ()

-- ----------------------------------------------------------------------------
-- Get tip as 'ChainPoint'
--

getLocalChainTip
  :: MonadIO m
  => LocalNodeConnectInfo
  -> m ChainTip
getLocalChainTip localNodeConInfo = do
  resultVar <- liftIO newEmptyTMVarIO
  connectToLocalNode
    localNodeConInfo
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ chainSyncGetCurrentTip resultVar
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }
  liftIO . atomically $ takeTMVar resultVar

chainSyncGetCurrentTip
  :: ()
  => TMVar ChainTip
  -> ChainSyncClient BlockInMode ChainPoint ChainTip IO ()
chainSyncGetCurrentTip tipVar =
  ChainSyncClient $ pure clientStIdle
 where
  clientStIdle :: Net.Sync.ClientStIdle BlockInMode ChainPoint ChainTip IO ()
  clientStIdle =
    Net.Sync.SendMsgRequestNext (pure ()) clientStNext

  clientStNext :: Net.Sync.ClientStNext BlockInMode ChainPoint ChainTip IO ()
  clientStNext =
    Net.Sync.ClientStNext
      { Net.Sync.recvMsgRollForward = \_block tip -> ChainSyncClient $ do
          void $ atomically $ tryPutTMVar tipVar tip
          pure $ Net.Sync.SendMsgDone ()
      , Net.Sync.recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
          void $ atomically $ tryPutTMVar tipVar tip
          pure $ Net.Sync.SendMsgDone ()
      }
