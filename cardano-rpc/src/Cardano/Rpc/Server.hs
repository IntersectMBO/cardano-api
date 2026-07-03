{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server
  ( runRpcServer
  , NodeKernelAccess
  , mkNodeKernelAccess

    -- * Traces
  , TraceRpc (..)
  , TraceRpcSubmit (..)
  , TraceRpcQuery (..)
  , TraceRpcSync (..)
  , TraceSpanEvent (..)
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.Node qualified as Rpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Sync qualified as UtxoRpc
import Cardano.Rpc.Server.Config
import Cardano.Rpc.Server.Internal.Env
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Node
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.Internal.UtxoRpc.Eval
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
import Cardano.Rpc.Server.Internal.UtxoRpc.Submit
import Cardano.Rpc.Server.Internal.UtxoRpc.Sync
import Cardano.Rpc.Server.NodeKernelAccess (NodeKernelAccess, mkNodeKernelAccess)

import RIO

import Control.Tracer
import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

-- | gRPC method table for the @Node@ service.
methodsNodeRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf Rpc.Node)
methodsNodeRpc =
  Method (mkNonStreaming getEraMethod)
    . Method (mkNonStreaming getProtocolParamsJsonMethod)
    $ NoMoreMethods

-- | gRPC method table for the UTxO RPC @QueryService@.
methodsUtxoRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.QueryService)
methodsUtxoRpc =
  Method (mkNonStreaming $ wrapInSpan TraceRpcQueryParamsSpan . readParamsMethod)
    . Method (mkNonStreaming $ wrapInSpan TraceRpcQueryReadUtxosSpan . readUtxosMethod)
    . Method (mkNonStreaming $ wrapInSpan TraceRpcQuerySearchUtxosSpan . searchUtxosMethod)
    $ NoMoreMethods

-- | gRPC method table for the UTxO RPC @SubmitService@.
methodsUtxoRpcSubmit
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.SubmitService)
methodsUtxoRpcSubmit =
  Method (mkNonStreaming $ wrapInSpan TraceRpcEvalTxSpan . evalTxMethod)
    . Method (mkNonStreaming $ wrapInSpan TraceRpcSubmitSpan . submitTxMethod)
    $ NoMoreMethods

-- | gRPC method table for the UTxO RPC @SyncService@.
-- Method order must match 'ServiceMethods': dumpHistory, fetchBlock, followTip, readTip.
methodsSyncRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.SyncService)
methodsSyncRpc =
  Method (mkNonStreaming $ const unimplemented) -- dumpHistory
    . Method (mkNonStreaming $ wrapInSpan TraceRpcFetchBlockSpan . fetchBlockMethod)
    . Method (mkServerStreaming $ \_ _ -> unimplemented) -- followTip
    . Method (mkNonStreaming $ const unimplemented) -- readTip
    $ NoMoreMethods
 where
  unimplemented =
    throwIO
      GrpcException
        { grpcError = GrpcUnimplemented
        , grpcErrorMessage = Just "Not yet implemented"
        , grpcErrorDetails = Nothing
        , grpcErrorMetadata = []
        }

-- | Start the gRPC server, registering all RPC service handlers.
-- Does nothing when the RPC server is disabled in configuration.
runRpcServer
  :: Tracer IO TraceRpc
  -- ^ Tracer for RPC lifecycle and error events
  -> RpcConfig
  -- ^ Server configuration
  -> NetworkMagic
  -- ^ Network discriminant
  -> IORef (Maybe NodeKernelAccess)
  -- ^ Node kernel access, populated when the kernel is ready
  -> IO ()
runRpcServer tracer rpcConfig networkMagic nodeKernelAccessRef = handleFatalExceptions $ do
  let RpcConfig
        { isEnabled = Identity isEnabled
        , rpcSocketPath = Identity (File rpcSocketPathFp)
        , nodeSocketPath = Identity nodeSocketPath
        } = rpcConfig
      config =
        ServerConfig
          { serverInsecure = Just $ InsecureUnix rpcSocketPathFp
          , serverSecure = Nothing
          }
      rpcEnv =
        RpcEnv
          { config = rpcConfig
          , tracer = natTracer liftIO tracer
          , rpcLocalNodeConnectInfo = mkLocalNodeConnectInfo nodeSocketPath networkMagic
          , rpcNodeKernelAccess = nodeKernelAccessRef
          }

  when isEnabled $
    runRIO rpcEnv $
      withRunInIO $ \runInIO ->
        runServerWithHandlers serverParams config . fmap (hoistSomeRpcHandler runInIO) $
          mconcat
            [ fromMethods methodsNodeRpc
            , fromMethods methodsUtxoRpc
            , fromMethods methodsUtxoRpcSubmit
            , fromMethods methodsSyncRpc
            ]
 where
  serverParams :: ServerParams
  serverParams = def{serverTopLevel = topLevelHandler}

  -- Top level hook for request handlers, handle exceptions
  topLevelHandler :: RequestHandler () -> RequestHandler ()
  topLevelHandler h unmask req resp = catchAny (h unmask req resp) $ \e ->
    traceWith tracer $ TraceRpcError e

  handleFatalExceptions :: (HasCallStack => IO ()) -> IO ()
  handleFatalExceptions = handleAny $ \e ->
    traceWith tracer $ TraceRpcFatalError e
