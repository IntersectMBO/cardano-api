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

    -- * Traces
  , TraceRpc (..)
  , TraceRpcSubmit (..)
  , TraceRpcQuery (..)
  , TraceSpanEvent (..)
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.Node qualified as Rpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Config
import Cardano.Rpc.Server.Internal.Env
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Node
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
import Cardano.Rpc.Server.Internal.UtxoRpc.Submit

import RIO

import Control.Tracer
import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

-- Server top level
methodsNodeRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf Rpc.Node)
methodsNodeRpc =
  Method (mkNonStreaming getEraMethod)
    . Method (mkNonStreaming getProtocolParamsJsonMethod)
    $ NoMoreMethods

methodsUtxoRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.QueryService)
methodsUtxoRpc =
  Method (mkNonStreaming $ wrapInSpan TraceRpcQueryParamsSpan . readParamsMethod)
    . Method (mkNonStreaming $ wrapInSpan TraceRpcQueryReadUtxosSpan . readUtxosMethod)
    $ NoMoreMethods

methodsUtxoRpcSubmit
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.SubmitService)
methodsUtxoRpcSubmit =
  Method (mkNonStreaming $ wrapInSpan TraceRpcSubmitSpan . submitTxMethod) NoMoreMethods

runRpcServer
  :: Tracer IO TraceRpc
  -> IO (RpcConfig, NetworkMagic)
  -- ^ action which reloads RPC configuration
  -> IO ()
runRpcServer tracer loadRpcConfig = handleFatalExceptions $ do
  ( rpcConfig@RpcConfig
      { isEnabled = Identity isEnabled
      , rpcSocketPath = Identity (File rpcSocketPathFp)
      , nodeSocketPath = Identity nodeSocketPath
      }
    , networkMagic
    ) <-
    loadRpcConfig
  let config =
        ServerConfig
          { serverInsecure = Just $ InsecureUnix rpcSocketPathFp
          , serverSecure = Nothing
          }
      rpcEnv =
        RpcEnv
          { config = rpcConfig
          , tracer = natTracer liftIO tracer
          , rpcLocalNodeConnectInfo = mkLocalNodeConnectInfo nodeSocketPath networkMagic
          }

  -- TODO this is logged by node configuration already, so it would make sense to log it again when
  -- configuration gets reloaded
  -- traceWith tracer $ "RPC configuration: " <> show rpcConfig

  when isEnabled $
    runRIO rpcEnv $
      withRunInIO $ \runInIO ->
        runServerWithHandlers serverParams config . fmap (hoistSomeRpcHandler runInIO) $
          mconcat
            [ fromMethods methodsNodeRpc
            , fromMethods methodsUtxoRpc
            , fromMethods methodsUtxoRpcSubmit
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
