{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server
  ( runRpcServer
  )
where

import Cardano.Api
import Cardano.Rpc.Proto.Api.Node qualified as Rpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as UtxoRpc
import Cardano.Rpc.Server.Config
import Cardano.Rpc.Server.Internal.Env
import Cardano.Rpc.Server.Internal.Monad
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Query
import Cardano.Rpc.Server.Internal.UtxoRpc.Submit

import RIO

import Control.Tracer
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field (field)
import Network.GRPC.Common
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType
import Network.GRPC.Spec hiding (Identity)

import Proto.Google.Protobuf.Empty

-- Individual handlers

getEraMethod :: MonadRpc e m => Proto Empty -> m (Proto Rpc.CurrentEra)
getEraMethod _ = pure mockNodeResponse

-- Mock node response
mockNodeResponse :: Proto Rpc.CurrentEra
mockNodeResponse = Proto $ defMessage & field @"era" .~ Rpc.Conway

-- Server top level
methodsNodeRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf Rpc.Node)
methodsNodeRpc = Method (mkNonStreaming getEraMethod) NoMoreMethods

methodsUtxoRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.QueryService)
methodsUtxoRpc =
  Method (mkNonStreaming readParamsMethod)
    . Method (mkNonStreaming readUtxosMethod)
    $ NoMoreMethods

methodsUtxoRpcSubmit
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.SubmitService)
methodsUtxoRpcSubmit =
  Method (mkNonStreaming submitTxMethod) NoMoreMethods

runRpcServer
  :: Tracer IO String
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
    traceWith tracer $ "Exception when processing RPC request:\n" <> displayException e

  handleFatalExceptions :: (HasCallStack => IO ()) -> IO ()
  handleFatalExceptions = handleAny $ \e ->
    traceWith tracer $ "RPC server fatal error: " <> displayException e
