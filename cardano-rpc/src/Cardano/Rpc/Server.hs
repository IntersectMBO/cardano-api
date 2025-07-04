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
import Cardano.Rpc.Server.Config
import Cardano.Rpc.Server.Internal.Monad

import RIO

import Control.Tracer
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Field (field)
import Network.GRPC.Common
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

runRpcServer
  :: Tracer IO String
  -> IO (RpcConfig, NetworkMagic)
  -- ^ action which reloads RPC configuration
  -> IO ()
runRpcServer tracer loadRpcConfig = handleExceptions $ do
  ( RpcConfig
      { isEnabled = Identity isEnabled
      , rpcSocketPath = Identity (File rpcSocketPathFp)
      }
    , _networkMagic
    ) <-
    loadRpcConfig
  let config =
        ServerConfig
          { serverInsecure = Just $ InsecureUnix rpcSocketPathFp
          , serverSecure = Nothing
          }

  -- TODO this is logged by node configuration already, so it would make sense to log it again when
  -- configuration gets reloaded
  -- putTrace $ "RPC configuration: " <> show rpcConfig

  when isEnabled $
    runServerWithHandlers def config $
      mconcat
        [ fromMethods methodsNodeRpc
        ]
 where
  handleExceptions :: (HasCallStack => IO ()) -> IO ()
  handleExceptions = handleAny $ \e ->
    putTrace $ "RPC server fatal error: " <> displayException e

  putTrace = traceWith tracer
