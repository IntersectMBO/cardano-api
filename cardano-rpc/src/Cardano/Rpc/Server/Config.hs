{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.Config
  ( RpcConfig
  , PartialRpcConfig
  , RpcConfigF (..)
  , RpcEndpoint (..)
  , defaultRpcListenAddress
  , makeRpcConfig
  , nodeSocketPathToRpcSocketPath
  )
where

import Cardano.Api

import RIO

import Data.Monoid
import Network.Socket (PortNumber)
import System.FilePath (takeDirectory, (</>))

import Generic.Data (gmappend, gmempty)

type PartialRpcConfig = RpcConfigF Last

type RpcConfig = RpcConfigF Identity

-- | Endpoint the RPC server listens on. Exactly one listener is active at a
-- time. Future transports (for example TLS) are added as new constructors.
data RpcEndpoint
  = RpcEndpointUnixSocket !SocketPath
  | -- | host and port of the TCP listener, HTTP/2 without TLS. The host is
    -- always concrete: config parsers apply 'defaultRpcListenAddress' when
    -- only a port was provided. Port 0 makes the operating system choose.
    RpcEndpointTcp !Text !PortNumber
  deriving (Eq, Show)

-- | Default host the TCP listener binds to when only a port is configured.
defaultRpcListenAddress :: Text
defaultRpcListenAddress = "127.0.0.1"

-- | RPC server configuration, which is a part of cardano-node configuration.
data RpcConfigF m = RpcConfig
  { isEnabled :: !(m Bool)
  -- ^ whether the RPC server is enabled
  , rpcEndpoint :: !(m RpcEndpoint)
  -- ^ endpoint where the RPC server listens
  , nodeSocketPath :: !(m SocketPath)
  -- ^ cardano-node socket path. Only valid if RPC endpoint is enabled.
  }

deriving instance Show (RpcConfigF Identity)

deriving instance Eq (RpcConfigF Identity)

deriving instance Show (RpcConfigF Last)

deriving instance Eq (RpcConfigF Last)

deriving instance Generic (RpcConfigF Last)

instance Semigroup (RpcConfigF Last) where
  (<>) = gmappend

instance Monoid (RpcConfigF Last) where
  mempty = gmempty

-- | Build RPC Config
--
-- Uses the following defaults if the values are not provided
-- * RPC is disabled
-- * the endpoint is a unix socket, @rpc.sock@, placed in the same path as the node socket
--
-- Validates if the node socket is enabled if RPC is enabled.
makeRpcConfig
  :: MonadError String m
  => PartialRpcConfig
  -> m RpcConfig
makeRpcConfig
  RpcConfig
    { isEnabled = Last mIsEnabled
    , rpcEndpoint = Last mRpcEndpoint
    , nodeSocketPath = Last mNodeSocketPath
    } = do
    let isEnabled = fromMaybe False mIsEnabled
        -- default to a some non-existing path. Does not matter if the gRPC endpoint is disabled
        nodeSocketPath = fromMaybe "./node.socket" mNodeSocketPath
        rpcEndpoint = fromMaybe (RpcEndpointUnixSocket $ nodeSocketPathToRpcSocketPath nodeSocketPath) mRpcEndpoint
    when (isEnabled && isNothing mNodeSocketPath) $
      throwError
        "Configuration error: gRPC endpoint was enabled but node socket file was not specified. Cannot run gRPC server without node socket."
    pure
      RpcConfig
        { isEnabled = pure isEnabled
        , rpcEndpoint = pure rpcEndpoint
        , nodeSocketPath = pure nodeSocketPath
        }

-- | Convert node socket path to a default rpc socket path.
-- By default it's @rpc.sock@ in the same directory as node socket path.
nodeSocketPathToRpcSocketPath :: SocketPath -> SocketPath
nodeSocketPathToRpcSocketPath nodeSocketPath = do
  let socketDir = takeDirectory $ unFile nodeSocketPath
  File $ socketDir </> "rpc.sock"
