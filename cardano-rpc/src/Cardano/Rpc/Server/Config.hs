{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.Config
  ( RpcConfig
  , PartialRpcConfig
  , RpcConfigF (..)
  , RpcEndpoint (..)
  , RpcTlsFiles (..)
  , TlsCertificate
  , TlsPrivateKey
  , defaultRpcListenAddress
  , makeRpcConfig
  , nodeSocketPathToRpcSocketPath
  )
where

import Cardano.Api

import RIO

import Data.IP (IP)
import Data.Monoid
import Network.Socket (PortNumber)
import System.FilePath (takeDirectory, (</>))

import Generic.Data (gmappend, gmempty)

type PartialRpcConfig = RpcConfigF Last

type RpcConfig = RpcConfigF Identity

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

-- | Endpoint the RPC server listens on. Exactly one listener is active at a
-- time.
data RpcEndpoint
  = RpcEndpointUnixSocket !SocketPath
  | -- | IP address and port of the HTTP/2 without TLS (h2c) listener.
    RpcEndpointHttp !IP !PortNumber
  | -- | IP address, port and TLS credential files of the HTTP/2 over TLS
    -- listener.
    RpcEndpointHttps !IP !PortNumber !RpcTlsFiles
  deriving (Eq, Show)

instance Pretty RpcEndpoint where
  pretty = \case
    RpcEndpointUnixSocket (File socketPath) -> pretty socketPath
    RpcEndpointHttp host port -> pshow host <> ":" <> pshow port
    RpcEndpointHttps host port _ -> pshow host <> ":" <> pshow port <> " (TLS)"

-- | TLS credential files for the RPC server, PEM format.
data RpcTlsFiles = RpcTlsFiles
  { certificateFile :: !(File TlsCertificate In)
  -- ^ server X.509 certificate
  , privateKeyFile :: !(File TlsPrivateKey In)
  -- ^ private key matching the certificate
  , chainCertificateFiles :: ![File TlsCertificate In]
  -- ^ intermediate chain certificates, if any
  }
  deriving (Eq, Show)

-- | Empty content tag for 'File' identifying a TLS certificate file.
data TlsCertificate

-- | Empty content tag for 'File' identifying a TLS private key file.
data TlsPrivateKey

-- | Default IP address the HTTP/2 listener binds to when only a port is configured.
defaultRpcListenAddress :: IP
defaultRpcListenAddress = "127.0.0.1"

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
        -- Default to a non-existing path. Irrelevant when the RPC server is disabled; when enabled, the validation below requires an explicit node socket path.
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
