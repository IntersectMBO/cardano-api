{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.Internal.Env
  ( RpcEnv (..)
  , mkLocalNodeConnectInfo
  )
where

import Cardano.Api
import Cardano.Rpc.Server.Config
import Cardano.Rpc.Server.Internal.Tracing
import Cardano.Rpc.Server.NodeKernelAccess.Type (NodeKernelAccess)

import Control.Tracer (Tracer)
import Data.IORef

data RpcEnv = RpcEnv
  { config :: !RpcConfig
  , tracer :: forall m. MonadIO m => Tracer m TraceRpc
  , -- TODO replace with better connection management than one connection per rpc request
    rpcLocalNodeConnectInfo :: !LocalNodeConnectInfo
  , rpcNodeKernelAccess :: !(IORef (Maybe NodeKernelAccess))
  }

mkLocalNodeConnectInfo :: SocketPath -> NetworkMagic -> LocalNodeConnectInfo
mkLocalNodeConnectInfo nodeSocketPath networkMagic =
  LocalNodeConnectInfo
    { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
    , localNodeNetworkId = fromNetworkMagic networkMagic
    , localNodeSocketPath = nodeSocketPath
    }
