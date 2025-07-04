{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Cardano.Api (MonadError (..))
import Cardano.Api.IO (File (..))
import Cardano.Api.Monad.Error (MonadError)
import Cardano.Api.Network (NetworkMagic (..))

import Cardano.Rpc.Server (runRpcServer)
import Cardano.Rpc.Server.Config (RpcConfigF (..), makeRpcConfig)
import Cardano.Rpc.Server.Internal.Env (RpcEnv (rpcLocalNodeConnectInfo))

import Control.Exception (throwIO)
import Control.Tracer (nullTracer)
import Data.Monoid (Last (..))
import GHC.IO.Exception (IOErrorType (OtherError), IOException (..))

main :: IO ()
main = do
  rpcConfig <-
    throwToIO $
      makeRpcConfig
        RpcConfig
          { isEnabled = Last (Just True)
          , rpcSocketPath = Last (Just (File "/Users/palas/work/cardano-api/rpc.socket"))
          , nodeSocketPath = Last (Just (File "/Users/palas/testnet/state-node-preview/node.socket"))
          }
  putStrLn "Starting RPC server..."
  runRpcServer nullTracer (return (rpcConfig, NetworkMagic 2))
  return ()

throwToIO :: Either String a -> IO a
throwToIO =
  either
    ( \reason ->
        ( ioError $
            IOError
              { ioe_handle = Nothing
              , ioe_type = OtherError
              , ioe_location = "Calculating config"
              , ioe_description = reason
              , ioe_errno = Nothing
              , ioe_filename = Nothing
              }
        )
    )
    return
