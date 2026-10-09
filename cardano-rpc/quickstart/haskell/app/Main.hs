{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Rpc.Client (defMessage, (^.))
import qualified Cardano.Rpc.Client as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as Query
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Sync as Sync

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Default.Class (def)
import Data.Text (Text)
import qualified Data.Text as T (show)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.IO as T (putStrLn)

-- | Address of a locally running node's gRPC endpoint, started with
-- @--enable-grpc-http@ (cardano-testnet), as set up in the quickstart README.
rpcAddress :: Rpc.Address
rpcAddress =
  Rpc.Address
    { Rpc.addressHost = "127.0.0.1"
    , Rpc.addressPort = 50051
    , Rpc.addressAuthority = Nothing
    }

main :: IO ()
main = Rpc.withConnection def (Rpc.ServerInsecure rpcAddress) $ \conn -> do
  tipResponse <-
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Sync.SyncService "readTip")) defMessage
  let tip = tipResponse ^. Sync.tip
  T.putStrLn $
    "Tip: slot "
      <> T.show (tip ^. Sync.slot)
      <> " height "
      <> T.show (tip ^. Sync.height)
      <> " hash "
      <> hexEncode (tip ^. Sync.hash)

  paramsResponse <-
    Rpc.nonStreaming conn (Rpc.rpc @(Rpc.Protobuf Query.QueryService "readParams")) defMessage
  let params = paramsResponse ^. Query.values . Query.cardano
  T.putStrLn $
    "Protocol parameters: max_tx_size "
      <> T.show (params ^. Query.maxTxSize)
      <> " max_block_body_size "
      <> T.show (params ^. Query.maxBlockBodySize)

hexEncode :: BS.ByteString -> Text
hexEncode = T.decodeUtf8 . Base16.encode
