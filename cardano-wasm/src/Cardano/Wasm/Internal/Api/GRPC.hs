module Cardano.Wasm.Internal.Api.GRPC where

import Cardano.Wasm.Internal.Api.Tx qualified as Wasm
import Cardano.Wasm.Internal.ExceptionHandling (rightOrError, toMonadFail)

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BS

-- | Internal data for the GrpcConnection virtual object. Currently, it is just a wrapper around the grpcClient,
-- which is some object that allows us to interact with the Cardano Node via gRPC or GRPC-web.
newtype GrpcObject grpcClient
  = GrpcObject grpcClient

-- | Create a new unsigned transaction object for making a Conway era transaction.
newGrpcConnectionImpl :: (String -> IO grpcClient) -> String -> IO (GrpcObject grpcClient)
newGrpcConnectionImpl createClientFunc host = GrpcObject <$> createClientFunc host

-- | Get the era from the Cardano Node using gRPC or GRPC-web.
getEraImpl :: (grpcClient -> IO Int) -> GrpcObject grpcClient -> IO Int
getEraImpl getEraFunc (GrpcObject client) = getEraFunc client

-- | Get the protocol parameters from the Cardano Node using gRPC or GRPC-web.
getProtocolParamsImpl
  :: (grpcClient -> IO Wasm.ProtocolParamsJSON)
  -> GrpcObject grpcClient
  -> IO Wasm.ProtocolParamsJSON
getProtocolParamsImpl getProtocolParamsFunc (GrpcObject client) = getProtocolParamsFunc client

-- | Get all UTXOs from the node using a gRPC or GRPC-web client.
getAllUtxosImpl
  :: (grpcClient -> IO utxos)
  -> GrpcObject grpcClient
  -> IO utxos
getAllUtxosImpl getUtxosFunc (GrpcObject client) = getUtxosFunc client

-- | Get UTXOs for a given address using a gRPC or GRPC-web client.
getUtxosForAddressImpl
  :: (grpcClient -> String -> IO utxos)
  -> GrpcObject grpcClient
  -> String
  -> IO utxos
getUtxosForAddressImpl getUtxosForAddressFunc (GrpcObject client) = getUtxosForAddressFunc client

-- | Submit a transaction to the Cardano Node using gRPC or GRPC-web.
submitTxImpl
  :: (grpcClient -> String -> IO (Either String String))
  -> GrpcObject grpcClient
  -> String
  -> IO String
submitTxImpl submitTxFunc (GrpcObject client) tx =
  toMonadFail . rightOrError . (base64ToBase16 =<<) =<< submitTxFunc client tx
 where
  -- We reencode as Base16 because it is a more common format for txIds
  base64ToBase16 :: String -> Either String String
  base64ToBase16 encoded = do
    decoded <- Base64.decode $ BS.pack encoded
    return $ BS.unpack $ Base16.encode decoded
