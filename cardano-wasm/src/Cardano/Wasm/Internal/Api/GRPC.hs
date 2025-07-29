module Cardano.Wasm.Internal.Api.GRPC where

import Cardano.Wasm.Internal.Api.Tx qualified as Wasm
import Cardano.Wasm.Internal.ExceptionHandling (rightOrError, toMonadFail)
import Cardano.Wasm.Internal.JavaScript.GRPCTypes (JSGRPCClient)

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BS

-- | Internal data for the GrpcConnection virtual object. Currently, it is just a wrapper around the JSGRPCClient,
-- which is a JavaScript object that allows us to interact with the Cardano Node via gRPC-web.
newtype GrpcObject
  = GrpcObject JSGRPCClient

-- | Create a new unsigned transaction object for making a Conway era transaction.
newGrpcConnectionImpl :: (String -> IO JSGRPCClient) -> String -> IO GrpcObject
newGrpcConnectionImpl createClientJsFunc host = GrpcObject <$> createClientJsFunc host

-- | Get the era from the Cardano Node using GRPC-web.
getEraImpl :: (JSGRPCClient -> IO Int) -> GrpcObject -> IO Int
getEraImpl getEraJsFunc (GrpcObject client) = getEraJsFunc client

-- | Get the protocol parameters from the Cardano Node using GRPC-web.
getProtocolParamsImpl
  :: (JSGRPCClient -> IO Wasm.ProtocolParamsJSON) -> GrpcObject -> IO Wasm.ProtocolParamsJSON
getProtocolParamsImpl getProtocolParamsJsFunc (GrpcObject client) = getProtocolParamsJsFunc client

-- | Submit a transaction to the Cardano Node using GRPC-web.
submitTxImpl
  :: (JSGRPCClient -> String -> IO (Either String String))
  -> GrpcObject
  -> String
  -> IO String
submitTxImpl submitTxJsFunc (GrpcObject client) tx =
  toMonadFail . rightOrError . (base64ToBase16 =<<) =<< submitTxJsFunc client tx
 where
  -- We reencode as Base16 because it is a more common format for txIds
  base64ToBase16 :: String -> Either String String
  base64ToBase16 encoded = do
    decoded <- Base64.decode $ BS.pack encoded
    return $ BS.unpack $ Base16.encode decoded
