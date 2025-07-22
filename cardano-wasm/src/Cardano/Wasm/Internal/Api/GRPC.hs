module Cardano.Wasm.Internal.Api.GRPC where

import Cardano.Wasm.Internal.JavaScript.GRPCTypes (JSGRPCClient)

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
