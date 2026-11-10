{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- The 'NonStreaming' constraint of 'rpc' only rejects streaming methods, so GHC sees it as redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Wasm.Api.GRPC where

import Cardano.Wasm.Api.Tx qualified as Wasm
import Cardano.Wasm.ExceptionHandling (rightOrError, throwError, toMonadFail)

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.ProtoLens (decodeMessage, encodeMessage)
import Data.ProtoLens.Service.Types
  ( HasMethod
  , HasMethodImpl (..)
  , Service (..)
  , StreamingType (..)
  )
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (symbolVal)

-- | Internal data for the GrpcConnection virtual object. Currently, it is just a wrapper around the grpcClient,
-- which is some object that allows us to interact with the Cardano Node via gRPC or GRPC-web.
newtype GrpcObject grpcClient
  = GrpcObject grpcClient

-- | Create a new gRPC or GRPC-web connection to the Cardano Node.
newGrpcConnectionImpl :: (String -> IO grpcClient) -> String -> IO (GrpcObject grpcClient)
newGrpcConnectionImpl createClientFunc host = GrpcObject <$> createClientFunc host

-- | Send one unary gRPC request: it takes the path of the method (like
-- @\/cardano.rpc.Node\/GetEra@) and the encoded request, and returns the encoded response.
type Transport = String -> ByteString -> IO ByteString

-- | Call the unary gRPC method @m@ of the service @s@ using a 'Transport'.
-- The method fixes the types of the request and the response, so a request
-- that does not belong to the method does not compile.
rpc
  :: forall s m
   . (HasCallStack, HasMethod s m, MethodStreamingType s m ~ 'NonStreaming)
  => Transport
  -> MethodInput s m
  -> IO (MethodOutput s m)
rpc transport request = do
  response <- transport path (encodeMessage request)
  either (\err -> throwError $ "Failed to decode the response of " <> path <> ": " <> err) pure $
    decodeMessage response
 where
  path =
    "/"
      <> symbolVal (Proxy @(ServicePackage s))
      <> "."
      <> symbolVal (Proxy @(ServiceName s))
      <> "/"
      <> symbolVal (Proxy @(MethodName s m))

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
