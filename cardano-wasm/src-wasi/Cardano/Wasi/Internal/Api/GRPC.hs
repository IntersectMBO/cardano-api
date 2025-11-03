{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.GRPC (newGrpcConnection, getEra, closeGrpcConnection) where

import Cardano.Wasm.Api.GRPC (GrpcObject (..), getEraImpl, newGrpcConnectionImpl)

import GHC.Foreign (CString)

import Foreign (Ptr)
import Foreign.C (peekCString, withCString)

-- * GRPCObject

foreign import ccall "newGrpcConnectionCall"
  newGrpcConnectionCall :: CString -> IO GrpcClientPtr

foreign import ccall "getEraCall"
  getEraCall :: GrpcClientPtr -> IO Int

foreign import ccall "closeGrpcConnectionCall"
  closeGrpcConnection :: GrpcObject GrpcClientPtr -> IO ()

type GrpcClientPtr = Ptr ()

newGrpcConnectionCallWrapper :: String -> IO GrpcClientPtr
newGrpcConnectionCallWrapper host = do
  withCString host $ \cstr -> do
    newGrpcConnectionCall cstr

#if defined(wasm32_HOST_ARCH)

foreign export ccall "newGrpcConnection"
  newGrpcConnection :: CString -> IO (GrpcObject GrpcClientPtr)

foreign export ccall "getEra"
  getEra :: GrpcObject GrpcClientPtr -> IO Int

foreign export ccall "closeGrpcConnection"
  closeGrpcConnection :: GrpcObject GrpcClientPtr -> IO ()

#endif

newGrpcConnection :: CString -> IO (GrpcObject GrpcClientPtr)
newGrpcConnection host = newGrpcConnectionImpl newGrpcConnectionCallWrapper =<< peekCString host

getEra :: GrpcObject GrpcClientPtr -> IO Int
getEra = getEraImpl getEraCall
