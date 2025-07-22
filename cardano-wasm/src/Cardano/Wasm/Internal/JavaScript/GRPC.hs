{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module Cardano.Wasm.Internal.JavaScript.GRPC where
#else

module Cardano.Wasm.Internal.JavaScript.GRPC (js_newWebGrpcClient, js_getEra) where

import GHC.Wasm.Prim

-- | Create a GRPC-web client for the Cardano API.
foreign import javascript safe "new grpc.NodePromiseClient($1, null, null)"
  js_newWebGrpcClientImpl :: JSString -> IO JSVal

js_newWebGrpcClient :: String -> IO JSVal
js_newWebGrpcClient = js_newWebGrpcClientImpl . toJSString

-- | Get the era from the Cardano API using a GRPC-web client.
foreign import javascript safe "($1).getEra(new proto.Empty(), {})"
  js_getEra :: JSVal -> IO Int

#endif
