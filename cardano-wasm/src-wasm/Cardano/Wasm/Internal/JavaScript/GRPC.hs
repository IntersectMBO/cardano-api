{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module Cardano.Wasm.Internal.JavaScript.GRPC where
#else

module Cardano.Wasm.Internal.JavaScript.GRPC (js_newWebGrpcClient, js_unary) where

import GHC.Wasm.Prim
import Cardano.Wasm.Api.GRPC (Transport, base64Transport)

-- | Create a GRPC-web client for the Cardano API.
foreign import javascript safe "globalThis.createClient($1)"
  js_newWebGrpcClientImpl :: JSString -> IO JSVal

js_newWebGrpcClient :: String -> IO JSVal
js_newWebGrpcClient = js_newWebGrpcClientImpl . toJSString

-- | Send a unary gRPC request with a client made by @createClient@. It takes the
-- path of the method and the request, and returns the response, both in base64.
foreign import javascript safe "($1).unary($2, $3)"
  js_unaryImpl :: JSVal -> JSString -> JSString -> IO JSString

-- | The 'Transport' of a client made by @createClient@.
js_unary :: JSVal -> Transport
js_unary client = base64Transport $ \path request ->
  fromJSString <$> js_unaryImpl client (toJSString path) (toJSString request)

#endif
