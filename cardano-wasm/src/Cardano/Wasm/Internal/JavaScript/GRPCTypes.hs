{-# LANGUAGE CPP #-}

module Cardano.Wasm.Internal.JavaScript.GRPCTypes (JSGRPCClient) where

#if defined(wasm32_HOST_ARCH)

import GHC.Wasm.Prim

-- | In the WebAssembly target, we use JSVal to represent the gRPC client.
type JSGRPCClient = JSVal

#else

-- | In the non-WebAssembly target, we use an opaque type to represent the gRPC client.
data JSGRPCClient

#endif
