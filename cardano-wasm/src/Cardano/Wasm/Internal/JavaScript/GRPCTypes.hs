{-# LANGUAGE CPP #-}

module Cardano.Wasm.Internal.JavaScript.GRPCTypes (JSGRPCClient, JSUtxos, JSUtxoFilter) where

#if defined(wasm32_HOST_ARCH)

import GHC.Wasm.Prim

-- | In the WebAssembly target, we use JSVal to represent the gRPC client.
type JSGRPCClient = JSVal

-- | Utxos represented as an arbitrary javascript object
type JSUtxos = JSVal

-- | Utxos filter represented as an arbitrary javascript object
type JSUtxoFilter = JSVal

#else

-- | In the non-WebAssembly target, we use an opaque type to represent the gRPC client.
data JSGRPCClient

-- | Uninhabited type for non-WASM targets.
data JSUtxos

-- | Uninhabited type for non-WASM targets.
data JSUtxoFilter

#endif
