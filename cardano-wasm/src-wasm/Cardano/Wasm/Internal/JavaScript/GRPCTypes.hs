{-# LANGUAGE CPP #-}

module Cardano.Wasm.Internal.JavaScript.GRPCTypes (JSUtxos) where

#if defined(wasm32_HOST_ARCH)

import GHC.Wasm.Prim

-- | Utxos represented as an arbitrary javascript object
type JSUtxos = JSVal

#else

-- | Uninhabited type for non-WASM targets.
data JSUtxos

#endif
