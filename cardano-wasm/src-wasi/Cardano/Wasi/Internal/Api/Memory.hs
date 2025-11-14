{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Api.Memory
  ( mallocNBytes
  , getStrLen
  , freeMemory
  )
where

import Foreign.C (CChar (..), CString)
import Foreign.Marshal (free, lengthArray0, mallocBytes)
import Foreign.Ptr (Ptr)

-- * Memory

#if defined(wasm32_HOST_ARCH)

foreign export ccall "mallocNBytes"
  mallocBytes :: Int -> IO (Ptr a)

foreign export ccall "getStrLen"
  getStrLen :: CString -> IO Int

foreign export ccall "freeMemory"
  freeMemory :: Ptr a -> IO ()

#endif

mallocNBytes :: Int -> IO (Ptr a)
mallocNBytes = mallocBytes

getStrLen :: CString -> IO Int
getStrLen = lengthArray0 (CChar 0)

freeMemory :: Ptr a -> IO ()
freeMemory = free
