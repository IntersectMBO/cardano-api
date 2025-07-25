{-# LANGUAGE CPP #-}

module Cardano.Wasm.Internal.JavaScript.Random (getRandomBytes) where

import Data.ByteString (ByteString)

#if !defined(wasm32_HOST_ARCH)

getRandomBytes :: Word -> IO ByteString
getRandomBytes _ = error "getRandomBytes is not implemented for non-WASM targets"

#else

import Cardano.Wasm.Internal.ExceptionHandling (rightOrError)

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import GHC.Wasm.Prim

-- | Create a GRPC-web client for the Cardano API.
foreign import javascript safe "{ let numBytes = $1; \
                                  let randomBytes = new Uint8ClampedArray(numBytes); \
                                  crypto.getRandomValues(randomBytes); \
                                  return (randomBytes.reduce((acc, next) => acc.concat(next.toString(16).padStart(2, '0')), '')); \
                                }"
  js_generateRandomBytes :: Int -> IO JSString

getRandomBytes :: Word -> IO ByteString
getRandomBytes n = do
  hexRandomBytes <- fromJSString <$> js_generateRandomBytes (fromIntegral n)
  rightOrError $ Base16.decode $ BSC.pack hexRandomBytes

#endif
