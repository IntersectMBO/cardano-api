{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

module Cardano.Wasm.Internal.JavaScript.Random (getRandomBytes) where

#if !defined(wasm32_HOST_ARCH)

import Data.ByteString (ByteString)

getRandomBytes :: Word -> IO ByteString
getRandomBytes _ = error "getRandomBytes is not implemented for non-WASM targets"

#else

import Control.Exception (throwIO)
import Data.ByteString (ByteString, packCStringLen)
import Data.Word (Word8)
import System.IO.Error (mkIOError, userErrorType)

import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)

-- | Import the 'random_get' function from wasi
-- module provided by the WASI runtime.
--
-- It takes a raw pointer to a u8 buffer (Ptr Word8) and the buffer's
-- length (CSize) and returns a CInt representing the error code number.
-- A return value of 0 means success.
foreign import ccall "__wasi_random_get"
  wasi_random_get
    :: Ptr Word8
    -- ^ Pointer to the buffer to fill
    -> CSize
    -- ^ Number of bytes to write
    -> IO CInt
    -- ^ Returns 0 on success, or an errno code

-- | A safe Haskell wrapper around 'random_get' function from wasi.
-- It requests the given number of cryptographically secure random
-- bytes and returns them as a ByteString.
getRandomBytes :: Word -> IO ByteString
getRandomBytes n =
  allocaBytes (fromIntegral n) $ \ptr -> do
    -- Get pointer to allocated buffer
    errno <- wasi_random_get ptr (fromIntegral n)
    if errno == 0
      -- If successful, pack the bytes from the pointer into a Haskell managed ByteString
      then packCStringLen (castPtr ptr, fromIntegral n)
      else -- Otherwise, we throw
        throwIO $
          mkIOError
            userErrorType
            ("wasi_random_get failed with errno: " ++ show errno)
            Nothing
            Nothing

#endif
