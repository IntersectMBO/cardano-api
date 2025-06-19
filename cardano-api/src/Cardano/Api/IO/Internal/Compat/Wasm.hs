{-# LANGUAGE CPP #-}

#if defined(wasm32_HOST_ARCH)
#define WASM
#endif

module Cardano.Api.IO.Internal.Compat.Wasm
  (
#ifdef WASM
    checkVrfFilePermissionsImpl
  , handleFileForWritingWithOwnerPermissionImpl
  , writeSecretsImpl
#endif
  )
where

#ifdef WASM

import           Cardano.Api.Error (FileError (..))
import           Cardano.Api.IO.Internal.Base
import           Control.Monad.Except (ExceptT)
import           Data.ByteString (ByteString)
import           System.IO (Handle)

handleFileForWritingWithOwnerPermissionImpl
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermissionImpl _path _f = return $ Right () -- Dummy implementation for WASM

writeSecretsImpl :: FilePath -> [Char] -> [Char] -> (a -> ByteString) -> [a] -> IO ()
writeSecretsImpl _outDir _prefix _suffix _secretOp _xs = return () -- Dummy implementation for WASM

-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVrfFilePermissionsImpl
  :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissionsImpl _vrfPrivKeyFile = return () -- Dummy implementation for WASM

#endif
