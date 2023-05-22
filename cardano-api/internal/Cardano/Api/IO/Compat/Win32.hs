{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Api.IO.Compat.Win32
  (
#ifndef UNIX
    handleFileForWritingWithOwnerPermissionImpl,
    writeSecretsImpl,
#endif
  ) where

#ifndef UNIX

import           Cardano.Api.Error (FileError (..))

import           Control.Exception (bracketOnError)
import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Directory as IO
import           System.Directory (emptyPermissions, readable, setPermissions)
import           System.FilePath (splitFileName, (<.>), (</>))
import qualified System.IO as IO
import           System.IO (Handle)
import           Text.Printf (printf)

handleFileForWritingWithOwnerPermissionImpl
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermissionImpl path f = do
  -- On something other than unix, we make a _new_ file, and since we created it,
  -- we must own it. We then place it at the target location. Unfortunately this
  -- won't work correctly with pseudo-files.
  bracketOnError
    (IO.openTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, h) -> do
      IO.hClose h >> IO.removeFile tmpPath
      return . Left $ FileErrorTempFile path tmpPath h)
    (\(tmpPath, h) -> do
        f h
        IO.hClose h
        IO.renameFile tmpPath path
        return $ Right ())
  where
    (targetDir, targetFile) = splitFileName path

writeSecretsImpl :: FilePath -> [Char] -> [Char] -> (a -> ByteString) -> [a] -> IO ()
writeSecretsImpl outDir prefix suffix secretOp xs =
  forM_ (zip xs [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    BS.writeFile filename $ secretOp secret
    setPermissions filename (emptyPermissions {readable = True})

#endif
