module Test.Cardano.Api.IO.Compat
  ( makeGroupOtherReadable
  , readPipeWrittenThrough
  )
where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.IO (hClose)
import System.Posix.Files (setFileMode)
import System.Posix.IO (createPipe, fdToHandle)

-- | Make the file group\/other readable (@0644@), establishing the loose
-- permissions the overwrite test starts from.
makeGroupOtherReadable :: FilePath -> IO ()
makeGroupOtherReadable file = setFileMode file 0o644

-- | Open a pipe, run the action on the @\/dev\/fd\/N@ path of its write end
-- (a symbolic link to the pipe, like @\/dev\/stdout@ is when the output is
-- piped), and return the action's result with what went through the pipe.
readPipeWrittenThrough :: (FilePath -> IO a) -> IO (a, ByteString)
readPipeWrittenThrough action =
  bracket openPipe closePipe $ \(readHandle, writeHandle, writePath) -> do
    result <- action writePath
    hClose writeHandle
    contents <- BS.hGetContents readHandle
    pure (result, contents)
 where
  openPipe = do
    (readEnd, writeEnd) <- createPipe
    readHandle <- fdToHandle readEnd
    writeHandle <- fdToHandle writeEnd
    pure (readHandle, writeHandle, "/dev/fd/" <> show writeEnd)
  -- hClose is a no-op on a handle that is already closed
  closePipe (readHandle, writeHandle, _) = hClose writeHandle >> hClose readHandle
