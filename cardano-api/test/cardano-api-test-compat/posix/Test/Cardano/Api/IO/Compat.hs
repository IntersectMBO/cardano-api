module Test.Cardano.Api.IO.Compat
  ( makeGroupOtherReadable
  , readPipeWrittenThrough
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.Posix.Files (setFileMode)
import System.Posix.IO (closeFd, createPipe, fdToHandle)

-- | Make the file group\/other readable (@0644@), establishing the loose
-- permissions the overwrite test starts from.
makeGroupOtherReadable :: FilePath -> IO ()
makeGroupOtherReadable file = setFileMode file 0o644

-- | Open a pipe, run the action on the @\/dev\/fd\/N@ path of its write end
-- (a symbolic link to the pipe, like @\/dev\/stdout@ is when the output is
-- piped), and return the action's result with what went through the pipe.
readPipeWrittenThrough :: (FilePath -> IO a) -> IO (a, ByteString)
readPipeWrittenThrough action = do
  (readEnd, writeEnd) <- createPipe
  result <- action ("/dev/fd/" <> show writeEnd)
  closeFd writeEnd
  contents <- BS.hGetContents =<< fdToHandle readEnd
  pure (result, contents)
