module Test.Cardano.Api.IO.Compat
  ( makeGroupOtherReadable
  , readPipeWrittenThrough
  )
where

import Data.ByteString (ByteString)

-- | Windows has no group\/other permission bits, and the overwrite test
-- skips its permission precondition at runtime there, so this is never
-- called.
makeGroupOtherReadable :: FilePath -> IO ()
makeGroupOtherReadable _ = pure ()

-- | Windows has no @\/dev\/fd@ paths, and the pipe test is skipped at runtime
-- there, so this is never called.
readPipeWrittenThrough :: (FilePath -> IO a) -> IO (a, ByteString)
readPipeWrittenThrough _ = error "readPipeWrittenThrough: not available on Windows"
