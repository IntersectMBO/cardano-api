module Test.Cardano.Api.IO.Compat
  ( makeGroupOtherReadable
  )
where

import System.Posix.Files (setFileMode)

-- | Make the file group\/other readable (@0644@), establishing the loose
-- permissions the overwrite test starts from.
makeGroupOtherReadable :: FilePath -> IO ()
makeGroupOtherReadable file = setFileMode file 0o644
