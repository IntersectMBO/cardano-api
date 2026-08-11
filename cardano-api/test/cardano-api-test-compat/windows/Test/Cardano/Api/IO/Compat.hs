module Test.Cardano.Api.IO.Compat
  ( makeGroupOtherReadable
  )
where

-- | Windows has no group\/other permission bits, and the overwrite test
-- skips its permission precondition at runtime there, so this is never
-- called.
makeGroupOtherReadable :: FilePath -> IO ()
makeGroupOtherReadable _ = pure ()
