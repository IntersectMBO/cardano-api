module Test.Golden.Cardano.Wasm.TypeScript where

import Control.Monad (forM_)
import Control.Monad.IO.Class qualified as H
import System.FilePath ((</>))

import Hedgehog as H
import Hedgehog.Extras (workspace)
import Hedgehog.Extras qualified as H

hprop_cardano_wasm_typescript_declarations_match_generated :: Property
hprop_cardano_wasm_typescript_declarations_match_generated =
  H.propertyOnce $ do
    workspace "cardano-wasm-ts-decls" $ \workDir -> do
      dir <- H.createDirectoryIfMissing (workDir </> "lib-wrapper")
      _ <- H.execFlex "cardano-wasm" "CARDANO_WASM" ["--output-dir", dir]
      forFilesInDir dir $ \file -> do
        H.annotate (dir </> file)
        H.diffFileVsGoldenFile (dir </> file) ("lib-wrapper" </> file)

forFilesInDir :: (H.MonadTest m, H.MonadIO m) => FilePath -> (FilePath -> m ()) -> m ()
forFilesInDir dir action = do
  files <- H.listDirectory dir
  forM_ files $ \file -> do
    let fullPath = dir </> file
    H.assertFileExists fullPath
    action file
