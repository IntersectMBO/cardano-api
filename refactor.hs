#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , rio
             , text
             , bytestring
             , transformers
default-language: GHC2021
default-extensions:
  NoImplicitPrelude
  OverloadedStrings
-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Stack
import System.IO
import RIO
import RIO.FilePath
import RIO.Directory
import Data.List(isSuffixOf)
import Control.Monad.Trans.Maybe (runMaybeT)

type ModuleName = Text

-- renames without prefix: [(new name, old name)]
renames :: [(ModuleName, ModuleName)]
renames = map (bimap prependModulePrefix prependModulePrefix)
  [ ("Byron.Internal.Key", "Internal.Keys.Byron")
  ,("Address", "Internal.Address")
  , ("Block", "Internal.Block")

  -- TODO reexport through Cardano.Api.Tx
  , ("Tx.Internal.Convenience", "Internal.Convenience.Construction")
  , ("Tx.Internal.Fees", "Internal.Fees")
  -- TODO reexport through Cardano.Api.Query
  ,("Query.Internal.Convenience", "Internal.Convenience.Query")

  -- TODO reexport everything through Cardano.Api.Era
  , ("Era", "Internal.Eras")
  , ("Era.Internal.Case", "Internal.Eras.Case")
  , ("Era.Internal.Core", "Internal.Eras.Core")
  , ("Era.Internal.Eon.AllegraEraOnwards" ,"Internal.Eon.AllegraEraOnwards")
  , ("Era.Internal.Eon.AlonzoEraOnwards" ,"Internal.Eon.AlonzoEraOnwards")
  , ("Era.Internal.Eon.BabbageEraOnwards" ,"Internal.Eon.BabbageEraOnwards")
  , ("Era.Internal.Eon.ByronToAlonzoEra" ,"Internal.Eon.ByronToAlonzoEra")
  , ("Era.Internal.Eon.Convert" ,"Internal.Eon.Convert")
  , ("Era.Internal.Eon.ConwayEraOnwards" ,"Internal.Eon.ConwayEraOnwards")
  , ("Era.Internal.Eon.MaryEraOnwards" ,"Internal.Eon.MaryEraOnwards")
  , ("Era.Internal.Eon.ShelleyBasedEra" ,"Internal.Eon.ShelleyBasedEra")
  , ("Era.Internal.Eon.ShelleyEraOnly" ,"Internal.Eon.ShelleyEraOnly")
  , ("Era.Internal.Eon.ShelleyToAllegraEra" ,"Internal.Eon.ShelleyToAllegraEra")
  , ("Era.Internal.Eon.ShelleyToAlonzoEra" ,"Internal.Eon.ShelleyToAlonzoEra")
  , ("Era.Internal.Eon.ShelleyToBabbageEra" ,"Internal.Eon.ShelleyToBabbageEra")
  , ("Era.Internal.Eon.ShelleyToMaryEra" ,"Internal.Eon.ShelleyToMaryEra")

  , ("Error", "Internal.Error")

  , ("Genesis", "Internal.Genesis")
  -- TODO reexport through Genesis module
  , ("Genesis.Internal.Parameters", "Internal.GenesisParameters")

  , ("Governance.Internal.Metadata.Validation", "Internal.Governance.Metadata.Validation")

  , ("IO", "Internal.IO")

  , ("IPC", "Internal.IPC")
  -- TODO reexport through IPC module
  , ("IPC.Internal.Monad", "Internal.IPC.Monad")

  , ("LedgerState", "Internal.LedgerState")

  , ("Consensus", "Internal.LedgerState")
  ]

  -- TODO remove Cardano.Api.Shelley
    where
      prependModulePrefix t = modulePrefix <> "." <> t
      modulePrefix = "Cardano.Api"

-- TODO commits order:
-- 1. Update refactor.hs
-- 2. Adjust single function exports from modules, delete modules
-- 3. Move modules not being exported to other-modules in cardano-api.cabal
--    * caveat, some modules will still have to be exported
-- 4. Run refactor.hs renaming modules

type MonadApp e m = (HasCallStack, MonadIO m, MonadReader e m, HasLogFunc e)

substituteModuleNames :: [(ModuleName, ModuleName)] -> Text -> Text
substituteModuleNames renames' text = foldl' f text renames'
 where
  f :: Text -> (ModuleName, ModuleName) -> Text
  f text' (nameTo, nameFrom) =
    T.replace nameFrom nameTo text'

toFilePath :: ModuleName -> FilePath
toFilePath =
  T.unpack
  . (<> ".hs")
  . ("cardano-api/src/" <>)
  . T.replace "." "/"

main :: IO ()
main = runSimpleApp $ do
  logInfo "Updating cabal file"
  updateFile renames "cardano-api/cardano-api.cabal"

  logInfo "Renaming modules"
  mapM_ renameModule renames

  logInfo "Updating modules' references"
  allFiles <- listDirectoryRecursively "cardano-api"
  forM_ allFiles $ \file -> do
    if ".hs" `isSuffixOf` file
       then do
        let shelley = ("Cardano.Api", "Cardano.Api.Shelley")
        updateFile (shelley:renames) file
       else
        removeEmptyDirectory file

renameModule :: MonadApp e m
             => (ModuleName, ModuleName)  -- ^ (new, old)
             -> m ()
renameModule (newModule, oldModule) = do
  let oldPath = toFilePath oldModule
      newPath = toFilePath newModule
  isFilePresent <- doesFileExist oldPath
  if isFilePresent
    then do
      createDirectoryIfMissing True $ takeDirectory newPath
      renameFile oldPath newPath
    else
      logWarn $ "Could not find file: " <> display oldPath

listDirectoryRecursively :: MonadApp e m => FilePath -> m [FilePath]
listDirectoryRecursively dir = do
  -- logInfo $ "Entering: " <> display dir
  entries <- listDirectory dir
  let paths = (dir </>) <$> entries
  foldM (\acc path -> do
    isDirectory <- doesDirectoryExist path
    if isDirectory
       then (acc <>) <$> listDirectoryRecursively path
       else pure $ path : acc
    ) paths paths

removeEmptyDirectory :: MonadApp e m => FilePath -> m ()
removeEmptyDirectory path = void . runMaybeT $ do
  True <- doesDirectoryExist path
  [] <- listDirectory path
  removeDirectory path
  logInfo $ "Removed empty directory" <> display path

filterHaskellFiles :: [FilePath] -> [FilePath]
filterHaskellFiles = filter (isSuffixOf ".hs")

updateFile :: MonadApp e m
           => [(ModuleName, ModuleName)]
           -> FilePath
           -> m ()
updateFile renames' filePath = do
  contents <- liftIO $ T.decodeUtf8 <$> B.readFile filePath
  liftIO $ T.writeFile filePath $ substituteModuleNames renames' contents

instance Display String where
  display = fromString
