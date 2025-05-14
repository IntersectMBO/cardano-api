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

------------------------
-- The refactor strategy
------------------------

-- renames without Cardano.Api prefix: [(new name, old name)]
renames :: [(ModuleName, ModuleName)]
renames = map (bimap prependModulePrefix prependModulePrefix)
  [ ("Byron.Internal.Key", "Internal.Keys.Byron")

  , ("Address", "Internal.Address")

  , ("Block", "Internal.Block")

  -- TODO reexport everything through Cardano.Api.Tx
  , ("Tx.Internal.Body", "Internal.Tx.Body")
  , ("Tx.Internal.Convenience", "Internal.Convenience.Construction")
  , ("Tx.Internal.Fees", "Internal.Fees")
  , ("Tx.Internal.Sign", "Internal.Tx.Sign")

  , ("Query", "Internal.Query")
  , ("Query.Internal.Convenience", "Internal.Convenience.Query") -- TODO reexport through Query module

  -- TODO reexport everything through Era
  , ("Era", "Internal.Eras")
  , ("Era.Internal.Case", "Internal.Eras.Case")
  , ("Era.Internal.Core", "Internal.Eras.Core")
  , ("Era.Internal.Eon.AllegraEraOnwards", "Internal.Eon.AllegraEraOnwards")
  , ("Era.Internal.Eon.AlonzoEraOnwards", "Internal.Eon.AlonzoEraOnwards")
  , ("Era.Internal.Eon.BabbageEraOnwards", "Internal.Eon.BabbageEraOnwards")
  , ("Era.Internal.Eon.ByronToAlonzoEra", "Internal.Eon.ByronToAlonzoEra")
  , ("Era.Internal.Eon.Convert", "Internal.Eon.Convert")
  , ("Era.Internal.Eon.ConwayEraOnwards", "Internal.Eon.ConwayEraOnwards")
  , ("Era.Internal.Eon.MaryEraOnwards", "Internal.Eon.MaryEraOnwards")
  , ("Era.Internal.Eon.ShelleyBasedEra", "Internal.Eon.ShelleyBasedEra")
  , ("Era.Internal.Eon.ShelleyEraOnly", "Internal.Eon.ShelleyEraOnly")
  , ("Era.Internal.Eon.ShelleyToAllegraEra", "Internal.Eon.ShelleyToAllegraEra")
  , ("Era.Internal.Eon.ShelleyToAlonzoEra", "Internal.Eon.ShelleyToAlonzoEra")
  , ("Era.Internal.Eon.ShelleyToBabbageEra", "Internal.Eon.ShelleyToBabbageEra")
  , ("Era.Internal.Eon.ShelleyToMaryEra", "Internal.Eon.ShelleyToMaryEra")

  , ("Error", "Internal.Error")

  , ("Genesis", "Internal.Genesis")
  , ("Genesis.Internal.Parameters", "Internal.GenesisParameters") -- TODO reexport through Genesis module

  -- TODO export everything through Governance module
  , ("Governance.Internal.Action.ProposalProcedure", "Internal.Governance.Actions.ProposalProcedure")
  , ("Governance.Internal.Action.VotingProcedure", "Internal.Governance.Actions.VotingProcedure")
  , ("Governance.Internal.Anchor", "Internal.Anchor")
  , ("Governance.Internal.Metadata.DrepRegistration", "Internal.Governance.Metadata.DrepRegistration")
  , ("Governance.Internal.Metadata.GovAction", "Internal.Governance.Metadata.GovAction")
  , ("Governance.Internal.Metadata.Parsers", "Internal.Governance.Metadata.Parsers")
  , ("Governance.Internal.Metadata.Validation", "Internal.Governance.Metadata.Validation")
  , ("Governance.Internal.Poll", "Internal.Governance.Poll")

  -- TODO reexport everything through IO
  , ("IO", "Internal.IO")
  , ("IO.Internal.Base", "Internal.IO.Base")
  , ("IO.Internal.Compat", "Internal.IO.Compat")
  , ("IO.Internal.Posix", "Internal.IO.Posix")
  , ("IO.Internal.Win32", "Internal.IO.Win32")

  , ("IPC", "Internal.IPC")
  , ("IPC.Internal.Monad", "Internal.IPC.Monad") -- TODO reexport through IPC module
  , ("IPC.Internal.Version", "Internal.IPC.Version") -- TODO reexport through IPC module

  , ("LedgerState", "Internal.LedgerState")

  , ("Consensus.Internal.Mode", "Internal.Modes") -- TODO reexport through Consensus module
  , ("Consensus.Internal.InMode", "Internal.InMode") -- TODO reexport through Consensus module -- should we get rid of this?

  , ("Pretty", "Internal.Pretty")

  , ("Plutus", "Internal.Plutus")
  , ("Plutus.Script", "Internal.Script")
  , ("Plutus.ScriptData", "Internal.ScriptData")

  , ("ProtocolParameters", "Internal.ProtocolParameters")

  , ("Serialise.Cbor.Canonical", "Internal.Serialise.Cbor.Canonical")
  , ("Serialise.TextEnvelope", "Internal.SerialiseTextEnvelope")
  , ("Serialise.Cip129", "Internal.CIP.Cip129"
  , ("Serialise.DeserialiseAnyOf", "Internal.DeserialiseAnyOf")
  , ("Serialise.Internal.Json", "Internal.Json")
  -- TODO this module needs to be removed: the conversion functions need to go into their respective places
  -- TODO remove references to CDDL
  , ("Serialise.TextEnvelope.Internal", "Internal.SerialiseLedgerCddl")

  , ("Ledger.Internal", "Internal.ReexposeLedger")

  , ("Network.Internal", "Internal.ReexposeNetwork")

  , ("Certificate", "Internal.Certificate")

  , ("Compatible.Tx", "Internal.Compatible.Tx")

  , ("Experimental.Era", "Internal.Experimental.Eras")
  , ("Experimental.Plutus.IndexedPlutusScriptWitness", "Internal.Experimental.Plutus.IndexedPlutusScriptWitness")
  , ("Experimental.Plutus.Script", "Internal.Experimental.Plutus.Script")
  , ("Experimental.Plutus.ScriptWitness", "Internal.Experimental.Plutus.ScriptWitness")
  , ("Experimental.Plutus.Shim.LegacyScripts", "Internal.Experimental.Plutus.Shim.LegacyScripts")
  , ("Experimental.Simple.Script", "Internal.Experimental.Simple.Script")
  , ("Experimental.Tx", "Internal.Experimental.Tx")
  , ("Experimental.Witness.AnyWitness", "Internal.Experimental.Witness.AnyWitness")
  , ("Experimental.Witness.TxScriptWitnessRequirements", "Internal.Experimental.Witness.TxScriptWitnessRequirements")

  , ("Feature", "Internal.Feature")

  , ("HasTypeProxy", "Internal.HasTypeProxy")

  , ("Hash", "Hash")
  ]

  -- TODO remove Cardano.Api.Shelley, and make sure that everything is exported from `Cardano.Api`
  -- TODO move Cardano.Api.Internal.Tx.UTxO to Cardano.Api.Tx.UTxO - we don't need extra layer of redirection
    where
      prependModulePrefix t = modulePrefix <> "." <> t
      modulePrefix = "Cardano.Api"

-- TODO commits order:
-- 1. Update refactor.hs
-- 2. Adjust single function exports from modules, delete modules
-- 3. Move some modules not being exported to other-modules in cardano-api.cabal
--    * caveat, some modules will still have to be exported, for e.g. for tests purposes
-- 4. Run refactor.hs renaming modules


-------------------------------------
-- The internals of the refactor code
-------------------------------------

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
        updateFile renames file
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
