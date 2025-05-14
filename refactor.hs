#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , containers
             , mtl
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

import Control.Monad.State.Class (MonadState, modify')
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List (foldl', isSuffixOf, isPrefixOf, intercalate)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GHC.Stack
import GHC.IsList (fromList)
import RIO
import RIO.Directory
import RIO.FilePath
import System.IO

type ModuleName = Text

------------------------
-- The refactor strategy
------------------------

isDryRun :: Bool
isDryRun = True

-- The proposed changes. Renames of modules in this list are without 'Cardano.Api' prefix: [(new module name, old module name)]
-- The TODOs have to be performed manually
renames :: [(ModuleName, ModuleName)]
renames = map (bimap prependModulePrefix prependModulePrefix)
  [
  -- TODO remove Cardano.Api.Shelley, and make sure that everything is exported from `Cardano.Api`

    ("Byron.Internal.Key", "Internal.Keys.Byron")
  , ("Byron.Internal.Proposal", "Internal.SpecialByron")

  -- TODO reexport everything through 'Certificate'
  , ("Certificate.Internal", "Internal.Certificate")
  , ("Certificate.Internal.OperationalCertificate", "Internal.OperationalCertificate")
  , ("Certificate.Internal.StakePoolMetadata", "Internal.StakePoolMetadata")
  , ("Certificate.Internal.DRepMetadata", "Internal.DRepMetadata")

  -- TODO reexport through 'Compatible' module
  , ("Compatible.Tx", "Internal.Compatible.Tx")

 -- TODO reexport everything through Consensus module
  , ("Consensus.Internal", "Internal.ReexposeConsensus")
  , ("Consensus.Internal.Mode", "Internal.Modes")
  , ("Consensus.Internal.InMode", "Internal.InMode") -- TODO reexport through Consensus module -- should we get rid of this?
  , ("Consensus.Internal.Protocol", "Internal.Protocol")

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


  , ("Experimental.Era", "Internal.Experimental.Eras")
  -- TODO reexport everything through 'Experimental.Plutus'
  , ("Experimental.Plutus.Internal.IndexedPlutusScriptWitness", "Internal.Experimental.Plutus.IndexedPlutusScriptWitness")
  , ("Experimental.Plutus.Internal.Script", "Internal.Experimental.Plutus.Script")
  , ("Experimental.Plutus.Internal.ScriptWitness", "Internal.Experimental.Plutus.ScriptWitness")
  , ("Experimental.Plutus.Internal.Shim.LegacyScripts", "Internal.Experimental.Plutus.Shim.LegacyScripts")

  , ("Experimental.Simple.Script", "Internal.Experimental.Simple.Script")
  , ("Experimental.Tx", "Internal.Experimental.Tx")

  -- TODO reexport everything through 'Experimental.Witness'
  , ("Experimental.Witness.AnyWitness", "Internal.Experimental.Witness.AnyWitness")
  , ("Experimental.Witness.TxScriptWitnessRequirements", "Internal.Experimental.Witness.TxScriptWitnessRequirements")

  -- TODO reexport everything through 'Genesis'
  , ("Genesis.Internal", "Internal.Genesis")
  , ("Genesis.Internal.Parameters", "Internal.GenesisParameters") -- TODO reexport through Genesis module

  -- TODO export everything through 'Governance' module
  , ("Governance.Internal.Action.ProposalProcedure", "Internal.Governance.Actions.ProposalProcedure")
  , ("Governance.Internal.Action.VotingProcedure", "Internal.Governance.Actions.VotingProcedure")
  , ("Governance.Internal.Anchor", "Internal.Anchor")
  , ("Governance.Internal.Metadata.DrepRegistration", "Internal.Governance.Metadata.DrepRegistration")
  , ("Governance.Internal.Metadata.GovAction", "Internal.Governance.Metadata.GovAction")
  , ("Governance.Internal.Metadata.Parsers", "Internal.Governance.Metadata.Parsers")
  , ("Governance.Internal.Metadata.Validation", "Internal.Governance.Metadata.Validation")
  , ("Governance.Internal.Poll", "Internal.Governance.Poll")

  -- TODO reexport everything through 'IO'
  , ("IO", "Internal.IO")
  , ("IO.Internal.Base", "Internal.IO.Base")
  , ("IO.Internal.Compat", "Internal.IO.Compat")
  , ("IO.Internal.Posix", "Internal.IO.Compat.Posix")
  , ("IO.Internal.Win32", "Internal.IO.Compat.Win32")

  -- TODO reexport everything through 'Key'
  , ("Key.Internal", "Internal.Keys.Shelley")
  , ("Key.Internal.Class", "Internal.Keys.Class")
  , ("Key.Internal.Mnemonic", "Internal.Keys.Mnemonics")
  , ("Key.Internal.Praos", "Internal.Keys.Praos")
  , ("Key.Internal.Read", "Internal.Keys.Read") -- TODO those functions aren't even specialised, module to remove

  -- TODO reexport everything through 'Ledger'
  , ("Ledger.Internal", "Internal.ReexposeLedger")

  -- TODO reexport everything through 'LedgerState'
  , ("LedgerState", "Internal.LedgerState")
  , ("LedgerState.Internal.ConvertLedgerEvent", "Internal.LedgerEvents.ConvertLedgerEvent")
  , ("LedgerState.Internal.LedgerEvent", "Internal.LedgerEvents.LedgerEvent")
  , ("LedgerState.Internal.Rule.BBODY.DELEGS", "Internal.LedgerEvents.Rule.BBODY.DELEGS")
  , ("LedgerState.Internal.Rule.BBODY.LEDGER", "Internal.LedgerEvents.Rule.BBODY.LEDGER")
  , ("LedgerState.Internal.Rule.BBODY.UTXOW", "Internal.LedgerEvents.Rule.BBODY.UTXOW")
  , ("LedgerState.Internal.Rule.TICK.RUPD", "Internal.LedgerEvents.Rule.TICK.RUPD")
  , ("LedgerState.Internal.Rule.TICK.NEWEPOCH", "Internal.LedgerEvents.Rule.TICK.NEWEPOCH")

  -- TODO reexport everything through 'Network'
  , ("Network.Internal", "Internal.ReexposeNetwork")
  , ("Network.Internal.NetworkId", "Internal.NetworkId")

  , ("Network.ChainSync.Client", "ChainSync.Client")
  , ("Network.ChainSync.ClientPipelined", "ChainSync.ClientPipelined")

  -- TODO Reexport everything through 'Network.IPC'
  , ("Network.IPC.Internal", "Internal.IPC") -- TODO: chop down IPC into smaller modules: chainsync
  , ("Network.IPC.Internal.Monad", "Internal.IPC.Monad") -- TODO reexport through IPC module
  , ("Network.IPC.Internal.Version", "Internal.IPC.Version") -- TODO reexport through IPC module

  -- TODO Reexport everything through 'Plutus'
  , ("Plutus", "Internal.Plutus")
  , ("Plutus.Internal.Script", "Internal.Script")
  , ("Plutus.Internal.ScriptData", "Internal.ScriptData")

  , ("Serialise.Bech32", "Internal.SerialiseBech32")
  , ("Serialise.Cip129", "Internal.CIP.Cip129")
  , ("Serialise.Cbor", "Internal.Serialise.Cbor")
  , ("Serialise.Cbor.Canonical", "Internal.Serialise.Cbor.Canonical")
  , ("Serialise.DeserialiseAnyOf", "Internal.DeserialiseAnyOf")
  , ("Serialise.Json", "Internal.SerialiseJSON")
  , ("Serialise.Internal.Json", "Internal.Json") -- TODO move content to Internal.SerialiseJSON and remove the module
  , ("Serialise.Raw", "Internal.SerialiseRaw")
  , ("Serialise.SerialiseUsing", "Internal.SerialiseUsing")
  , ("Serialise.TextEnvelope", "Internal.SerialiseTextEnvelope")
  -- TODO this module needs to be removed: the conversion functions need to go into their respective places
  -- TODO remove references to CDDL
  , ("Serialise.TextEnvelope.Internal", "Internal.SerialiseLedgerCddl")

 -- TODO reexport everything through Query module
  , ("Query.Internal.Convenience", "Internal.Convenience.Query")
  , ("Query.Internal.Expr", "Internal.Query.Expr")
  , ("Query.Internal.Type.DebugLedgerState", "Internal.Query.Types")
  , ("Query.Internal.Type.DelegationsAndRewards", "Internal.Rewards")
  , ("Query.Internal.Type.QueryInMode", "Internal.Query")

  -- TODO reexport everything through Cardano.Api.Tx
  , ("Tx.Internal.Body", "Internal.Tx.Body")
  , ("Tx.Internal.Body.Lens", "Ledger.Lens")
  , ("Tx.Internal.Convenience", "Internal.Convenience.Construction")
  , ("Tx.Internal.Fees", "Internal.Fees")
  , ("Tx.Internal.Sign", "Internal.Tx.Sign")
  , ("Tx.Internal.ScriptData", "Internal.ScriptData")
  , ("Tx.Internal.BuildTxWith", "Internal.Tx.BuildTxWith")
  , ("Tx.Internal.Output", "Internal.Tx.Output")

  , ("Type.Address", "Internal.Address")
  , ("Type.Block", "Internal.Block")
  , ("Type.Feature", "Internal.Feature")
  , ("Type.ProtocolParameters", "Internal.ProtocolParameters")
  , ("Type.TxIn", "Internal.TxIn")
  , ("Type.TxMetadata", "Internal.TxMetadata")
  , ("Type.UTxO", "Internal.Tx.UTxO") -- TODO: remove Cardano.Api.Tx.UTxO
  , ("Type.Value", "Internal.Value")
  , ("Type.Value.Internal.Parser", "Internal.ValueParser") -- TODO reexport through Type.Value

  , ("Error", "Internal.Error")
  , ("HasTypeProxy", "Internal.HasTypeProxy")
  , ("Hash", "Internal.Hash")
  , ("Monad.Error", "Internal.Monad.Error")
  , ("Pretty", "Internal.Pretty")
  , ("Pretty.Internal.ShowOf", "Internal.Via.ShowOf") -- TODO reexport through Pretty

  ]
    where
      prependModulePrefix t = modulePrefix <> "." <> t
      modulePrefix = "Cardano.Api"

-- TODO work order:
-- 1. Update refactor.hs
-- 2. Reorganise functions in modules as required
-- 3. Try to limit what's exposed from cardano-api
--    - might not be feasible because some tests rely on access to some internals
-- 4. Run refactor.hs renaming modules


-------------------------------------
-- The internals of the refactor code
-------------------------------------

type MonadApp e m = (HasCallStack, MonadIO m, MonadReader e m, HasLogFunc e)

main :: IO ()
main = runSimpleApp $ do
  logInfo "💡 Updating cabal file"
  updateFile renames "cardano-api/cardano-api.cabal"

  logInfo "💡 Renaming modules"
  let modulesToRename = snd <$> renames
  mapM_ renameModule renames

  logInfo "💡 Updating modules' references"
  allFiles <- listDirectoryRecursively "cardano-api"
  forM_ allFiles $ \file -> do
    if ".hs" `isSuffixOf` file
       then do
        updateFile renames file
       else
        removeEmptyDirectory file

  logInfo "💡 Skipped modules:"
  logInfo $ display $ listUntouchedModules allFiles modulesToRename

substituteModuleNames :: [(ModuleName, ModuleName)] -> Text -> Text
substituteModuleNames renames' text = foldl' f text renames'
 where
  f :: Text -> (ModuleName, ModuleName) -> Text
  f text' (nameTo, nameFrom)
    | nameTo == nameFrom = text'
    | otherwise = T.replace nameFrom nameTo text'

toFilePath :: ModuleName -> FilePath
toFilePath =
  T.unpack
  . (<> ".hs")
  . ("cardano-api/src/" <>)
  . T.replace "." "/"

-- | Perform filesystem rename
renameModule :: MonadApp e m
             => (ModuleName, ModuleName)  -- ^ (new, old)
             -> m ()
renameModule (newModule, oldModule)
  | newModule == oldModule = pure ()
  | otherwise = do
    let oldPath = toFilePath oldModule
        newPath = toFilePath newModule
    isFilePresent <- doesFileExist oldPath
    if isFilePresent
      then
        if isDryRun
           then
             logInfo $ "〰️ " <> display oldPath <> " -> " <> display newPath
           else do
             createDirectoryIfMissing True $ takeDirectory newPath
             renameFile oldPath newPath
      else
        logWarn $ "‼️ Could not find file: " <> display oldPath

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
  logInfo $ "🗑️ Removed empty directory " <> display path

filterHaskellFiles :: [FilePath] -> [FilePath]
filterHaskellFiles = filter (isSuffixOf ".hs")

updateFile :: MonadApp e m
           => [(ModuleName, ModuleName)]
           -> FilePath
           -> m ()
updateFile renames' filePath
  | isDryRun = pure ()
  | otherwise = do
      contents <- liftIO $ T.decodeUtf8 <$> B.readFile filePath
      liftIO $ T.writeFile filePath $ substituteModuleNames renames' contents

listUntouchedModules :: [FilePath] -> [ModuleName] -> [FilePath]
listUntouchedModules allFiles modulesToRename = do
  let nonTestHsFiles =
        fromList
        . filterHaskellFiles
        $ filter ("cardano-api/src" `isPrefixOf`)
        allFiles
          :: Set FilePath
  toList $ S.difference nonTestHsFiles (fromList $ toFilePath <$> modulesToRename)

instance Display String where
  display = fromString

instance Display [FilePath] where
  display = display . intercalate "\n"
