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
isDryRun = False

-- The proposed changes. Renames of modules in this list are without 'Cardano.Api' prefix: [(new module name, old module name)]
-- The TODOs have to be performed manually
renames :: [(ModuleName, ModuleName)]
renames = map (bimap prependModulePrefix prependModulePrefix)
  [
    ("Byron.Internal.Key", "Internal.Keys.Byron")
  , ("Byron.Internal.Proposal", "Internal.SpecialByron")

  , ("Certificate.Internal", "Internal.Certificate")
  , ("Certificate.Internal.OperationalCertificate", "Internal.OperationalCertificate")
  , ("Certificate.Internal.StakePoolMetadata", "Internal.StakePoolMetadata")
  , ("Certificate.Internal.DRepMetadata", "Internal.DRepMetadata")

  , ("Compatible.Tx", "Internal.Compatible.Tx")

  , ("ProtocolParameters", "Internal.ProtocolParameters")

  , ("Consensus.Internal.Reexport", "Internal.ReexposeConsensus")
  , ("Consensus.Internal.Mode", "Internal.Modes")
  , ("Consensus.Internal.InMode", "Internal.InMode")
  , ("Consensus.Internal.Protocol", "Internal.Protocol")

  , ("Era.Internal.Case", "Internal.Eras.Case")
  , ("Era.Internal.Core", "Internal.Eras.Core")
  , ("Era.Internal.Feature", "Internal.Feature")
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
  , ("Era", "Internal.Eras")

  , ("Experimental.Era", "Internal.Experimental.Eras")
  , ("Experimental.Plutus.Internal.IndexedPlutusScriptWitness", "Internal.Experimental.Plutus.IndexedPlutusScriptWitness")
  , ("Experimental.Plutus.Internal.ScriptWitness", "Internal.Experimental.Plutus.ScriptWitness")
  , ("Experimental.Plutus.Internal.Script", "Internal.Experimental.Plutus.Script")
  , ("Experimental.Plutus.Internal.Shim.LegacyScripts", "Internal.Experimental.Plutus.Shim.LegacyScripts")

  , ("Experimental.Simple.Script", "Internal.Experimental.Simple.Script")
  , ("Experimental.Tx", "Internal.Experimental.Tx")
  , ("Experimental.Tx.Internal.AnyWitness", "Internal.Experimental.Witness.AnyWitness")
  , ("Experimental.Tx.Internal.TxScriptWitnessRequirements", "Internal.Experimental.Witness.TxScriptWitnessRequirements")

  , ("Genesis.Internal.Parameters", "Internal.GenesisParameters")
  , ("Genesis.Internal", "Internal.Genesis")

  , ("Governance.Internal.Action.ProposalProcedure", "Internal.Governance.Actions.ProposalProcedure")
  , ("Governance.Internal.Action.VotingProcedure", "Internal.Governance.Actions.VotingProcedure")
  , ("Governance.Internal.Metadata.Anchor", "Internal.Anchor")
  , ("Governance.Internal.Metadata.DrepRegistration", "Internal.Governance.Metadata.DrepRegistration")
  , ("Governance.Internal.Metadata.GovAction", "Internal.Governance.Metadata.GovAction")
  , ("Governance.Internal.Metadata.Validation", "Internal.Governance.Metadata.Validation")
  , ("Governance.Internal.Poll", "Internal.Governance.Poll")

  , ("IO.Internal.Compat.Posix", "Internal.IO.Compat.Posix")
  , ("IO.Internal.Compat.Win32", "Internal.IO.Compat.Win32")
  , ("IO.Internal.Compat", "Internal.IO.Compat")
  , ("IO.Internal.Base", "Internal.IO.Base")
  , ("IO", "Internal.IO")

  , ("Key.Internal", "Internal.Keys.Shelley")
  , ("Key.Internal.Class", "Internal.Keys.Class")
  , ("Key.Internal.Mnemonic", "Internal.Keys.Mnemonics")
  , ("Key.Internal.Praos", "Internal.Keys.Praos")
  , ("Key.Internal.SomeAddressVerificationKey", "Internal.Keys.SomeAddressVerificationKey")

  , ("Ledger.Internal.Reexport", "Internal.ReexposeLedger")

  , ("LedgerState", "Internal.LedgerState")
  , ("LedgerState.Internal.ConvertLedgerEvent", "Internal.LedgerEvents.ConvertLedgerEvent")
  , ("LedgerState.Internal.LedgerEvent", "Internal.LedgerEvents.LedgerEvent")
  , ("LedgerState.Internal.Rule.BBODY.DELEGS", "Internal.LedgerEvents.Rule.BBODY.DELEGS")
  , ("LedgerState.Internal.Rule.BBODY.LEDGER", "Internal.LedgerEvents.Rule.BBODY.LEDGER")
  , ("LedgerState.Internal.Rule.BBODY.UTXOW", "Internal.LedgerEvents.Rule.BBODY.UTXOW")
  , ("LedgerState.Internal.Rule.TICK.RUPD", "Internal.LedgerEvents.Rule.TICK.RUPD")
  , ("LedgerState.Internal.Rule.TICK.NEWEPOCH", "Internal.LedgerEvents.Rule.TICK.NEWEPOCH")

  , ("Network.Internal.Reexport", "Internal.ReexposeNetwork")
  , ("Network.Internal.NetworkId", "Internal.NetworkId")

  , ("Network.IPC.Internal.ChainSync.ClientPipelined", "ChainSync.ClientPipelined")
  , ("Network.IPC.Internal.ChainSync.Client", "ChainSync.Client")
  , ("Network.IPC.Internal.Monad", "Internal.IPC.Monad")
  , ("Network.IPC.Internal.Version", "Internal.IPC.Version")
  , ("Network.IPC.Internal", "Internal.IPC")

  , ("Plutus.Internal", "Internal.Plutus")
  , ("Plutus.Internal.ScriptData", "Internal.ScriptData")
  , ("Plutus.Internal.Script", "Internal.Script")

  , ("Serialise.Bech32", "Internal.SerialiseBech32")
  , ("Serialise.Cip129", "Internal.CIP.Cip129")
  , ("Serialise.Cbor.Canonical", "Internal.Serialise.Cbor.Canonical")
  , ("Serialise.Cbor", "Internal.Serialise.Cbor")
  , ("Serialise.DeserialiseAnyOf", "Internal.DeserialiseAnyOf")
  , ("Serialise.Json", "Internal.SerialiseJSON")
  , ("Serialise.Raw", "Internal.SerialiseRaw")
  , ("Serialise.SerialiseUsing", "Internal.SerialiseUsing")

  , ("Serialise.TextEnvelope.Internal", "Internal.SerialiseTextEnvelope")
  , ("Serialise.TextEnvelope.Internal.Cddl", "Internal.SerialiseLedgerCddl")

  , ("Query.Internal.Convenience", "Internal.Convenience.Query")
  , ("Query.Internal.Expr", "Internal.Query.Expr")
  , ("Query.Internal.Type.DebugLedgerState", "Internal.Query.Types")
  , ("Query.Internal.Type.DelegationsAndRewards", "Internal.Rewards")
  , ("Query.Internal.Type.QueryInMode", "Internal.Query")

  , ("Tx.Internal.Body", "Internal.Tx.Body")
  , ("Tx.Internal.Body.Lens", "Ledger.Lens")
  , ("Tx.Internal.Convenience", "Internal.Convenience.Construction")
  , ("Tx.Internal.Fee", "Internal.Fees")
  , ("Tx.Internal.Sign", "Internal.Tx.Sign")
  , ("Tx.Internal.BuildTxWith", "Internal.Tx.BuildTxWith")
  , ("Tx.Internal.Output", "Internal.Tx.Output")
  , ("Tx.Internal.TxIn", "Internal.TxIn")
  , ("Tx.Internal.TxMetadata", "Internal.TxMetadata")

  , ("Address", "Internal.Address")
  , ("Block", "Internal.Block")
  , ("Error", "Internal.Error")
  , ("HasTypeProxy", "Internal.HasTypeProxy")
  , ("Hash", "Internal.Hash")
  , ("Monad.Error", "Internal.Monad.Error")
  , ("Pretty", "Internal.Pretty")
  , ("Pretty.Internal.ShowOf", "Internal.Via.ShowOf")
  , ("UTxO", "Internal.Tx.UTxO")
  , ("Value.Internal.Parser", "Internal.ValueParser")
  , ("Value.Internal", "Internal.Value")

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
