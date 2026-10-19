{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.NodeConfig
  ( tests
  )
where

import Cardano.Api (EpochNo (..), File (..))
import Cardano.Api.LedgerState
  ( GenesisConfig (..)
  , NodeConfig
  , NodeConfigError (..)
  , ShelleyConfig (..)
  , envSecurityParam
  , initialLedgerState
  , ncByronProtocolVersion
  , ncHardForkTriggers
  , ncPBftSignatureThreshold
  , readAlonzoGenesisConfig
  , readByronGenesisConfig
  , readCardanoGenesisConfig
  , readConwayGenesisConfig
  , readNodeConfig
  , readShelleyGenesisConfig
  )

import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.Update qualified as Byron
import Cardano.Configuration qualified as Cfg
import Cardano.Ledger.Alonzo.Genesis qualified as Alonzo
import Cardano.Ledger.Api.Transition qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Conway.Genesis qualified as Conway
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Ledger.Dijkstra.Genesis qualified as Dijkstra
import Cardano.Ledger.Dijkstra.PParams qualified as Dijkstra
import Cardano.Ledger.Shelley.Genesis qualified as Shelley
import Ouroboros.Consensus.Cardano.Node qualified as Consensus

import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32)
import GHC.Stack
import Lens.Micro ((^.))
import System.Directory (makeAbsolute)
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.FilePath (takeDirectory, takeFileName, (</>))

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | The genesis files the configurations below name.
genesisDir :: FilePath
genesisDir = "test/cardano-api-test/files/input/node-config"

-- | The genesis keys of a configuration naming the files in 'genesisDir' by
-- absolute path, with their hashes.
genesisKeys :: FilePath -> [(Aeson.Key, Aeson.Value)]
genesisKeys dir =
  [ "ByronGenesisFile" Aeson..= (dir </> "byron-genesis.json")
  , "ByronGenesisHash"
      Aeson..= ("31cac3a016a7d1db5460ad98e43c27eee4bad769c739a694eddd6a82d944130a" :: Text)
  , "ShelleyGenesisFile" Aeson..= (dir </> "shelley-genesis.json")
  , "ShelleyGenesisHash"
      Aeson..= ("6dc7b632017152c61a69dd17f50c03bfabbe3d24a9ad27273693bb8666140423" :: Text)
  , "AlonzoGenesisFile" Aeson..= (dir </> "alonzo-genesis.json")
  , "AlonzoGenesisHash"
      Aeson..= ("7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874" :: Text)
  , "ConwayGenesisFile" Aeson..= (dir </> "conway-genesis.json")
  , "ConwayGenesisHash"
      Aeson..= ("15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62" :: Text)
  , "RequiresNetworkMagic" Aeson..= ("RequiresNoMagic" :: Text)
  ]

-- | The keys naming the Dijkstra genesis in 'genesisDir', with experimental hard
-- forks enabled or not. That genesis is the example Dijkstra genesis, except for
-- its maximum reference script size per transaction: 307200 instead of 204800.
dijkstraKeys :: Bool -> FilePath -> [(Aeson.Key, Aeson.Value)]
dijkstraKeys enabled dir =
  [ "ExperimentalHardForksEnabled" Aeson..= enabled
  , "DijkstraGenesisFile" Aeson..= (dir </> "dijkstra-genesis.json")
  , "DijkstraGenesisHash"
      Aeson..= ("6569e322c881684123b5f962176afb8d99d664123b066d24e7d32ad651dc0f95" :: Text)
  ]

-- | Write the configuration to a file in a fresh workspace, and use the file.
withConfigFile
  :: HasCallStack
  => (FilePath -> Aeson.Value)
  -- ^ The configuration, given the absolute path of 'genesisDir'.
  -> (FilePath -> H.Integration ())
  -> H.Integration ()
withConfigFile mkConfig use = withFrozenCallStack $ do
  dir <- H.evalIO $ makeAbsolute genesisDir
  H.moduleWorkspace "node-config" $ \ws -> do
    let file = ws </> "config.json"
    H.evalIO . Aeson.encodeFile file $ mkConfig dir
    use file

-- | Write the configuration to a file in a fresh workspace, read it back with
-- 'readNodeConfig', and check the result.
withConfig
  :: HasCallStack
  => (FilePath -> Aeson.Value)
  -- ^ The configuration, given the absolute path of 'genesisDir'.
  -> (Either NodeConfigError NodeConfig -> H.Integration ())
  -> H.Integration ()
withConfig mkConfig expect = withFrozenCallStack $ withConfigFile mkConfig $ \file ->
  H.evalIO (runExceptT . fmap fst . readNodeConfig $ File file) >>= expect

-- | A configuration in the flat format, with the given extra keys.
flatConfig :: [(Aeson.Key, Aeson.Value)] -> FilePath -> Aeson.Value
flatConfig extra dir = Aeson.object $ genesisKeys dir <> extra

-- | The configuration reads, and its Dijkstra hard fork is at epoch 5.
expectDijkstraAtEpoch5 :: HasCallStack => Either NodeConfigError NodeConfig -> H.Integration ()
expectDijkstraAtEpoch5 result = withFrozenCallStack $ do
  nc <- H.evalEither result
  case ncHardForkTriggers nc of
    Consensus.CardanoHardForkTriggers'{Consensus.triggerHardForkDijkstra = trigger} ->
      case trigger of
        Consensus.CardanoTriggerHardForkAtEpoch (EpochNo 5) -> H.success
        other -> H.annotateShow other >> H.failure

-- | The maximum reference script size per transaction of the Dijkstra genesis
-- that 'readCardanoGenesisConfig' gives for the configuration.
readMaxRefScriptSizePerTx :: HasCallStack => NodeConfig -> H.Integration Word32
readMaxRefScriptSizePerTx nc = withFrozenCallStack $ do
  GenesisCardano _ _ _ transCfg <-
    H.evalEither =<< H.evalIO (runExceptT $ readCardanoGenesisConfig nc)
  pure . Dijkstra.udppMaxRefScriptSizePerTx . Dijkstra.dgUpgradePParams $
    transCfg ^. Ledger.tcTranslationContextL

-- | The configuration does not read: cardano-config rejects it with an error
-- that passes the given check.
expectRejectedWith
  :: HasCallStack
  => (Cfg.ConfigurationParsingError -> H.Integration ())
  -> Either NodeConfigError NodeConfig
  -> H.Integration ()
expectRejectedWith checkError result = withFrozenCallStack $
  case result of
    Right _ -> H.annotate "the configuration was read" >> H.failure
    Left (NodeConfigParsingError err) -> H.annotateShow err >> checkError err
    Left err -> H.annotateShow err >> H.failure

prop_read_dijkstra_hard_fork_at_epoch :: Property
prop_read_dijkstra_hard_fork_at_epoch =
  H.propertyOnce $
    withConfig (flatConfig ["TestDijkstraHardForkAtEpoch" Aeson..= (5 :: Int)]) expectDijkstraAtEpoch5

prop_read_dijkstra_hard_fork_default :: Property
prop_read_dijkstra_hard_fork_default = H.propertyOnce . withConfig (flatConfig []) $ \result -> do
  nc <- H.evalEither result
  case ncHardForkTriggers nc of
    Consensus.CardanoHardForkTriggers'{Consensus.triggerHardForkDijkstra = trigger} ->
      case trigger of
        Consensus.CardanoTriggerHardForkAtDefaultVersion -> H.success
        other -> H.annotateShow other >> H.failure

prop_read_enveloped_config :: Property
prop_read_enveloped_config = H.propertyOnce $ withConfig envelopedConfig expectDijkstraAtEpoch5
 where
  envelopedConfig dir =
    Aeson.object
      [ "Version" Aeson..= (2 :: Int)
      , "Configuration"
          Aeson..= Aeson.object
            [ "Protocol" Aeson..= Aeson.object (genesisKeys dir)
            , "Testing" Aeson..= Aeson.object ["TestDijkstraHardForkAtEpoch" Aeson..= (5 :: Int)]
            ]
      ]

prop_reject_config_without_genesis_hashes :: Property
prop_reject_config_without_genesis_hashes =
  H.propertyOnce . withConfig withoutHashes . expectRejectedWith $ \err -> do
    -- The protocol section of the configuration itself, not a genesis file.
    Cfg.errFile err H.=== SNothing
    Cfg.errSection err H.=== SJust "Protocol"
 where
  withoutHashes dir =
    Aeson.object
      [ (k, v)
      | (k, v) <- genesisKeys dir
      , k `notElem` ["ByronGenesisHash", "ShelleyGenesisHash", "AlonzoGenesisHash", "ConwayGenesisHash"]
      ]

-- | A configuration whose hash for one genesis file is wrong is rejected, and the
-- error names that file. The Byron hash is checked differently from the others
-- (over canonical JSON rather than the raw bytes).
prop_reject_wrong_genesis_hash
  :: Aeson.Key
  -- ^ The hash to break.
  -> FilePath
  -- ^ The genesis file it is the hash of.
  -> Property
prop_reject_wrong_genesis_hash hashKey genesisFile =
  H.propertyOnce . withConfig wrongHash . expectRejectedWith $ \err ->
    (takeFileName <$> Cfg.errFile err) H.=== SJust genesisFile
 where
  wrongHash dir =
    Aeson.object
      [ if k == hashKey then (k, Aeson.toJSON (Text.replicate 64 "0")) else (k, v)
      | (k, v) <- genesisKeys dir
      ]

prop_reject_missing_genesis_file :: Property
prop_reject_missing_genesis_file =
  H.propertyOnce . withConfig missingShelleyGenesis . expectRejectedWith $ \err ->
    (takeFileName <$> Cfg.errFile err) H.=== SJust "missing-genesis.json"
 where
  missingShelleyGenesis dir =
    Aeson.object
      [ if k == "ShelleyGenesisFile" then (k, Aeson.toJSON (dir </> "missing-genesis.json")) else (k, v)
      | (k, v) <- genesisKeys dir
      ]

-- | Each genesis file ends up in the configuration. The values are the ones in
-- the files in 'genesisDir'.
prop_read_genesis_files :: Property
prop_read_genesis_files = H.propertyOnce . withConfig (flatConfig []) $ \result -> do
  nc <- H.evalEither result
  Byron.gdStartTime (Byron.configGenesisData $ readByronGenesisConfig nc)
    H.=== posixSecondsToUTCTime 1506203091
  Shelley.sgNetworkMagic (scConfig $ readShelleyGenesisConfig nc) H.=== 764824073
  Alonzo.agCollateralPercentage (readAlonzoGenesisConfig nc) H.=== 150
  Conway.ucppGovActionLifetime (Conway.cgUpgradePParams $ readConwayGenesisConfig nc)
    H.=== Ledger.EpochInterval 6

-- | The Byron settings that are no longer read. Both functions are deprecated,
-- hence @-Wno-deprecations@ in this module.
prop_byron_constants :: Property
prop_byron_constants = H.propertyOnce . withConfig (flatConfig []) $ \result -> do
  nc <- H.evalEither result
  ncPBftSignatureThreshold nc H.=== Nothing
  ncByronProtocolVersion nc H.=== Byron.ProtocolVersion 1 0 0

prop_initial_ledger_state :: Property
prop_initial_ledger_state = H.propertyOnce . withConfigFile (flatConfig []) $ \file -> do
  let fs = SomeHasFS . ioHasFS . MountPoint $ takeDirectory file
  (env, _) <- H.evalEither =<< H.evalIO (runExceptT $ initialLedgerState fs (File file))
  -- The security parameter of the genesis files in 'genesisDir'.
  envSecurityParam env H.=== 2160

prop_read_dijkstra_genesis_with_experimental_hard_forks :: Property
prop_read_dijkstra_genesis_with_experimental_hard_forks =
  H.propertyOnce . withConfig (\dir -> flatConfig (dijkstraKeys True dir) dir) $ \result -> do
    nc <- H.evalEither result
    size <- readMaxRefScriptSizePerTx nc
    size H.=== 307200

prop_use_example_dijkstra_genesis_without_experimental_hard_forks :: Property
prop_use_example_dijkstra_genesis_without_experimental_hard_forks =
  H.propertyOnce . withConfig (\dir -> flatConfig (dijkstraKeys False dir) dir) $ \result -> do
    nc <- H.evalEither result
    size <- readMaxRefScriptSizePerTx nc
    -- The example Dijkstra genesis's, not the one in the file the configuration
    -- names.
    size H.=== 204800

prop_reject_experimental_hard_forks_without_dijkstra_genesis :: Property
prop_reject_experimental_hard_forks_without_dijkstra_genesis =
  H.propertyOnce . withConfig (flatConfig ["ExperimentalHardForksEnabled" Aeson..= True]) $ \case
    Left (NodeConfigResolutionError (Cfg.ViolatedChecks _)) -> H.success
    Left err -> H.annotateShow err >> H.failure
    Right _ -> H.annotate "the configuration was read" >> H.failure

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.NodeConfig"
    [ testProperty "read TestDijkstraHardForkAtEpoch" prop_read_dijkstra_hard_fork_at_epoch
    , testProperty "read Dijkstra hard fork default" prop_read_dijkstra_hard_fork_default
    , testProperty "read an enveloped configuration" prop_read_enveloped_config
    , testProperty
        "reject a configuration without genesis hashes"
        prop_reject_config_without_genesis_hashes
    , testProperty "reject a wrong Byron genesis hash" $
        prop_reject_wrong_genesis_hash "ByronGenesisHash" "byron-genesis.json"
    , testProperty "reject a wrong Shelley genesis hash" $
        prop_reject_wrong_genesis_hash "ShelleyGenesisHash" "shelley-genesis.json"
    , testProperty "reject a wrong Alonzo genesis hash" $
        prop_reject_wrong_genesis_hash "AlonzoGenesisHash" "alonzo-genesis.json"
    , testProperty "reject a wrong Conway genesis hash" $
        prop_reject_wrong_genesis_hash "ConwayGenesisHash" "conway-genesis.json"
    , testProperty "reject a missing genesis file" prop_reject_missing_genesis_file
    , testProperty "read the genesis files" prop_read_genesis_files
    , testProperty "read the Byron constants" prop_byron_constants
    , testProperty "read an initial ledger state" prop_initial_ledger_state
    , testProperty
        "read the Dijkstra genesis with experimental hard forks"
        prop_read_dijkstra_genesis_with_experimental_hard_forks
    , testProperty
        "use the example Dijkstra genesis without experimental hard forks"
        prop_use_example_dijkstra_genesis_without_experimental_hard_forks
    , testProperty
        "reject experimental hard forks without a Dijkstra genesis"
        prop_reject_experimental_hard_forks_without_dijkstra_genesis
    ]
