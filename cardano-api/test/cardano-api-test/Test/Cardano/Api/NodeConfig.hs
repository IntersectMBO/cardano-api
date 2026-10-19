{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Api.NodeConfig
  ( tests
  )
where

import Cardano.Api (EpochNo (..), File (..))
import Cardano.Api.LedgerState
  ( NodeConfig
  , NodeConfigError (..)
  , ncHardForkTriggers
  , readNodeConfig
  )

import Cardano.Configuration qualified as Cfg
import Ouroboros.Consensus.Cardano.Node qualified as Consensus

import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack
import System.Directory (makeAbsolute)
import System.FilePath (takeFileName, (</>))

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

-- | Write the configuration to a file in a fresh workspace, read it back with
-- 'readNodeConfig', and check the result.
withConfig
  :: HasCallStack
  => (FilePath -> Aeson.Value)
  -- ^ The configuration, given the absolute path of 'genesisDir'.
  -> (Either NodeConfigError NodeConfig -> H.Integration ())
  -> H.Integration ()
withConfig mkConfig expect = withFrozenCallStack $ do
  dir <- H.evalIO $ makeAbsolute genesisDir
  H.moduleWorkspace "node-config" $ \ws -> do
    let file = ws </> "config.json"
    H.evalIO . Aeson.encodeFile file $ mkConfig dir
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
    ]
