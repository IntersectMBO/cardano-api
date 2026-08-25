{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Api.NodeConfig
  ( tests
  )
where

import Cardano.Api (EpochNo (..))
import Cardano.Api.LedgerState (NodeConfig (..))

import Ouroboros.Consensus.Cardano.Node qualified as Consensus

import Data.Aeson qualified as Aeson
import GHC.Stack

import Hedgehog as H
import Hedgehog.Extras (propertyOnce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | A minimal node configuration with the given extra keys.
nodeConfigWith :: [(Aeson.Key, Aeson.Value)] -> Aeson.Value
nodeConfigWith extras =
  Aeson.object $
    [ "ByronGenesisFile" Aeson..= ("byron-genesis.json" :: String)
    , "ShelleyGenesisFile" Aeson..= ("shelley-genesis.json" :: String)
    , "AlonzoGenesisFile" Aeson..= ("alonzo-genesis.json" :: String)
    , "ConwayGenesisFile" Aeson..= ("conway-genesis.json" :: String)
    , "RequiresNetworkMagic" Aeson..= ("RequiresNoMagic" :: String)
    , "LastKnownBlockVersion-Major" Aeson..= (3 :: Int)
    , "LastKnownBlockVersion-Minor" Aeson..= (0 :: Int)
    , "LastKnownBlockVersion-Alt" Aeson..= (0 :: Int)
    ]
      <> map (uncurry (Aeson..=)) extras

parseTriggers
  :: (HasCallStack, MonadTest m)
  => [(Aeson.Key, Aeson.Value)]
  -> m Consensus.CardanoHardForkTriggers
parseTriggers extras =
  case Aeson.fromJSON $ nodeConfigWith extras of
    Aeson.Error e -> withFrozenCallStack $ H.annotate e >> H.failure
    Aeson.Success nc -> pure $ ncHardForkTriggers nc

prop_parse_dijkstra_hard_fork_at_epoch :: Property
prop_parse_dijkstra_hard_fork_at_epoch = propertyOnce $ do
  triggers <- parseTriggers [("TestDijkstraHardForkAtEpoch", Aeson.toJSON (5 :: Int))]
  case triggers of
    Consensus.CardanoHardForkTriggers'{Consensus.triggerHardForkDijkstra = trigger} ->
      case trigger of
        Consensus.CardanoTriggerHardForkAtEpoch (EpochNo 5) -> H.success
        other -> H.annotateShow other >> H.failure

prop_parse_dijkstra_hard_fork_default :: Property
prop_parse_dijkstra_hard_fork_default = propertyOnce $ do
  triggers <- parseTriggers []
  case triggers of
    Consensus.CardanoHardForkTriggers'{Consensus.triggerHardForkDijkstra = trigger} ->
      case trigger of
        Consensus.CardanoTriggerHardForkAtDefaultVersion -> H.success
        other -> H.annotateShow other >> H.failure

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.NodeConfig"
    [ testProperty "parse TestDijkstraHardForkAtEpoch" prop_parse_dijkstra_hard_fork_at_epoch
    , testProperty "parse Dijkstra hard fork default" prop_parse_dijkstra_hard_fork_default
    ]
