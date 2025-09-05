{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Cip129
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Api.Governance qualified as Gov
import Cardano.Ledger.Core qualified as L

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack

import Test.Gen.Cardano.Api.Typed ()

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen.QuickCheck qualified as Q
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

-- | Test CIP-129 encodings. Contains examples from CIP-129 and dual hash types for committee roles.
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0129/README.md#abstract
prop_decode_cip129 :: Property
prop_decode_cip129 = H.propertyOnce $ do
  "cc_cold1zgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6yewvh"
    `assertDecodesTo` L.KeyHashObj @L.ColdCommitteeRole
      (L.KeyHash "00000000000000000000000000000000000000000000000000000000")

  "cc_cold1zvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6kflvs"
    `assertDecodesTo` L.ScriptHashObj @L.ColdCommitteeRole
      (L.ScriptHash "00000000000000000000000000000000000000000000000000000000")

  "cc_hot1qgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqvcdjk7"
    `assertDecodesTo` L.KeyHashObj @L.HotCommitteeRole
      (L.KeyHash "00000000000000000000000000000000000000000000000000000000")

  "cc_hot1qvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqv2arke"
    `assertDecodesTo` L.ScriptHashObj @L.HotCommitteeRole
      (L.ScriptHash "00000000000000000000000000000000000000000000000000000000")

  "drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n"
    `assertDecodesTo` L.KeyHashObj @L.DRepRole (L.KeyHash "00000000000000000000000000000000000000000000000000000000")

  "drep1yvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq770f95"
    `assertDecodesTo` L.ScriptHashObj @L.DRepRole
      (L.ScriptHash "00000000000000000000000000000000000000000000000000000000")

  "gov_action1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqpzklpgpf"
    `assertDecodesTo` L.GovActionId
      { L.gaidTxId =
          L.TxId (L.unsafeMakeSafeHash "0000000000000000000000000000000000000000000000000000000000000000")
      , L.gaidGovActionIx = L.GovActionIx 17
      }

  "gov_action1zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygsq6dmejn"
    `assertDecodesTo` L.GovActionId
      { L.gaidTxId =
          L.TxId (L.unsafeMakeSafeHash "1111111111111111111111111111111111111111111111111111111111111111")
      , L.gaidGovActionIx = L.GovActionIx 0
      }
 where
  assertDecodesTo
    :: (HasCallStack, Show a, Eq a, Cip129 a, MonadTest m)
    => Text
    -> a
    -> m ()
  assertDecodesTo bech32id refIdentifier = withFrozenCallStack $ do
    identifier <- H.leftFail $ deserialiseFromBech32Cip129 bech32id
    H.note_ "Expected identifier in CIP129 format:"
    H.note_ . T.unpack $ serialiseToBech32Cip129 refIdentifier
    identifier === refIdentifier

prop_roundtrip_cip129 :: Property
prop_roundtrip_cip129 = H.property $ do
  coldCommittee <- forAll $ Q.arbitrary @(L.Credential L.ColdCommitteeRole)
  tripping coldCommittee serialiseToBech32Cip129 deserialiseFromBech32Cip129

  hotCommittee <- forAll $ Q.arbitrary @(L.Credential L.HotCommitteeRole)
  tripping hotCommittee serialiseToBech32Cip129 deserialiseFromBech32Cip129

  drep <- forAll $ Q.arbitrary @(L.Credential L.DRepRole)
  tripping drep serialiseToBech32Cip129 deserialiseFromBech32Cip129

  govActionId <- forAll $ Q.arbitrary @Gov.GovActionId
  tripping govActionId serialiseToBech32Cip129 deserialiseFromBech32Cip129

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Cip129"
    [ testProperty "test Cip129 serialisation roundtrip" prop_roundtrip_cip129
    , testProperty "test Cip129 sample deserialisation" prop_decode_cip129
    ]
