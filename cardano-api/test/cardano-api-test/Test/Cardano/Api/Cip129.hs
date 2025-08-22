{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Cip129
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Api.Governance qualified as Gov
import Cardano.Ledger.Core qualified as L

import Test.Gen.Cardano.Api.Typed ()

import Test.Cardano.Api.Orphans ()

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen.QuickCheck qualified as Q
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

prop_decode_cip129 :: Property
prop_decode_cip129 = H.propertyOnce $ do
  H.noteShow_ $
    serialiseToBech32Cip129 @(L.Credential L.HotCommitteeRole) $
      L.KeyHashObj (L.KeyHash "00000000000000000000000000000000000000000000000000000000")
  H.noteShow_ $
    serialiseToBech32Cip129 @(L.Credential L.HotCommitteeRole) $
      L.ScriptHashObj (L.ScriptHash "00000000000000000000000000000000000000000000000000000000")
  ccCredKey <-
    H.leftFail $
      deserialiseFromBech32Cip129 @(L.Credential L.ColdCommitteeRole)
        "cc_cold1zvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6kflvs"
  ccCredKey === L.KeyHashObj (L.KeyHash "91ce24460a9bf64bf5a829d8d57d3f7a157c2bf22d830c14b6b641ee")

  ccCredScript <-
    H.leftFail $
      deserialiseFromBech32Cip129 @(L.Credential L.ColdCommitteeRole)
        "cc_cold1zvqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6kflvs"
  ccCredScript
    === L.ScriptHashObj (L.ScriptHash "91ce24460a9bf64bf5a829d8d57d3f7a157c2bf22d830c14b6b641ee")

  hcCredKey <-
    H.leftFail $
      deserialiseFromBech32Cip129 @(L.Credential L.HotCommitteeRole)
        "cc_hot1qgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqvcdjk7"
  hcCredKey === L.KeyHashObj (L.KeyHash "ece12e2051c92b3da63193ec3190a07ad7ec6883e10685d3664fc49c")

  hcCredScript <-
    H.leftFail $
      deserialiseFromBech32Cip129 @(L.Credential L.HotCommitteeRole)
        "cc_hot1qgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqvcdjk7"
  hcCredScript
    === L.ScriptHashObj (L.ScriptHash "ece12e2051c92b3da63193ec3190a07ad7ec6883e10685d3664fc49c")

  drepCredKey <-
    H.leftFail $
      deserialiseFromBech32Cip129 @(L.Credential L.DRepRole)
        "drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n"
  drepCredKey === L.KeyHashObj (L.KeyHash "ece12e2051c92b3da63193ec3190a07ad7ec6883e10685d3664fc49c")

  drepCredScript <-
    H.leftFail $
      deserialiseFromBech32Cip129 @(L.Credential L.DRepRole)
        "drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n"
  drepCredScript
    === L.ScriptHashObj (L.ScriptHash "ece12e2051c92b3da63193ec3190a07ad7ec6883e10685d3664fc49c")

  govActId <-
    H.leftFail $
      deserialiseGovActionIdFromBech32Cip129
        "gov_action1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqpzklpgpf"

  govActId
    === L.GovActionId
      { L.gaidTxId =
          L.TxId (L.unsafeMakeSafeHash "1c210d1b25116472d96fe6676b0ebcb4339280ba080edc6d3647a75b2dcc1798")
      , L.gaidGovActionIx = L.GovActionIx 1
      }

prop_roundtrip_cip129 :: Property
prop_roundtrip_cip129 = H.property $ do
  coldCommittee <- forAll $ Q.arbitrary @(L.Credential L.ColdCommitteeRole)
  tripping coldCommittee serialiseToBech32Cip129 deserialiseFromBech32Cip129

  hotCommittee <- forAll $ Q.arbitrary @(L.Credential L.HotCommitteeRole)
  tripping hotCommittee serialiseToBech32Cip129 deserialiseFromBech32Cip129

  drep <- forAll $ Q.arbitrary @(L.Credential L.DRepRole)
  tripping drep serialiseToBech32Cip129 deserialiseFromBech32Cip129

  govActionId <- forAll $ Q.arbitrary @Gov.GovActionId
  tripping govActionId serialiseGovActionIdToBech32Cip129 deserialiseGovActionIdFromBech32Cip129

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Cip129"
    [ testProperty "test Cip129 serialisation roundtrip" prop_roundtrip_cip129
    , testProperty "test Cip129 sample deserialisation" prop_decode_cip129
    ]
