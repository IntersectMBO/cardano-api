{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Cip129
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Ledger

import Cardano.Ledger.Api.Governance qualified as Gov
import Cardano.Ledger.Keys qualified as L

import Test.Gen.Cardano.Api.Typed ()

import Test.Cardano.Api.Orphans ()

import Hedgehog as H
import Hedgehog.Gen.QuickCheck qualified as Q
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_cip129 :: Property
prop_roundtrip_cip129 = H.property $ do
  coldCommittee <- forAll $ Q.arbitrary @(Credential L.ColdCommitteeRole)
  tripping coldCommittee serialiseToBech32Cip129 deserialiseFromBech32Cip129

  hotCommittee <- forAll $ Q.arbitrary @(Credential L.HotCommitteeRole)
  tripping hotCommittee serialiseToBech32Cip129 deserialiseFromBech32Cip129

  drep <- forAll $ Q.arbitrary @(Credential L.DRepRole)
  tripping drep serialiseToBech32Cip129 deserialiseFromBech32Cip129

  govActionId <- forAll $ Q.arbitrary @Gov.GovActionId
  tripping govActionId serialiseGovActionIdToBech32Cip129 deserialiseGovActionIdFromBech32Cip129

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Cip129"
    [testProperty "test Cip129 serialisation roundtrip" prop_roundtrip_cip129]
