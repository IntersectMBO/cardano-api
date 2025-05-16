module Test.Golden.Cardano.Api.Ledger
  ( test_golden_ShelleyGenesis
  )
where

import Test.Cardano.Ledger.Core.Arbitrary ()

import Hedgehog.Extras.Aeson qualified as H
import Test.Golden.Cardano.Api.Genesis (exampleShelleyGenesis)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

test_golden_ShelleyGenesis :: TestTree
test_golden_ShelleyGenesis =
  testProperty "golden ShelleyGenesis" $
    H.goldenTestJsonValuePretty
      exampleShelleyGenesis
      "test/cardano-api-golden/files/ShelleyGenesis.json"
