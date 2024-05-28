module Test.Cardano.Api.Typed.Ord
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Metadata (genTxMetadataValue)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

ordDistributive :: (Show a, Ord a, Ord b)
                      => H.Gen a -> (a -> b) -> Property
ordDistributive gen to =
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen
      compare x y === compare (to x) (to y)


prop_ord_distributive_TxId :: Property
prop_ord_distributive_TxId =
    ordDistributive genTxId toShelleyTxId

prop_ord_distributive_TxIn :: Property
prop_ord_distributive_TxIn =
    ordDistributive genTxIn toShelleyTxIn

prop_ord_distributive_Address :: Property
prop_ord_distributive_Address =
    ordDistributive genAddressShelley (toShelleyAddr . toAddressInAnyEra)
  where
    toAddressInAnyEra :: Address ShelleyAddr -> AddressInEra ShelleyEra
    toAddressInAnyEra = anyAddressInShelleyBasedEra ShelleyBasedEraShelley . toAddressAny

prop_ord_distributive_StakeAddress :: Property
prop_ord_distributive_StakeAddress =
    ordDistributive genStakeAddress toShelleyStakeAddr

prop_ord_distributive_TxMetadata :: Property
prop_ord_distributive_TxMetadata =
    ordDistributive genTxMetadataValue toShelleyMetadatum

prop_ord_distributive_ScriptData :: Property
prop_ord_distributive_ScriptData =
    ordDistributive (getScriptData <$> genHashableScriptData) toPlutusData

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.Ord"
  [ testProperty "ord distributive TxId"         prop_ord_distributive_TxId
  , testProperty "ord distributive TxIn"         prop_ord_distributive_TxIn
  , testProperty "ord distributive Address"      prop_ord_distributive_Address
  , testProperty "ord distributive StakeAddress" prop_ord_distributive_StakeAddress
  , testProperty "ord distributive TxMetadata"   prop_ord_distributive_TxMetadata
  , testProperty "ord distributive ScriptData"   prop_ord_distributive_ScriptData
  ]
