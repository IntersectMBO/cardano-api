module Test.Cardano.Api.Ord
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Shelley

import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.Metadata (genTxMetadataValue)

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

ord_distributive
  :: (Show a, Ord a, Ord b)
  => H.Gen a -> (a -> b) -> Property
ord_distributive gen to =
  H.property $ do
    x <- H.forAll gen
    y <- H.forAll gen
    compare x y === compare (to x) (to y)

prop_ord_distributive_TxId :: Property
prop_ord_distributive_TxId =
  ord_distributive genTxId toShelleyTxId

prop_ord_distributive_TxIn :: Property
prop_ord_distributive_TxIn =
  ord_distributive genTxIn toShelleyTxIn

prop_ord_distributive_Address :: Property
prop_ord_distributive_Address =
  ord_distributive genAddressShelley (toShelleyAddr . toAddressInAnyEra)
 where
  toAddressInAnyEra :: Address ShelleyAddr -> AddressInEra ShelleyEra
  toAddressInAnyEra = anyAddressInShelleyBasedEra ShelleyBasedEraShelley . toAddressAny

prop_ord_distributive_StakeAddress :: Property
prop_ord_distributive_StakeAddress =
  ord_distributive genStakeAddress toShelleyStakeAddr

prop_ord_distributive_TxMetadata :: Property
prop_ord_distributive_TxMetadata =
  ord_distributive genTxMetadataValue toShelleyMetadatum

prop_ord_distributive_ScriptData :: Property
prop_ord_distributive_ScriptData =
  ord_distributive (getScriptData <$> genHashableScriptData) toPlutusData

prop_ord_distributive_Certificate :: Property
prop_ord_distributive_Certificate = H.property $ do
  AnyShelleyBasedEra sbe <- H.forAll H.enumBounded
  cert1 <- H.forAll $ genCertificate sbe
  cert2 <- H.forAll $ genCertificate sbe
  case (cert1, cert2) of
    (ShelleyRelatedCertificate w1 c1, ShelleyRelatedCertificate _ c2) -> do
      shelleyToBabbageEraConstraints w1 $
        compare cert1 cert2 === compare c1 c2
    (ConwayCertificate w1 c1, ConwayCertificate _ c2) ->
      conwayEraOnwardsConstraints w1 $
        compare cert1 cert2 === compare c1 c2
    _ -> H.note_ "impossible, two different eras!" >> H.failure

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.Ord"
    [ testProperty "ord distributive TxId" prop_ord_distributive_TxId
    , testProperty "ord distributive TxIn" prop_ord_distributive_TxIn
    , testProperty "ord distributive Address" prop_ord_distributive_Address
    , testProperty "ord distributive StakeAddress" prop_ord_distributive_StakeAddress
    , testProperty "ord distributive TxMetadata" prop_ord_distributive_TxMetadata
    , testProperty "ord distributive ScriptData" prop_ord_distributive_ScriptData
    , testProperty "ord distributive Certificate" prop_ord_distributive_Certificate
    ]
