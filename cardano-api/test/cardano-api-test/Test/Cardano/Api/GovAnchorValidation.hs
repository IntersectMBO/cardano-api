{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.GovAnchorValidation
  ( tests
  )
where

import Cardano.Api
  ( CIP108 (..)
  , CIP119 (..)
  , File (File)
  , FileDirection (In)
  , FileError
  , readByteStringFile
  )
import Cardano.Api.Certificate (DRepMetadata (..))
import Cardano.Api.Governance (validateGovActionAnchorData)

import Data.ByteString (ByteString)
import Data.Monoid (Any)

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras (propertyOnce)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.GovAnchorValidation"
-- ```
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.GovAnchorValidation"
    [ testProperty
        "Positive test for DRep registration JSON schema"
        prop_positive_drep_registration_json
    , testProperty
        "Missing 'givenName' test for DRep registration JSON schema"
        prop_missing_given_name_drep_registration_json
    , testProperty
        "Given name too long test for DRep registration JSON schema"
        prop_given_name_too_long_drep_registration_json
    , testProperty
        "Positive test for no confidence JSON schema"
        prop_positive_no_confidence_json
    , testProperty
        "Positive test for treasury withdrawal JSON schema"
        prop_positive_treasury_withdrawal_json
    , testProperty
        "Title name too long test for treasury withdrawal JSON schema"
        prop_title_name_too_long_treasury_withdrawal_json
    ]

prop_positive_drep_registration_json :: Property
prop_positive_drep_registration_json = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/valid-drep-metadata.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateGovActionAnchorData DrepRegistrationMetadata value === Right ()

prop_missing_given_name_drep_registration_json :: Property
prop_missing_given_name_drep_registration_json = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/invalid-drep-metadata.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateGovActionAnchorData DrepRegistrationMetadata value
    === Left "Error in $.body: key \"givenName\" not found"

prop_given_name_too_long_drep_registration_json :: Property
prop_given_name_too_long_drep_registration_json = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/too-long-given-name-drep-metadata.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateGovActionAnchorData DrepRegistrationMetadata value
    === Left "Error in $.body: key \"givenName\" exceeds maximum length of 80 characters. Got length: 90"

prop_positive_no_confidence_json :: Property
prop_positive_no_confidence_json = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/no-confidence.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateGovActionAnchorData BaseGovActionMetadata value === Right ()

prop_positive_treasury_withdrawal_json :: Property
prop_positive_treasury_withdrawal_json = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/threasury-withdrawal.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateGovActionAnchorData BaseGovActionMetadata value === Right ()

prop_title_name_too_long_treasury_withdrawal_json :: Property
prop_title_name_too_long_treasury_withdrawal_json = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/too-long-title-treasury-withdraw.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateGovActionAnchorData BaseGovActionMetadata value
    === Left "Error in $.body: key \"title\" exceeds maximum length of 80 characters. Got length: 112"
