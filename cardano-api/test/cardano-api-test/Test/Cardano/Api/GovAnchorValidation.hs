{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.GovAnchorValidation
  ( tests
  )
where

import           Cardano.Api (File (File), FileDirection (In), FileError, readByteStringFile,
                   validateDRepAnchorData)
import           Cardano.Api.DRepMetadata (DRepMetadata (..))

import           Data.ByteString (ByteString)
import           Data.Monoid (Any)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Extras (propertyOnce)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.GovAnchorValidation"
-- ```
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.GovAnchorValidation"
    [ testProperty
        "Positive smoke test for DRep registration anchor data JSON schema"
        prop_positive_smoke_test_drep_registration_anchor_data_json_schema
    , testProperty
        "Negative smoke test for DRep registration anchor data JSON schema"
        prop_negative_smoke_test_drep_registration_anchor_data_json_schema
    ]

prop_positive_smoke_test_drep_registration_anchor_data_json_schema :: Property
prop_positive_smoke_test_drep_registration_anchor_data_json_schema = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/valid-drep-metadata.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateDRepAnchorData (DRepMetadata value) === Right ()

prop_negative_smoke_test_drep_registration_anchor_data_json_schema :: Property
prop_negative_smoke_test_drep_registration_anchor_data_json_schema = propertyOnce $ do
  (eitherValue :: Either (FileError Any) ByteString) <-
    readByteStringFile
      ( File "test/cardano-api-test/files/input/gov-anchor-data/invalid-drep-metadata.jsonld"
          :: File DRepMetadata In
      )
  value <- H.evalEither eitherValue
  validateDRepAnchorData (DRepMetadata value) === Left "Error in $.body: key \"givenName\" not found"
