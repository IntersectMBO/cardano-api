module Test.Golden.Cardano.Api.Value where

import           Cardano.Api (MaryEraOnwards (..), ShelleyBasedEra (..), ValueNestedBundle (..),
                   ValueNestedRep (..), fromLedgerValue, parseValue, renderValue, renderValuePretty,
                   valueFromNestedRep, valueToNestedRep)
import qualified Cardano.Api as Api

import           Prelude

import           Data.Aeson (eitherDecode, encode)
import           Data.List (groupBy, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec (parse)

import           Test.Gen.Cardano.Api.Typed (genAssetName, genValueDefault, genValueNestedRep)

import           Hedgehog (Property, forAll, property, tripping, (===))
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Test.Golden as H

{- HLINT ignore "Use let" -}

hprop_roundtrip_Value_parse_render :: Property
hprop_roundtrip_Value_parse_render =
  property $ do
    ledgerValue <- forAll $ genValueDefault MaryEraOnwardsConway
    let value = fromLedgerValue ShelleyBasedEraConway ledgerValue
    H.noteShow_ value
    tripping
      value
      renderValue
      (Parsec.parse parseValue "" . Text.unpack)

hprop_roundtrip_Value_parse_renderPretty :: Property
hprop_roundtrip_Value_parse_renderPretty =
  property $ do
    ledgerValue <- forAll $ genValueDefault MaryEraOnwardsConway
    let value = fromLedgerValue ShelleyBasedEraConway ledgerValue
    H.noteShow_ value
    tripping
      value
      renderValuePretty
      (Parsec.parse parseValue "" . Text.unpack)

hprop_goldenValue_1_lovelace :: Property
hprop_goldenValue_1_lovelace =
  H.propertyOnce $ do
    valueList <- pure [(Api.AdaAssetId, 1)]
    value <- pure $ Text.unpack $ Api.renderValuePretty $ Api.valueFromList valueList

    H.diffVsGoldenFile value "test/cardano-api-golden/files/golden/Cardano/Api/Value/value-ada-1.json"

hprop_goldenValue1 :: Property
hprop_goldenValue1 =
  H.propertyOnce $ do
    policyId <- pure $ Api.PolicyId "a0000000000000000000000000000000000000000000000000000000"
    assetName <- pure $ Api.AssetName "asset1"
    valueList <- pure [(Api.AssetId policyId assetName, 1)]
    value <- pure $ Text.unpack $ Api.renderValuePretty $ Api.valueFromList valueList

    H.diffVsGoldenFile
      value
      "test/cardano-api-golden/files/golden/Cardano/Api/Value/value-asset1-1.json"

hprop_roundtrip_Value_JSON :: Property
hprop_roundtrip_Value_JSON =
  property $ do
    v <- forAll $ fromLedgerValue ShelleyBasedEraConway <$> genValueDefault MaryEraOnwardsConway
    tripping v encode eitherDecode

hprop_roundtrip_Value_flatten_unflatten :: Property
hprop_roundtrip_Value_flatten_unflatten =
  property $ do
    v <- forAll $ fromLedgerValue ShelleyBasedEraConway <$> genValueDefault MaryEraOnwardsConway
    valueFromNestedRep (valueToNestedRep v) === v

hprop_roundtrip_Value_unflatten_flatten :: Property
hprop_roundtrip_Value_unflatten_flatten =
  property $ do
    v <- forAll genValueNestedRep
    canonicalise v === valueToNestedRep (valueFromNestedRep v)

canonicalise :: ValueNestedRep -> ValueNestedRep
canonicalise =
  ValueNestedRep
    . filter (not . isZeroOrEmpty)
    . map (filterZeros . foldl1 mergeBundle)
    . groupBy samePolicyId
    . sort
    . (\(ValueNestedRep bundles) -> bundles)
 where
  samePolicyId
    ValueNestedBundleAda{}
    ValueNestedBundleAda{} = True
  samePolicyId
    (ValueNestedBundle pid _)
    (ValueNestedBundle pid' _) = pid == pid'
  samePolicyId _ _ = False

  -- Merge together bundles that have already been grouped by same PolicyId:
  mergeBundle
    (ValueNestedBundleAda q)
    (ValueNestedBundleAda q') =
      ValueNestedBundleAda (q <> q')
  mergeBundle
    (ValueNestedBundle pid as)
    (ValueNestedBundle pid' as')
      | pid == pid' =
          ValueNestedBundle pid (Map.unionWith (<>) as as')
  mergeBundle _ _ = error "canonicalise.mergeBundle: impossible"

  filterZeros b@ValueNestedBundleAda{} = b
  filterZeros (ValueNestedBundle pid as) =
    ValueNestedBundle pid (Map.filter (/= 0) as)

  isZeroOrEmpty (ValueNestedBundleAda q) = q == 0
  isZeroOrEmpty (ValueNestedBundle _ as) = Map.null as

hprop_roundtrip_AssetName_JSON :: Property
hprop_roundtrip_AssetName_JSON =
  property $ do
    v <- forAll genAssetName
    tripping v encode eitherDecode

hprop_roundtrip_AssetName_JSONKey :: Property
hprop_roundtrip_AssetName_JSONKey =
  property $ do
    v <- forAll genAssetName
    tripping (Map.singleton v ()) encode eitherDecode
