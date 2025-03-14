module Test.Golden.Cardano.Api.Value where

import Cardano.Api
  ( MaryEraOnwards (..)
  , ShelleyBasedEra (..)
  , ValueNestedBundle (..)
  , ValueNestedRep (..)
  , fromLedgerValue
  , parseMintingMultiAssetValue
  , parseTxOutMultiAssetValue
  , renderMultiAsset
  , renderMultiAssetPretty
  , renderValue
  , renderValuePretty
  , valueFromNestedRep
  , valueToNestedRep
  )
import Cardano.Api qualified as Api
import Cardano.Api.Internal.Eras

import Prelude

import Data.Aeson (eitherDecode, encode)
import Data.List (groupBy, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Exts (IsList (..))
import Text.Parsec qualified as Parsec (parse)

import Test.Gen.Cardano.Api.Typed

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Test.Golden qualified as H

currentEra :: MaryEraOnwards ConwayEra
currentEra = MaryEraOnwardsConway

hprop_roundtrip_txout_Value_parse_render :: Property
hprop_roundtrip_txout_Value_parse_render =
  property $ do
    value <- forAll $ genValueForTxOut (convert currentEra)
    tripping
      value
      renderValue
      (Parsec.parse parseTxOutMultiAssetValue "" . Text.unpack)

hprop_roundtrip_txout_Value_parse_renderPretty :: Property
hprop_roundtrip_txout_Value_parse_renderPretty =
  property $ do
    value <- forAll $ genValueForTxOut (convert currentEra)
    tripping
      value
      renderValuePretty
      (Parsec.parse parseTxOutMultiAssetValue "" . Text.unpack)

hprop_roundtrip_mint_Value_parse_render :: Property
hprop_roundtrip_mint_Value_parse_render =
  property $ do
    value <- forAll genLedgerMultiAssetValue
    tripping
      value
      renderMultiAsset
      (Parsec.parse (parseMintingMultiAssetValue currentEra) "" . Text.unpack)

hprop_roundtrip_mint_Value_parse_renderPretty :: Property
hprop_roundtrip_mint_Value_parse_renderPretty =
  property $ do
    value <- forAll genLedgerMultiAssetValue
    tripping
      value
      renderMultiAssetPretty
      (Parsec.parse (parseMintingMultiAssetValue currentEra) "" . Text.unpack)

hprop_goldenValue_1_lovelace :: Property
hprop_goldenValue_1_lovelace =
  H.propertyOnce $ do
    let valueList = [(Api.AdaAssetId, 1)]
        value = Text.unpack $ Api.renderValuePretty $ fromList valueList

    H.diffVsGoldenFile value "test/cardano-api-golden/files/golden/Cardano/Api/Value/value-ada-1.json"

hprop_goldenValue1 :: Property
hprop_goldenValue1 =
  H.propertyOnce $ do
    let policyId = Api.PolicyId "a0000000000000000000000000000000000000000000000000000000"
        assetName = Api.AssetName "asset1"
        valueList = [(Api.AssetId policyId assetName, 1)]
        value = Text.unpack $ Api.renderValuePretty $ fromList valueList

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
