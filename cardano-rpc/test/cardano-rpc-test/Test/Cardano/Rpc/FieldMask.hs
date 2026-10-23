{-# LANGUAGE OverloadedStrings #-}

-- | Tests for field mask path normalisation. Some unit cases are ported from
-- protobuf Java's @FieldMaskTreeTest@; the rest are our own.
module Test.Cardano.Rpc.FieldMask where

import Cardano.Rpc.Server.Internal.FieldMask

import RIO

import Data.List (isPrefixOf, nub, sort)
import Data.Text qualified as T

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Port of Java's @testAddFieldPath@: each step adds one path to the tree
-- built so far and checks the resulting leaves.
hprop_field_mask_add_field_path :: Property
hprop_field_mask_add_field_path = H.propertyOnce $ do
  normalise [] === PathTree mempty
  paths [] === []
  paths [""] === []
  paths ["", "foo"] === ["foo"]
  paths ["", "foo", "foo"] === ["foo"]
  paths ["", "foo", "foo", "bar.baz"] === ["bar.baz", "foo"]
  paths ["", "foo", "foo", "bar.baz", "foo.bar"] === ["bar.baz", "foo"]
  paths ["", "foo", "foo", "bar.baz", "foo.bar", "bar.quz"] === ["bar.baz", "bar.quz", "foo"]
  paths ["", "foo", "foo", "bar.baz", "foo.bar", "bar.quz", "bar"] === ["bar", "foo"]

-- | Port of Java's @testMergeFromFieldMask@.
hprop_field_mask_merge_from_field_mask :: Property
hprop_field_mask_merge_from_field_mask = H.propertyOnce $ do
  paths ["foo", "bar.baz", "bar.quz"] === ["bar.baz", "bar.quz", "foo"]
  paths ["foo", "bar.baz", "bar.quz", "foo.bar", "bar"] === ["bar", "foo"]

hprop_field_mask_prefix_is_kept_in_any_order :: Property
hprop_field_mask_prefix_is_kept_in_any_order = H.propertyOnce $ do
  paths ["a.b.c", "a.b", "a"] === ["a"]
  paths ["a", "a.b", "a.b.c"] === ["a"]
  paths ["a.b", "a.b.c"] === ["a.b"]

-- | Port of Java's @testGetFieldPaths_excessivelyDeepTree@.
hprop_field_mask_deep_path :: Property
hprop_field_mask_deep_path = H.propertyOnce $ do
  let deepPath = T.intercalate "." $ replicate 250000 "a"
  paths [deepPath, deepPath] === [deepPath]

hprop_field_mask_empty_segment_paths_are_dropped :: Property
hprop_field_mask_empty_segment_paths_are_dropped = H.propertyOnce $ do
  paths ["", ".", "..", ".a", "a.", "a..b", "a..b.c"] === []
  paths ["a.", "a.b", ".a", "a..b"] === ["a.b"]

hprop_field_mask_star_is_an_ordinary_segment :: Property
hprop_field_mask_star_is_an_ordinary_segment = H.propertyOnce $ do
  paths ["*"] === ["*"]
  paths ["a.*", "a.b"] === ["a.*", "a.b"]
  paths ["a", "a.*"] === ["a"]

hprop_field_mask_order_independent :: Property
hprop_field_mask_order_independent = H.property $ do
  inputs <- H.forAll genPaths
  shuffled <- H.forAll $ Gen.shuffle inputs
  paths shuffled === paths inputs

hprop_field_mask_idempotent :: Property
hprop_field_mask_idempotent = H.property $ do
  inputs <- H.forAll genPaths
  paths (paths inputs) === paths inputs

hprop_field_mask_output_sorted_and_antichain :: Property
hprop_field_mask_output_sorted_and_antichain = H.property $ do
  inputs <- H.forAll genPaths
  let outputs = paths inputs
  H.assertWith outputs $ \xs -> map segments xs == sort (map segments xs)
  H.assertWith outputs $ \xs ->
    not $ or [isSegmentPrefix x y | x <- xs, y <- xs, x /= y]

hprop_field_mask_matches_minimal_elements_oracle :: Property
hprop_field_mask_matches_minimal_elements_oracle = H.property $ do
  inputs <- H.forAll genPaths
  let wellFormed = map segments $ filter (not . any T.null . segments) inputs
      minimal = filter (\x -> not $ any (\y -> y /= x && y `isPrefixOf` x) wellFormed) wellFormed
  paths inputs === map (T.intercalate ".") (sort $ nub minimal)

hprop_field_mask_output_covers_input_and_comes_from_input :: Property
hprop_field_mask_output_covers_input_and_comes_from_input = H.property $ do
  inputs <- H.forAll genPaths
  let outputs = paths inputs
      wellFormed = filter (not . any T.null . segments) inputs
  H.assertWith (outputs, wellFormed) $ \(os, ins) ->
    all (\input -> any (`isSegmentPrefix` input) os) ins
  H.assertWith (outputs, inputs) $ \(os, ins) -> all (`elem` ins) os

paths :: [Text] -> [Text]
paths = toPaths . normalise

segments :: Text -> [Text]
segments = T.splitOn "."

isSegmentPrefix :: Text -> Text -> Bool
isSegmentPrefix prefix path = segments prefix `isPrefixOf` segments path

-- | Dotted paths of up to four segments over a tiny alphabet, so prefixes
-- collide often. Segments are occasionally empty to exercise the dropping.
genPaths :: Gen [Text]
genPaths = Gen.list (Range.linear 0 12) genPath

genPath :: Gen Text
genPath = T.intercalate "." <$> Gen.list (Range.linear 1 4) genSegment

genSegment :: Gen Text
genSegment = Gen.frequency [(1, pure ""), (12, Gen.element ["a", "b", "c", "a-"])]
