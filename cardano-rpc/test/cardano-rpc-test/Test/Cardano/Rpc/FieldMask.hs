{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for field mask normalisation and resolution. Some unit cases are
-- ported from protobuf Java's @FieldMaskTreeTest@; the rest are our own.
module Test.Cardano.Rpc.FieldMask where

import Cardano.Rpc.Proto.Api.UtxoRpc.Query
  ( ReadGenesisResponse
  , ReadParamsResponse
  , ReadUtxosResponse
  )
import Cardano.Rpc.Server.Internal.FieldMask

import RIO

import Data.List (isPrefixOf, nub, sort)
import Data.ProtoLens.Message (Message)
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

hprop_field_mask_resolve_select_all :: Property
hprop_field_mask_resolve_select_all = H.propertyOnce $ do
  resolve @ReadParamsResponse [] === SelectAll
  resolve @ReadParamsResponse ["*"] === SelectAll
  resolve @ReadParamsResponse ["ledger_tip", "*"] === SelectAll

hprop_field_mask_resolve_keeps_resolving_paths :: Property
hprop_field_mask_resolve_keeps_resolving_paths = H.propertyOnce $ do
  resolved @ReadParamsResponse ["ledger_tip"] === ["ledger_tip"]
  resolved @ReadParamsResponse ["values.cardano.max_tx_size"] === ["values.cardano.max_tx_size"]
  resolved @ReadParamsResponse ["values.cardano.pool_influence.numerator"]
    === ["values.cardano.pool_influence.numerator"]

hprop_field_mask_resolve_drops_unknown_paths :: Property
hprop_field_mask_resolve_drops_unknown_paths = H.propertyOnce $ do
  resolve @ReadParamsResponse ["zzz"] === selectsNothing
  resolve @ReadParamsResponse ["values.zzz"] === selectsNothing
  resolve @ReadParamsResponse ["values.cardano.zzz"] === selectsNothing
  resolve @ReadParamsResponse ["a..b"] === selectsNothing
  resolved @ReadParamsResponse ["ledger_tip", "zzz"] === ["ledger_tip"]
  resolved @ReadParamsResponse ["ledger_tip", "values.zzz"] === ["ledger_tip"]
  resolved @ReadParamsResponse ["values.cardano.max_tx_size", "values.cardano.zzz"]
    === ["values.cardano.max_tx_size"]
  resolve @ReadParamsResponse ["values.*"] === selectsNothing
  resolved @ReadParamsResponse ["values.*", "ledger_tip"] === ["ledger_tip"]

hprop_field_mask_resolve_keeps_oneof_member :: Property
hprop_field_mask_resolve_keeps_oneof_member =
  H.propertyOnce $
    resolved @ReadParamsResponse ["values.cardano"] === ["values.cardano"]

hprop_field_mask_resolve_drops_paths_below_scalars :: Property
hprop_field_mask_resolve_drops_paths_below_scalars = H.propertyOnce $ do
  resolved @ReadParamsResponse ["ledger_tip.slot.x"] === []
  resolved @ReadParamsResponse ["ledger_tip.slot", "ledger_tip.slot.x"] === ["ledger_tip.slot"]

hprop_field_mask_resolve_repeated_scalar_is_a_leaf :: Property
hprop_field_mask_resolve_repeated_scalar_is_a_leaf = H.propertyOnce $ do
  let costModel = "values.cardano.cost_models.plutus_v1.values"
  resolved @ReadParamsResponse [costModel] === [costModel]
  resolved @ReadParamsResponse [costModel <> ".x"] === []

hprop_field_mask_resolve_through_repeated_field :: Property
hprop_field_mask_resolve_through_repeated_field = H.propertyOnce $ do
  resolved @ReadUtxosResponse ["items.txo_ref.hash"] === ["items.txo_ref.hash"]
  resolved @ReadUtxosResponse ["items", "ledger_tip.slot"] === ["items", "ledger_tip.slot"]
  resolved @ReadUtxosResponse ["items.zzz"] === []

hprop_field_mask_resolve_map_field_is_a_leaf :: Property
hprop_field_mask_resolve_map_field_is_a_leaf = H.propertyOnce $ do
  resolved @ReadGenesisResponse ["cardano.avvm_distr"] === ["cardano.avvm_distr"]
  resolved @ReadGenesisResponse ["cardano.avvm_distr.key"] === []

-- | Valid paths follow a hand-written subset of the 'ReadParamsResponse' field
-- tree, optionally with one extra segment that names no field.
hprop_field_mask_resolve_matches_field_tree_oracle :: Property
hprop_field_mask_resolve_matches_field_tree_oracle = H.property $ do
  inputs <- H.forAll $ Gen.list (Range.linear 1 8) genTreePath
  resolve @ReadParamsResponse inputs
    === Select (normalise $ filter (isValidPath readParamsShape . segments) inputs)

hprop_field_mask_resolve_result_comes_from_input :: Property
hprop_field_mask_resolve_result_comes_from_input = H.property $ do
  inputs <- H.forAll genResolvePaths
  case resolve @ReadParamsResponse inputs of
    SelectAll -> H.success
    Select tree ->
      H.assertWith (toPaths tree, paths inputs) $ \(os, ins) -> all (`elem` ins) os

hprop_field_mask_resolve_idempotent :: Property
hprop_field_mask_resolve_idempotent = H.property $ do
  inputs <- H.forAll genResolvePaths
  case resolve @ReadParamsResponse inputs of
    SelectAll -> H.success
    Select tree -> do
      -- No paths would read back as an absent mask, which selects everything.
      let resolvedPaths = toPaths tree
      unless (null resolvedPaths) $
        resolve @ReadParamsResponse resolvedPaths === Select tree

-- | Resolving against nothing that exists keeps no paths, unlike 'SelectAll'.
selectsNothing :: FieldMaskTree msg
selectsNothing = Select $ PathTree mempty

-- | A field's type in a hand-written description of a message: scalars and
-- unlisted message fields have no children.
newtype Shape = Shape [(Text, Shape)]

readParamsShape :: Shape
readParamsShape =
  Shape
    [ ("ledger_tip", Shape [(name, leaf) | name <- ["slot", "hash", "height", "timestamp"]])
    ,
      ( "values"
      , Shape
          [
            ( "cardano"
            , Shape
                [ ("max_tx_size", leaf)
                , ("pool_influence", rational)
                , ("protocol_version", Shape [("major", leaf), ("minor", leaf)])
                , ("prices", Shape [("steps", rational), ("memory", rational)])
                , ("cost_models", Shape [("plutus_v1", Shape [("values", leaf)])])
                ]
            )
          ]
      )
    ]
 where
  leaf = Shape []
  rational = Shape [("numerator", leaf), ("denominator", leaf)]

-- | Whether the segments follow the description from the root.
isValidPath :: Shape -> [Text] -> Bool
isValidPath _ [] = True
isValidPath (Shape fields) (segment : rest) = case lookup segment fields of
  Nothing -> False
  Just child@(Shape grandchildren) -> null rest || (not (null grandchildren) && isValidPath child rest)

-- | A prefix of a path through the description, sometimes followed by a segment
-- that names no field, which may sit below a scalar.
genTreePath :: Gen Text
genTreePath = do
  depth <- Gen.int (Range.linear 1 4)
  walked <- walk depth readParamsShape
  junk <- Gen.element [[], [], ["zzz"], ["x"]]
  pure . T.intercalate "." $ walked <> junk
 where
  walk :: Int -> Shape -> Gen [Text]
  walk 0 _ = pure []
  walk _ (Shape []) = pure []
  walk remaining (Shape fields) = do
    (name, child) <- Gen.element fields
    (name :) <$> walk (remaining - 1) child

-- | The paths a mask resolves to, sorted; a placeholder for 'SelectAll'.
resolved :: forall msg. Message msg => [Text] -> [Text]
resolved input = case resolve @msg input of
  SelectAll -> ["<all>"]
  Select tree -> toPaths tree

-- | Dotted paths whose segments are real field names at that depth or junk.
genResolvePaths :: Gen [Text]
genResolvePaths = Gen.list (Range.linear 0 8) $ do
  depths <- Gen.list (Range.linear 1 4) (pure ())
  T.intercalate "." <$> traverse genSegmentAt (zip [0 :: Int ..] depths)
 where
  genSegmentAt (depth, ()) =
    Gen.frequency
      [ (1, Gen.element ["zzz", "x", ""])
      , (5, Gen.element $ realSegments depth)
      ]
  realSegments = \case
    0 -> ["values", "ledger_tip"]
    1 -> ["cardano", "slot", "hash", "height"]
    2 -> ["max_tx_size", "pool_influence", "cost_models", "protocol_version"]
    _ -> ["numerator", "denominator", "major", "plutus_v1"]

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
