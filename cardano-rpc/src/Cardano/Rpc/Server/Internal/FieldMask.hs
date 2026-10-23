{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Normal form of @google.protobuf.FieldMask@ paths, ported from
-- @addFieldPath@ and @toFieldMask@ of protobuf Java's @FieldMaskTree@.
module Cardano.Rpc.Server.Internal.FieldMask
  ( FieldMaskTree (..)
  , PathTree (..)
  , normalise
  , resolve
  , toPaths
  )
where

import RIO hiding (toList)

import Data.Map qualified as Map
import Data.ProtoLens.Message
  ( FieldAccessor (..)
  , FieldDescriptor (..)
  , FieldTypeDescriptor (..)
  , Message (..)
  )
import Data.Text qualified as T
import GHC.Exts (IsList (toList))

-- | Normalised field mask paths. An empty map at a non-root node selects that
-- whole field.
newtype PathTree = PathTree (Map Text PathTree)
  deriving (Eq, Show)

-- | Build a tree from dotted paths; a path below another is dropped, in any order.
-- Paths with an empty segment are dropped too (Java keeps some).
normalise :: [Text] -> PathTree
normalise = foldl' addPath (PathTree mempty)
 where
  addPath tree path
    | any T.null segments = tree
    | otherwise = addSegments segments tree
   where
    segments = T.splitOn "." path

-- | The leaves of the tree as dotted paths, sorted ascending. A tree without
-- children has no paths.
toPaths :: PathTree -> [Text]
toPaths (PathTree children) = foldMap (\(name, child) -> go [name] child) $ toList children
 where
  go reversedSegments (PathTree subtree)
    | null subtree = [T.intercalate "." $ reverse reversedSegments]
    | otherwise = foldMap (\(name, child) -> go (name : reversedSegments) child) $ toList subtree

-- | Add a path below a node that is the root or has children.
addSegments :: [Text] -> PathTree -> PathTree
addSegments [] _ = PathTree mempty
addSegments (segment : rest) (PathTree children) = PathTree $ Map.alter step segment children
 where
  step = \case
    Nothing -> Just $ addSegments rest (PathTree mempty)
    Just child@(PathTree grandchildren)
      | null grandchildren -> Just child
      | otherwise -> Just $ addSegments rest child

-- | A field mask resolved against the descriptors of @msg@.
data FieldMaskTree msg
  = -- | An absent or empty mask, or a @*@ path.
    SelectAll
  | -- | The paths that resolved. May be empty, which selects nothing.
    Select PathTree
  deriving (Eq, Show)

type role FieldMaskTree nominal

-- | Resolve field mask paths, rooted at @msg@, against its proto descriptors.
-- A path that does not name a field is dropped whole.
resolve :: forall msg. Message msg => [Text] -> FieldMaskTree msg
resolve paths
  | null paths || "*" `elem` paths = SelectAll
  | otherwise = Select . prune @msg $ normalise paths

-- | Keep the paths that resolve against the fields of @msg@. A branch whose
-- sub-paths all drop is dropped too.
prune :: forall msg. Message msg => PathTree -> PathTree
prune (PathTree children) = PathTree $ Map.mapMaybeWithKey keepField children
 where
  keepField name child@(PathTree grandchildren) =
    case Map.lookup (T.unpack name) $ fieldsByTextFormatName @msg of
      Nothing -> Nothing
      Just (FieldDescriptor _ fieldType accessor)
        | null grandchildren -> Just child
        | MessageField _ <- fieldType
        , hasSubfields accessor ->
            nonEmptyTree $ pruneBelow fieldType child
        | otherwise -> Nothing

  nonEmptyTree tree@(PathTree remaining) = if null remaining then Nothing else Just tree

  hasSubfields = \case
    MapField{} -> False
    _ -> True

  pruneBelow :: forall value. Message value => FieldTypeDescriptor value -> PathTree -> PathTree
  -- The descriptor argument only pins @value@.
  pruneBelow _ = prune @value
