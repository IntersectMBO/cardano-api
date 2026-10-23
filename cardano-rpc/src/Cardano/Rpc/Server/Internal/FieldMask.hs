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
  , applyFieldMask
  , normalise
  , resolve
  , toPaths
  )
where

import RIO hiding (toList)

import Data.Map qualified as Map
import Data.ProtoLens.Message
  ( FieldAccessor (..)
  , FieldDefault (..)
  , FieldDescriptor (..)
  , FieldTypeDescriptor (..)
  , Message (..)
  , WireDefault (..)
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

-- | Keep only the selected fields and reset the rest to defaults. Required and
-- unknown fields stay.
applyFieldMask :: forall msg. Message msg => FieldMaskTree msg -> msg -> msg
applyFieldMask SelectAll = id
applyFieldMask (Select tree) = applyTree tree

applyTree :: forall msg. Message msg => PathTree -> msg -> msg
applyTree (PathTree selected) message =
  foldl' step message . toList $ fieldsByTextFormatName @msg
 where
  step current (name, FieldDescriptor _ fieldType accessor) =
    case Map.lookup (T.pack name) selected of
      Nothing -> resetField accessor current
      Just subtree@(PathTree children)
        | null children -> current
        | MessageField _ <- fieldType -> trimField subtree accessor current
        | otherwise -> current

-- | Reset a field to its default. Required fields stay.
resetField :: FieldAccessor msg value -> msg -> msg
resetField accessor message = case accessor of
  PlainField Optional field -> message & field .~ fieldDefault
  PlainField Required _ -> message
  RepeatedField _ field -> message & field .~ []
  MapField _ _ field -> message & field .~ mempty
  OptionalField field -> whenActive (message ^. field) message $ message & field .~ Nothing

-- | Apply the subtree to the message or messages held by a field.
trimField :: Message value => PathTree -> FieldAccessor msg value -> msg -> msg
trimField subtree accessor message = case accessor of
  OptionalField field -> whenActive (message ^. field) message $ message & field %~ fmap (applyTree subtree)
  PlainField _ field -> message & field %~ applyTree subtree
  RepeatedField _ field -> message & field %~ map (applyTree subtree)
  MapField{} -> message

-- | Update only when the oneof member is active: proto-lens oneof setters
-- replace the whole oneof, so touching an inactive member wipes the active one.
whenActive :: Maybe value -> msg -> msg -> msg
whenActive member message update = maybe message (const update) member
