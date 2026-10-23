{-# LANGUAGE OverloadedStrings #-}

-- | Normal form of @google.protobuf.FieldMask@ paths, ported from
-- @addFieldPath@ and @toFieldMask@ of protobuf Java's @FieldMaskTree@.
module Cardano.Rpc.Server.Internal.FieldMask
  ( FieldPath
  , PathTree (..)
  , normalise
  , toPaths
  )
where

import RIO hiding (toList)

import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Exts (IsList (toList))

-- | A dot-separated field mask path, as in @google.protobuf.FieldMask.paths@ (e.g. @values.cardano@).
type FieldPath = Text

-- | Normalised field mask paths. An empty map at a non-root node selects that
-- whole field.
newtype PathTree = PathTree (Map Text PathTree)
  deriving (Eq, Show)

-- | Build a tree from dotted paths; a path below another is dropped, in any order.
-- Paths with an empty segment are dropped too (Java keeps some).
normalise
  :: [FieldPath]
  -- ^ Dotted field mask paths
  -> PathTree
normalise = foldl' addPath (PathTree mempty)
 where
  addPath tree path
    | any T.null segments = tree
    | otherwise = addSegments segments tree
   where
    segments = T.splitOn "." path

-- | The leaves of the tree as dotted paths, sorted ascending. A tree without
-- children has no paths.
toPaths
  :: PathTree
  -- ^ Normalised paths
  -> [FieldPath]
toPaths (PathTree children) = foldMap (\(name, child) -> go [name] child) $ toList children
 where
  go reversedSegments (PathTree subtree)
    | null subtree = [T.intercalate "." $ reverse reversedSegments]
    | otherwise = foldMap (\(name, child) -> go (name : reversedSegments) child) $ toList subtree

-- | Add a path below a node that is the root or has children.
addSegments
  :: [Text]
  -- ^ Segments of the path below the node
  -> PathTree
  -- ^ The node
  -> PathTree
addSegments [] _ = PathTree mempty
addSegments (segment : rest) (PathTree children) =
  PathTree $ Map.alter (Just . maybe (addSegments rest $ PathTree mempty) extend) segment children
 where
  -- A leaf already selects every path below it.
  extend child@(PathTree grandchildren)
    | null grandchildren = child
    | otherwise = addSegments rest child
