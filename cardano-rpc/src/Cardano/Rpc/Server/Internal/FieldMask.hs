{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Normal form of @google.protobuf.FieldMask@ paths, ported from
-- @addFieldPath@ and @toFieldMask@ of protobuf Java's @FieldMaskTree@.
module Cardano.Rpc.Server.Internal.FieldMask
  ( PathTree (..)
  , normalise
  , toPaths
  )
where

import RIO hiding (toList)

import Data.Map qualified as Map
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
