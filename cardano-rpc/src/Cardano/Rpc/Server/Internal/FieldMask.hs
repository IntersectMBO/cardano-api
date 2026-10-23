{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Normal form of @google.protobuf.FieldMask@ paths, ported from
-- @addFieldPath@ and @toFieldMask@ of protobuf Java's @FieldMaskTree@.
--
-- Paths start at the response message. A path may continue through repeated
-- message fields and then applies to each element. Paths that do not resolve
-- are dropped whole. An absent or empty mask, or a @*@ path, selects everything.
-- A mask whose paths all drop selects nothing.
module Cardano.Rpc.Server.Internal.FieldMask
  ( FieldMaskTree (..)
  , PathTree (..)
  , applyFieldMask
  , isSelected
  , lensLabel
  , maskedSet
  , normalise
  , resolve
  , subMask
  , subMaskEach
  , toPaths
  , whenSelected
  )
where

import RIO hiding (toList)

import Data.Char (isUpper)
import Data.List (find)
import Data.Map qualified as Map
import Data.ProtoLens.Field (HasField)
import Data.ProtoLens.Field qualified as Field (field)
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
import GHC.TypeLits (KnownSymbol, symbolVal)

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

-- | Set the field that the proto-lens lens label names only when the mask needs
-- it, so an unselected value is never evaluated.
maskedSet
  :: forall label msg a
   . (HasField msg label a, KnownSymbol label, Message msg)
  => FieldMaskTree msg
  -> a
  -> msg
  -> msg
maskedSet fieldMask value
  | isSelected @label fieldMask = Field.field @label .~ value
  | otherwise = id

-- | Run an action only when the mask needs the field that the lens label names.
whenSelected
  :: forall label msg a f b
   . (HasField msg label a, KnownSymbol label, Message msg, Applicative f)
  => FieldMaskTree msg
  -> f b
  -> f (Maybe b)
whenSelected fieldMask action
  | isSelected @label fieldMask = Just <$> action
  | otherwise = pure Nothing

-- | Whether the mask needs the field that the lens label names: the field or a
-- field below it is selected. A label that names no field is selected only under 'SelectAll'.
isSelected
  :: forall label msg a
   . (HasField msg label a, KnownSymbol label, Message msg)
  => FieldMaskTree msg
  -> Bool
isSelected fieldMask = case subMask @label fieldMask of
  SelectAll -> True
  Select (PathTree children) -> not $ null children

-- | The part of a mask below the singular message field that the lens label
-- names, for building that field on its own.
subMask
  :: forall label msg sub
   . (HasField msg label sub, KnownSymbol label, Message msg)
  => FieldMaskTree msg
  -> FieldMaskTree sub
subMask = fieldSubMask $ labelledFieldName @label @msg

-- | The part of a mask below the repeated message field that the lens label
-- names, for building each element of it on its own.
subMaskEach
  :: forall label msg sub
   . (HasField msg label [sub], KnownSymbol label, Message msg)
  => FieldMaskTree msg
  -> FieldMaskTree sub
subMaskEach = fieldSubMask $ labelledFieldName @label @msg

-- | The part of a mask below a top-level field; 'Nothing' names no field.
fieldSubMask :: Maybe Text -> FieldMaskTree msg -> FieldMaskTree sub
fieldSubMask _ SelectAll = SelectAll
fieldSubMask name (Select (PathTree roots)) =
  maybe (Select $ PathTree mempty) wholeOrPart $ (`Map.lookup` roots) =<< name
 where
  -- A field selected without sub-paths is selected whole.
  wholeOrPart (PathTree children)
    | null children = SelectAll
    | otherwise = Select $ PathTree children

-- | The proto name of the field whose 'lensLabel' is the label. A @maybe'@ label
-- names the same field as the plain one.
labelledFieldName
  :: forall label msg a
   . (HasField msg label a, KnownSymbol label, Message msg)
  => Maybe Text
labelledFieldName =
  find ((== label) . lensLabel) . map T.pack . Map.keys $ fieldsByTextFormatName @msg
 where
  symbol = T.pack $ symbolVal (Proxy @label)
  label = fromMaybe symbol $ T.stripPrefix "maybe'" symbol
  -- Uses the 'HasField' constraint, which rejects labels that name no lens of @msg@.
  _lens = Field.field @label @msg @a @Identity

-- | The label proto-lens gives the lenses of a proto field, for example
-- @ledger_tip@ to @ledgerTip@ or @plutus_v1@ to @plutusV1@.
lensLabel :: Text -> Text
lensLabel name
  | camelCased `elem` reservedKeywords = camelCased <> "'"
  | otherwise = camelCased
 where
  (underscores, rest) = T.span (== '_') name
  camelCased =
    underscores <> T.concat (zipWith ($) (lowerInitials : repeat capitalise) $ T.splitOn "_" rest)
  lowerInitials word = T.toLower (T.takeWhile isUpper word) <> T.dropWhile isUpper word
  capitalise word = T.toUpper (T.take 1 word) <> T.drop 1 word

-- | Haskell keywords, which proto-lens suffixes with @'@ in lens labels.
reservedKeywords :: [Text]
reservedKeywords =
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "forall"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "type"
  , "where"
  , "mdo"
  , "rec"
  , "pattern"
  , "proc"
  ]
