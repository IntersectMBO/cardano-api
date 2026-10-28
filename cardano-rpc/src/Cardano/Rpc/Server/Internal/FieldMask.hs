{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  ( FieldPath
  , FieldMaskTree (..)
  , PathTree (..)
  , applyFieldMask
  , FieldFill
  , runFill
  , runFillPure
  , fill
  , fillSub
  , fillEach
  , fillShared
  , fillFrom
  , isSelected
  , lensLabel
  , normalise
  , resolve
  , subMask
  , subMaskEach
  , toPaths
  )
where

import RIO hiding (toList)

import Data.Char (isUpper)
import Data.List (find)
import Data.Map qualified as Map
import Data.Monoid (Ap (..), Endo (..))
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
resolve
  :: forall msg
   . Message msg
  => [FieldPath]
  -- ^ Field mask paths, rooted at @msg@
  -> FieldMaskTree msg
resolve paths
  | null paths || "*" `elem` paths = SelectAll
  | otherwise = Select . prune @msg $ normalise paths

-- | Keep the paths that resolve against the fields of @msg@. A branch whose
-- sub-paths all drop is dropped too.
prune
  :: forall msg
   . Message msg
  => PathTree
  -- ^ Normalised paths, rooted at @msg@
  -> PathTree
prune (PathTree children) = PathTree $ Map.mapMaybeWithKey keepField children
 where
  keepField :: Text -> PathTree -> Maybe PathTree
  keepField name child =
    keepResolved child =<< Map.lookup (T.unpack name) (fieldsByTextFormatName @msg)

  keepResolved :: PathTree -> FieldDescriptor msg -> Maybe PathTree
  keepResolved child@(PathTree grandchildren) (FieldDescriptor _ fieldType accessor)
    | null grandchildren = Just child
    | MessageField _ <- fieldType
    , hasSubfields accessor =
        nonEmptyTree $ pruneBelow fieldType child
    | otherwise = Nothing

  nonEmptyTree tree@(PathTree remaining) = if null remaining then Nothing else Just tree

  hasSubfields = \case
    MapField{} -> False
    _ -> True

  pruneBelow :: forall value. Message value => FieldTypeDescriptor value -> PathTree -> PathTree
  -- The descriptor argument only pins @value@.
  pruneBelow _ = prune @value

-- | Keep only the selected fields and reset the rest to defaults. Required and
-- unknown fields stay.
applyFieldMask
  :: forall msg
   . Message msg
  => FieldMaskTree msg
  -- ^ Selects the fields to keep
  -> msg
  -- ^ The message to trim
  -> msg
applyFieldMask SelectAll = id
applyFieldMask (Select tree) = applyTree tree

applyTree :: forall msg. Message msg => PathTree -> msg -> msg
applyTree (PathTree selected) message =
  foldl' step message . toList $ fieldsByTextFormatName @msg
 where
  step :: msg -> (String, FieldDescriptor msg) -> msg
  step current (name, FieldDescriptor _ fieldType accessor) =
    maybe (resetField accessor current) (keepSelected fieldType accessor current) $
      Map.lookup (T.pack name) selected

  keepSelected :: FieldTypeDescriptor value -> FieldAccessor msg value -> msg -> PathTree -> msg
  keepSelected fieldType accessor current subtree@(PathTree children)
    | null children = current
    | MessageField _ <- fieldType = trimField subtree accessor current
    | otherwise = current

-- | Reset a field to its default. Required fields stay.
resetField
  :: FieldAccessor msg value
  -- ^ Accesses the field
  -> msg
  -- ^ The message holding the field
  -> msg
resetField accessor message = case accessor of
  PlainField Optional field -> message & field .~ fieldDefault
  PlainField Required _ -> message
  RepeatedField _ field -> message & field .~ []
  MapField _ _ field -> message & field .~ mempty
  OptionalField field -> whenActive (message ^. field) message $ message & field .~ Nothing

-- | Apply the subtree to the message or messages held by a field.
trimField
  :: Message value
  => PathTree
  -- ^ The paths below the field
  -> FieldAccessor msg value
  -- ^ Accesses the field
  -> msg
  -- ^ The message holding the field
  -> msg
trimField subtree accessor message = case accessor of
  OptionalField field -> whenActive (message ^. field) message $ message & field %~ fmap (applyTree subtree)
  PlainField _ field -> message & field %~ applyTree subtree
  RepeatedField _ field -> message & field %~ map (applyTree subtree)
  MapField{} -> message

-- | Update only when the oneof member is active: proto-lens oneof setters
-- replace the whole oneof, so touching an inactive member wipes the active one.
whenActive
  :: Maybe value
  -- ^ The oneof member, 'Nothing' when inactive
  -> msg
  -- ^ The message before the update
  -> msg
  -- ^ The message after the update
  -> msg
whenActive member message update = maybe message (const update) member

-- | Effects that fill the fields a mask selects; 'Nothing' when it selects none
-- of them. Effects run left to right.
--
-- The type, from the outside in:
--
-- * @FieldMaskTree msg ->@: each fill sees the part of the mask that applies to @msg@.
-- * @Maybe@: 'Nothing' when the mask selects none of the fill's fields, so 'runFill' never
--   calls its runner (no node session) for an empty selection.
-- * @Ap m@: the effect that obtains the selected values. 'Ap' gives @m (Endo msg)@ the
--   'Semigroup' that the 'Monoid' of 'Maybe' needs, and runs combined effects left to right.
-- * @Endo msg@: the setters for the selected fields; '(<>)' composes them and 'runFill'
--   applies them to @defMessage@.
newtype FieldFill m msg = FieldFill (FieldMaskTree msg -> Maybe (Ap m (Endo msg)))
  deriving (Semigroup, Monoid) via (FieldMaskTree msg -> Maybe (Ap m (Endo msg)))

-- | Run the effects of the fields the mask selects, then trim the message to the
-- mask. When the mask selects no field, the runner is never called.
runFill
  :: (Applicative n, Message msg)
  => (forall x. m x -> n x)
  -- ^ Runs the effects, for example in one node session
  -> FieldMaskTree msg
  -- ^ Selects the fields to fill
  -> FieldFill m msg
  -- ^ Fills the fields
  -> n msg
runFill runEffects fieldMask = fmap (applyFieldMask fieldMask) . fillMessage runEffects fieldMask

-- | 'runFill' for fills without effects.
runFillPure
  :: Message msg
  => FieldMaskTree msg
  -- ^ Selects the fields to fill
  -> FieldFill Identity msg
  -- ^ Fills the fields
  -> msg
runFillPure fieldMask = runIdentity . runFill id fieldMask

-- | Fill the field that the lens label names with the result of the action.
-- When the mask does not select the field, the action never runs and its value is never evaluated.
fill
  :: forall label msg a m
   . (HasField msg label a, KnownSymbol label, Message msg, Functor m)
  => m a
  -- ^ Obtains the value of the field
  -> FieldFill m msg
fill doValue = FieldFill $ \fieldMask -> do
  guard $ isSelected @label fieldMask
  pure . Ap $ Endo . (Field.field @label .~) <$> doValue

-- | Fill the singular message field that the lens label names from fills of its
-- own fields, against the part of the mask below it.
fillSub
  :: forall label msg sub m
   . (HasField msg label sub, KnownSymbol label, Message msg, Message sub, Applicative m)
  => FieldFill m sub
  -- ^ Fills the fields of the sub-message
  -> FieldFill m msg
fillSub subFill = FieldFill $ \fieldMask -> do
  guard $ isSelected @label fieldMask
  pure . Ap $ Endo . (Field.field @label .~) <$> fillMessage id (subMask @label fieldMask) subFill

-- | Fill the repeated message field that the lens label names: run the action for
-- the elements' inputs, then fill each element in order against the part of the mask below the field.
-- An element whose sub-mask selects nothing is still present, as 'defMessage'.
fillEach
  :: forall label msg sub input m
   . (HasField msg label [sub], KnownSymbol label, Message msg, Message sub, Monad m)
  => m [input]
  -- ^ Obtains the inputs of the elements
  -> (input -> FieldFill m sub)
  -- ^ Fills one element from its input; its effects run once per element
  -> FieldFill m msg
fillEach doInputs elementFill = FieldFill $ \fieldMask -> do
  guard $ isSelected @label fieldMask
  let doElement = fillMessage id (subMaskEach @label fieldMask) . elementFill
  pure . Ap $ doInputs >>= fmap (Endo . (Field.field @label .~)) . traverse doElement

-- | Fill several fields from the result of one action. The action runs once when
-- the mask selects any of the fields, and never otherwise.
fillShared
  :: Functor m
  => m shared
  -- ^ Obtains the result the fields share
  -> FieldFill ((->) shared) msg
  -- ^ Fills the fields from the result of the action
  -> FieldFill m msg
fillShared doShared (FieldFill sharedFill) = FieldFill $ \fieldMask ->
  Ap . (<$> doShared) . getAp <$> sharedFill fieldMask

-- | Fill several fields from one value that is already at hand, reading each
-- field with a function of it. A field the mask does not select never reads it.
fillFrom
  :: Applicative m
  => shared
  -- ^ The value the fields are read from
  -> FieldFill ((->) shared) msg
  -- ^ Reads each field from the value
  -> FieldFill m msg
fillFrom shared = fillShared $ pure shared

-- | Run the effects of the fields the mask selects and set them on 'defMessage',
-- untrimmed. When the mask selects no field, the runner is never called.
fillMessage
  :: (Applicative n, Message msg)
  => (forall x. m x -> n x)
  -- ^ Runs the effects
  -> FieldMaskTree msg
  -- ^ Selects the fields to fill
  -> FieldFill m msg
  -- ^ Fills the fields
  -> n msg
fillMessage runEffects fieldMask (FieldFill fieldFill) =
  (`appEndo` defMessage) . fold <$> traverse (runEffects . getAp) (fieldFill fieldMask)

-- | Whether the mask needs the field that the lens label names: the field or a
-- field below it is selected. A label that names no field is selected only under 'SelectAll'.
isSelected
  :: forall label msg a
   . (HasField msg label a, KnownSymbol label, Message msg)
  => FieldMaskTree msg
  -- ^ The mask, rooted at @msg@
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
  -- ^ The mask, rooted at @msg@
  -> FieldMaskTree sub
subMask = fieldSubMask $ labelledFieldName @label @msg

-- | The part of a mask below the repeated message field that the lens label
-- names, for building each element of it on its own.
subMaskEach
  :: forall label msg sub
   . (HasField msg label [sub], KnownSymbol label, Message msg)
  => FieldMaskTree msg
  -- ^ The mask, rooted at @msg@
  -> FieldMaskTree sub
subMaskEach = fieldSubMask $ labelledFieldName @label @msg

-- | The part of a mask below a top-level field; 'Nothing' names no field.
fieldSubMask
  :: Maybe Text
  -- ^ The proto name of the field
  -> FieldMaskTree msg
  -- ^ The mask, rooted at the message holding the field
  -> FieldMaskTree sub
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
lensLabel
  :: Text
  -- ^ The proto name of the field
  -> Text
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
-- Mirrors @reservedKeywords@ in proto-lens-protoc's @app/Data/ProtoLens/Compiler/Definitions.hs@.
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
