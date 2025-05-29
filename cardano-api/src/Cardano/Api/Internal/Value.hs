{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Currency values
module Cardano.Api.Internal.Value
  ( L.Coin (..)

    -- * Multi-asset values
  , Quantity (..)
  , parseQuantity
  , PolicyId (..)
  , parsePolicyId
  , scriptPolicyId
  , AssetName (..)
  , parseAssetName
  , AssetId (..)
  , parseAssetId
  , Value
  , selectAsset
  , valueFromList
  , valueToList
  , filterValue
  , allPositive
  , negateValue
  , negateLedgerValue
  , calcMinimumDeposit
  , PolicyAssets (..)
  , policyAssetsToValue
  , valueToPolicyAssets
  , multiAssetToPolicyAssets

    -- ** Ada \/ L.Coin specifically
  , Lovelace
  , quantityToLovelace
  , lovelaceToQuantity
  , selectLovelace
  , lovelaceToValue
  , valueToLovelace

    -- ** Alternative nested representation
  , ValueNestedRep (..)
  , ValueNestedBundle (..)
  , valueToNestedRep
  , valueFromNestedRep

    -- ** Rendering
  , renderValue
  , renderValuePretty
  , renderMultiAsset
  , renderMultiAssetPretty

    -- * Internal conversion functions
  , toByronLovelace
  , fromByronLovelace
  , fromShelleyDeltaLovelace
  , toMaryValue
  , fromMaryValue
  , fromMultiAsset
  , fromLedgerValue
  , toLedgerValue

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Internal.Eon.MaryEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Case
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Script
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseUsing
import Cardano.Api.Ledger.Lens qualified as A
import Cardano.Api.Parser.Text (($>), (<?>), (<|>))
import Cardano.Api.Parser.Text qualified as P

import Cardano.Chain.Common qualified as Byron
import Cardano.Ledger.Allegra.Core qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Mary.TxOut as Mary (scaledMinDeposit)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Mary.Value qualified as Mary

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, object, parseJSON, toJSON, withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.Types (ToJSONKey)
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as Short
import Data.Data (Data)
import Data.Function ((&))
import Data.Group (invert)
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MonoTraversable
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsList (..))
import Lens.Micro ((%~))

toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (L.Coin x) =
  case Byron.integerToLovelace x of
    Left _ -> Nothing
    Right x' -> Just x'

fromByronLovelace :: Byron.Lovelace -> Lovelace
fromByronLovelace = L.Coin . Byron.lovelaceToInteger

fromShelleyDeltaLovelace :: L.DeltaCoin -> Lovelace
fromShelleyDeltaLovelace (L.DeltaCoin d) = L.Coin d

-- ----------------------------------------------------------------------------
-- Multi asset Value
--

newtype Quantity = Quantity Integer
  deriving stock Data
  deriving newtype (Eq, Ord, Num, Show, ToJSON, FromJSON)

-- | A 'Coin' is a Lovelace.
type Lovelace = L.Coin

instance Semigroup Quantity where
  Quantity a <> Quantity b = Quantity (a + b)

instance Monoid Quantity where
  mempty = Quantity 0

-- | Quantity (word64) parser. Only accepts positive quantities.
parseQuantity :: P.Parser Quantity
parseQuantity = Quantity <$> P.parseWord64

lovelaceToQuantity :: Lovelace -> Quantity
lovelaceToQuantity (L.Coin x) = Quantity x

quantityToLovelace :: Quantity -> Lovelace
quantityToLovelace (Quantity x) = L.Coin x

newtype PolicyId = PolicyId {unPolicyId :: ScriptHash}
  deriving stock (Eq, Ord)
  deriving (Show, ToJSON, FromJSON) via UsingRawBytesHex PolicyId

instance HasTypeProxy PolicyId where
  data AsType PolicyId = AsPolicyId
  proxyToAsType _ = AsPolicyId

instance SerialiseAsRawBytes PolicyId where
  serialiseToRawBytes (PolicyId sh) = serialiseToRawBytes sh
  deserialiseFromRawBytes AsPolicyId bs =
    PolicyId <$> deserialiseFromRawBytes AsScriptHash bs

-- | Policy ID parser.
parsePolicyId :: P.Parser PolicyId
parsePolicyId = PolicyId <$> parseScriptHash

scriptPolicyId :: Script lang -> PolicyId
scriptPolicyId = PolicyId . hashScript

newtype AssetName = UnsafeAssetName ByteString
  deriving stock (Eq, Ord)
  deriving newtype Show
  deriving
    (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via UsingRawBytesHex AssetName

-- | Asset name parser.
parseAssetName :: P.Parser AssetName
parseAssetName = parseRawBytesHex

instance HasTypeProxy AssetName where
  data AsType AssetName = AsAssetName
  proxyToAsType _ = AsAssetName

instance SerialiseAsRawBytes AssetName where
  serialiseToRawBytes (UnsafeAssetName bs) = bs
  deserialiseFromRawBytes AsAssetName bs
    | BS.length bs <= 32 = Right (UnsafeAssetName bs)
    | otherwise =
        Left $
          SerialiseAsRawBytesError $
            "Unable to deserialise AssetName (the bytestring should be no longer than 32 bytes long "
              <> "which corresponds to a hex representation of 64 characters)"

data AssetId
  = AdaAssetId
  | AssetId !PolicyId !AssetName
  deriving (Eq, Ord, Show)

newtype Value = Value (Map AssetId Quantity)
  deriving Eq

instance Show Value where
  showsPrec d v =
    showParen (d > 10) $
      showString "valueFromList " . shows (toList v)

instance Semigroup Value where
  Value a <> Value b = Value (mergeAssetMaps a b)

instance Monoid Value where
  mempty = Value Map.empty

instance IsList Value where
  type Item Value = (AssetId, Quantity)
  fromList =
    Value
      . Map.filter (/= 0)
      . Map.fromListWith (<>)
  toList (Value m) = toList m

-- | Asset ID parser.
parseAssetId :: P.Parser AssetId
parseAssetId =
  P.try parseAdaAssetId
    <|> parseNonAdaAssetId
    <?> "asset ID"
 where
  -- Parse the ADA asset ID.
  parseAdaAssetId :: P.Parser AssetId
  parseAdaAssetId = P.string "lovelace" $> AdaAssetId

  -- Parse a multi-asset ID.
  parseNonAdaAssetId :: P.Parser AssetId
  parseNonAdaAssetId = do
    polId <- parsePolicyId
    parseFullAssetId polId <|> parseAssetIdNoAssetName polId

  -- Parse a fully specified multi-asset ID with both a policy ID and asset
  -- name.
  parseFullAssetId :: PolicyId -> P.Parser AssetId
  parseFullAssetId polId = do
    _ <- P.char '.'
    aName <- parseAssetName <?> "hexadecimal asset name"
    pure (AssetId polId aName)

  -- Parse a multi-asset ID that specifies a policy ID, but no asset name.
  parseAssetIdNoAssetName :: PolicyId -> P.Parser AssetId
  parseAssetIdNoAssetName polId = pure $ AssetId polId (UnsafeAssetName "")

{-# NOINLINE mergeAssetMaps #-} -- as per advice in Data.Map.Merge docs
mergeAssetMaps
  :: Map AssetId Quantity
  -> Map AssetId Quantity
  -> Map AssetId Quantity
mergeAssetMaps =
  Map.merge
    Map.preserveMissing
    Map.preserveMissing
    (Map.zipWithMaybeMatched mergeQuantity)
 where
  mergeQuantity :: AssetId -> Quantity -> Quantity -> Maybe Quantity
  mergeQuantity _k a b =
    case a <> b of
      Quantity 0 -> Nothing
      c -> Just c

instance ToJSON Value where
  toJSON = toJSON . valueToNestedRep

instance FromJSON Value where
  parseJSON v = valueFromNestedRep <$> parseJSON v

selectAsset :: Value -> (AssetId -> Quantity)
selectAsset (Value m) a = Map.findWithDefault mempty a m

{-# DEPRECATED valueFromList "Use 'fromList' instead." #-}
valueFromList :: [(AssetId, Quantity)] -> Value
valueFromList = fromList

{-# DEPRECATED valueToList "Use 'toList' instead." #-}
valueToList :: Value -> [(AssetId, Quantity)]
valueToList = toList

-- | Check if the 'Value' consists of /only/ positive quantities.
allPositive :: Value -> Bool
allPositive (Value m) = all (>= 0) (Map.elems m)

-- | This lets you write @a - b@ as @a <> negateValue b@.
negateValue :: Value -> Value
negateValue (Value m) = Value (Map.map negate m)

negateLedgerValue
  :: ShelleyBasedEra era -> L.Value (ShelleyLedgerEra era) -> L.Value (ShelleyLedgerEra era)
negateLedgerValue sbe v =
  caseShelleyToAllegraOrMaryEraOnwards
    (\_ -> v & A.adaAssetL sbe %~ L.Coin . negate . L.unCoin)
    (\w -> v & A.multiAssetL w %~ invert)
    sbe

filterValue :: (AssetId -> Bool) -> Value -> Value
filterValue p (Value m) = Value (Map.filterWithKey (\k _v -> p k) m)

selectLovelace :: Value -> Lovelace
selectLovelace = quantityToLovelace . flip selectAsset AdaAssetId

lovelaceToValue :: Lovelace -> Value
lovelaceToValue = Value . Map.singleton AdaAssetId . lovelaceToQuantity

-- | Check if the 'Value' consists of /only/ 'Lovelace' and no other assets,
-- and if so then return the Lovelace
--
-- See also 'selectLovelace' to select the Lovelace quantity from the Value,
-- ignoring other assets.
valueToLovelace :: Value -> Maybe Lovelace
valueToLovelace v =
  case valueToList v of
    [] -> Just (L.Coin 0)
    [(AdaAssetId, q)] -> Just (quantityToLovelace q)
    _ -> Nothing

toMaryValue :: Value -> MaryValue
toMaryValue v =
  Mary.valueFromList (L.Coin lovelace) other
 where
  Quantity lovelace = selectAsset v AdaAssetId
  other =
    [ (toMaryPolicyID pid, toMaryAssetName name, q)
    | (AssetId pid name, Quantity q) <- valueToList v
    ]

  toMaryPolicyID :: PolicyId -> Mary.PolicyID
  toMaryPolicyID (PolicyId sh) = Mary.PolicyID (toShelleyScriptHash sh)

  toMaryAssetName :: AssetName -> Mary.AssetName
  toMaryAssetName (UnsafeAssetName n) = Mary.AssetName $ Short.toShort n

toLedgerValue :: MaryEraOnwards era -> Value -> L.Value (ShelleyLedgerEra era)
toLedgerValue w = maryEraOnwardsConstraints w toMaryValue

fromLedgerValue :: ShelleyBasedEra era -> L.Value (ShelleyLedgerEra era) -> Value
fromLedgerValue sbe v =
  caseShelleyToAllegraOrMaryEraOnwards
    (const (lovelaceToValue v))
    (const (fromMaryValue v))
    sbe

fromMaryValue :: MaryValue -> Value
fromMaryValue (MaryValue (L.Coin lovelace) multiAsset) =
  Value
    -- TODO: write QC tests to show it's ok to use Map.fromAscList here
    (fromList [(AdaAssetId, Quantity lovelace) | lovelace /= 0])
    <> fromMultiAsset multiAsset

fromMultiAsset :: L.MultiAsset -> Value
fromMultiAsset multiAsset =
  fromList
    [ (AssetId (fromMaryPolicyID pid) (fromMaryAssetName name), Quantity q)
    | (pid, name, q) <- Mary.flattenMultiAsset multiAsset
    ]

fromMaryPolicyID :: Mary.PolicyID -> PolicyId
fromMaryPolicyID (Mary.PolicyID sh) = PolicyId (fromShelleyScriptHash sh)

fromMaryAssetName :: Mary.AssetName -> AssetName
fromMaryAssetName (Mary.AssetName n) = UnsafeAssetName $ Short.fromShort n

-- | Calculate cost of making a UTxO entry for a given 'Value' and
-- mininimum UTxO value derived from the 'ProtocolParameters'
calcMinimumDeposit :: Value -> Lovelace -> Lovelace
calcMinimumDeposit v =
  Mary.scaledMinDeposit (toMaryValue v)

-- | Map of non-ADA assets with their quantity, for a single policy
newtype PolicyAssets = PolicyAssets (Map AssetName Quantity)
  deriving Eq

type instance Element PolicyAssets = Quantity

instance MonoFunctor PolicyAssets where
  omap f (PolicyAssets as) = PolicyAssets $ f <$> as

instance Semigroup PolicyAssets where
  PolicyAssets a <> PolicyAssets b = PolicyAssets $ Map.unionWith (<>) a b

instance Monoid PolicyAssets where
  mempty = PolicyAssets Map.empty

instance IsList PolicyAssets where
  type Item PolicyAssets = (AssetName, Quantity)
  fromList =
    PolicyAssets
      . Map.filter (/= 0)
      . Map.fromListWith (<>)
  toList (PolicyAssets m) = toList m

instance Show PolicyAssets where
  showsPrec d v =
    showParen (d > 10) $
      showString "policyAssetsFromList " . shows (toList v)

policyAssetsToValue :: PolicyId -> PolicyAssets -> Value
policyAssetsToValue policyId = fromList . map (first (AssetId policyId)) . toList

-- | Converts 'Value' to 'PolicyAssets'. Discards any ADA value stored in 'Value'.
valueToPolicyAssets :: Value -> Map PolicyId PolicyAssets
valueToPolicyAssets v =
  Map.fromListWith
    (<>)
    [ (policyId, fromList [(assetName, q)])
    | (AssetId policyId assetName, q) <- toList v
    ]

-- | Convert cardano-ledger's 'L.MultiAsset' to a map of 'PolicyAssets'
multiAssetToPolicyAssets
  :: L.MultiAsset
  -> Map PolicyId PolicyAssets
multiAssetToPolicyAssets (L.MultiAsset mAssets) =
  fromList
    [ (fromMaryPolicyID ledgerPolicyId, PolicyAssets assetsQs)
    | (ledgerPolicyId, ledgerAssetsQs) <- toList mAssets
    , let assetsQs =
            Map.filter (/= 0)
              . fromList
              . map (bimap fromMaryAssetName Quantity)
              $ toList ledgerAssetsQs
    ]

-- ----------------------------------------------------------------------------
-- An alternative nested representation
--

-- | An alternative nested representation for 'Value' that groups assets that
-- share a 'PolicyId'.
newtype ValueNestedRep = ValueNestedRep [ValueNestedBundle]
  deriving (Eq, Ord, Show)

-- | A bundle within a 'ValueNestedRep' for a single 'PolicyId', or for the
-- special case of ada.
data ValueNestedBundle
  = ValueNestedBundleAda Quantity
  | ValueNestedBundle PolicyId (Map AssetName Quantity)
  deriving (Eq, Ord, Show)

valueToNestedRep :: Value -> ValueNestedRep
valueToNestedRep v =
  -- unflatten all the non-ada assets, and add ada separately
  ValueNestedRep $
    [ValueNestedBundleAda q | let q = selectAsset v AdaAssetId, q /= 0]
      ++ [ValueNestedBundle pId qs | (pId, qs) <- toList nonAdaAssets]
 where
  nonAdaAssets :: Map PolicyId (Map AssetName Quantity)
  nonAdaAssets =
    Map.fromListWith
      (Map.unionWith (<>))
      [ (pId, Map.singleton aName q)
      | (AssetId pId aName, q) <- valueToList v
      ]

valueFromNestedRep :: ValueNestedRep -> Value
valueFromNestedRep (ValueNestedRep bundles) =
  fromList
    [ (aId, q)
    | bundle <- bundles
    , (aId, q) <- case bundle of
        ValueNestedBundleAda q -> [(AdaAssetId, q)]
        ValueNestedBundle pId qs ->
          [ (AssetId pId aName, q)
          | (aName, q) <- toList qs
          ]
    ]

instance ToJSON ValueNestedRep where
  toJSON (ValueNestedRep bundles) = object $ map toPair bundles
   where
    toPair :: ValueNestedBundle -> (Aeson.Key, Aeson.Value)
    toPair (ValueNestedBundleAda q) = ("lovelace", toJSON q)
    toPair (ValueNestedBundle pid assets) = (Aeson.fromText $ renderPolicyId pid, toJSON assets)

instance FromJSON ValueNestedRep where
  parseJSON =
    withObject "ValueNestedRep" $ \obj ->
      ValueNestedRep
        <$> sequenceA
          [ parsePid keyValTuple
          | keyValTuple <- toList obj
          ]
   where
    parsePid :: (Aeson.Key, Aeson.Value) -> Aeson.Parser ValueNestedBundle
    parsePid ("lovelace", q) = ValueNestedBundleAda <$> parseJSON q
    parsePid (key, quantityBundleJson) = do
      polId <- P.runParserFail parsePolicyId $ Aeson.toText key
      ValueNestedBundle polId <$> parseJSON quantityBundleJson

-- ----------------------------------------------------------------------------
-- Printing and pretty-printing
--

-- | Render a textual representation of a 'Value'.
renderValue :: Value -> Text
renderValue = renderValueSep " + "

-- | Render a \"prettified\" textual representation of a 'Value'.
renderValuePretty :: Value -> Text
renderValuePretty = renderValueSep $ "\n" <> Text.replicate 4 " " <> "+ "

renderValueSep :: Text -> Value -> Text
renderValueSep sep v =
  if List.null valueList
    then "0 lovelace"
    else Text.intercalate sep (map renderAssetIdQuantityPair valueList)
 where
  valueList :: [(AssetId, Quantity)]
  valueList = valueToList v

renderAssetIdQuantityPair :: (AssetId, Quantity) -> Text
renderAssetIdQuantityPair (aId, quant) =
  Text.pack (show quant) <> " " <> renderAssetId aId

renderPolicyId :: PolicyId -> Text
renderPolicyId (PolicyId scriptHash) = serialiseToRawBytesHexText scriptHash

renderAssetId :: AssetId -> Text
renderAssetId AdaAssetId = "lovelace"
renderAssetId (AssetId polId (UnsafeAssetName "")) = renderPolicyId polId
renderAssetId (AssetId polId assetName) =
  renderPolicyId polId <> "." <> serialiseToRawBytesHexText assetName

renderMultiAsset :: L.MultiAsset -> Text
renderMultiAsset = renderValue . fromMultiAsset

renderMultiAssetPretty :: L.MultiAsset -> Text
renderMultiAssetPretty = renderValuePretty . fromMultiAsset
