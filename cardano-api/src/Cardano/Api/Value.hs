-- | Currency values
--
-- >>> lovelaceToQuantity 1000000
-- 1000000
--
-- >>> quantityToLovelace (Quantity 1000000)
-- Coin 1000000
--
-- >>> lovelaceToValue 1000000
-- valueFromList [(AdaAssetId,1000000)]
--
-- >>> selectLovelace mempty
-- Coin 0
--
-- >>> selectLovelace (lovelaceToValue 42)
-- Coin 42
--
-- >>> valueToLovelace (lovelaceToValue 42)
-- Just (Coin 42)
--
-- >>> valueToLovelace mempty
-- Just (Coin 0)
--
-- >>> allPositive (lovelaceToValue 42)
-- True
--
-- >>> allPositive (negateValue (lovelaceToValue 42))
-- False
--
-- >>> negateValue (lovelaceToValue 1000000)
-- valueFromList [(AdaAssetId,-1000000)]
--
-- >>> renderValue mempty
-- "0 lovelace"
--
-- >>> renderValue (lovelaceToValue 1000000)
-- "1000000 lovelace"
module Cardano.Api.Value
  ( Coin (..)

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
  , parseUTxOValue
  , parseMintingMultiAssetValue
  , parseTxOutMultiAssetValue

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
  , fromMaryPolicyID
  , fromMaryValue
  , fromMultiAsset
  , fromLedgerValue
  , toLedgerValue

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Value.Internal
import Cardano.Api.Value.Internal.Parser
