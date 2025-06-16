{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.Api.UTxO
  ( -- * UTxO type
    UTxO (..)

    -- * Operators
  , (\\)

    -- * Query
  , lookup
  , resolveTxIn
  , null
  , size
  , totalValue
  , totalLovelace

    -- * Construction
  , empty
  , singleton

    -- ** Insertion
  , insert

    -- ** Delete/Update
  , delete
  , adjust

    -- * Combine

    -- ** Union
  , union
  , unions

    -- ** Difference
  , difference

    -- ** Intersection
  , intersection

    -- * Traversal

    -- ** Map
  , map
  , mapWithKey
  , mapKeys

    -- ** Fold
  , foldMap

    -- * Conversion
  , inputSet
  , txOutputs

    -- ** Lists
  , fromList
  , toList

    -- ** Maps
  , toMap
  , fromMap

    -- ** Shelley
  , fromShelleyUTxO
  , toShelleyUTxO

    -- * Filter
  , Cardano.Api.UTxO.filter
  , filterWithKey

    -- * Find
  , find
  , findWithKey
  )
where

import Cardano.Api.Era.Internal.Core (IsCardanoEra)
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
  ( IsShelleyBasedEra
  , ShelleyBasedEra
  , ShelleyLedgerEra
  )
import Cardano.Api.Tx.Internal.Output
  ( CtxUTxO
  , TxOut (..)
  , fromShelleyTxOut
  , toShelleyTxOut
  , txOutValueToValue
  )
import Cardano.Api.Tx.Internal.TxIn (TxIn (..), fromShelleyTxIn, toShelleyTxIn)
import Cardano.Api.Value.Internal (Coin, Value, selectLovelace)

import Cardano.Ledger.Babbage ()
import Cardano.Ledger.Shelley.UTxO qualified as Ledger

import Prelude qualified

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Bool
import Data.Eq
import Data.Function
import Data.List qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.MonoTraversable
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple (uncurry)
import GHC.Exts qualified as GHC
import Text.Show

newtype UTxO era = UTxO {unUTxO :: Map TxIn (TxOut CtxUTxO era)}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance GHC.IsList (UTxO era) where
  type Item (UTxO era) = (TxIn, TxOut CtxUTxO era)
  fromList = UTxO . GHC.fromList
  toList = GHC.toList . unUTxO

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m
  toEncoding (UTxO m) = toEncoding m

instance IsShelleyBasedEra era => FromJSON (UTxO era) where
  parseJSON = Aeson.withObject "UTxO" $ \hm -> do
    let l = GHC.toList $ KeyMap.toHashMapText hm
    res <- mapM toTxIn l
    pure . UTxO $ Map.fromList res
   where
    toTxIn :: (Text, Aeson.Value) -> Parser (TxIn, TxOut CtxUTxO era)
    toTxIn (txinText, txOutVal) = do
      (,)
        <$> parseJSON (Aeson.String txinText)
        <*> parseJSON txOutVal

type instance Element (UTxO era) = TxOut CtxUTxO era

instance MonoFunctor (UTxO era) where
  omap f (UTxO utxos) = UTxO $ f <$> utxos

deriving newtype instance MonoFoldable (UTxO era)

instance MonoTraversable (UTxO era) where
  otraverse = omapM
  omapM f (UTxO utxos) = UTxO <$> omapM f utxos

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | Infix version of `difference`.
(\\) :: UTxO era -> UTxO era -> UTxO era
a \\ b = difference a b

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | Find a 'TxOut' for a given 'TxIn'.
lookup :: TxIn -> UTxO era -> Maybe (TxOut CtxUTxO era)
lookup k = Map.lookup k . unUTxO

-- | Synonym for `lookup`.
resolveTxIn :: TxIn -> UTxO era -> Maybe (TxOut CtxUTxO era)
resolveTxIn = Cardano.Api.UTxO.lookup

-- | Is the `UTxO` empty
null :: UTxO era -> Bool
null = Map.null . unUTxO

-- | The number of `TxOut`s in the `UTxO`.
size :: UTxO era -> Prelude.Int
size = Map.size . unUTxO

-- | The total `Value` stored in this `UTxO`.
totalValue :: UTxO era -> Value
totalValue = Cardano.Api.UTxO.foldMap (\(TxOut _ v _ _) -> txOutValueToValue v)

-- | The total `Lovelace` stored in this `UTxO`.
totalLovelace :: UTxO era -> Coin
totalLovelace = selectLovelace . totalValue

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | Create an empty `UTxO`.
empty :: UTxO era
empty = UTxO Map.empty

-- | Create a `UTxO` from a single unspent transaction output.
singleton :: TxIn -> TxOut CtxUTxO era -> UTxO era
singleton i o = UTxO $ Map.singleton i o

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | Insert a new `TxIn` and `TxOut` into the `UTxO`. If the `TxIn` is
-- already present in the `UTxO`, the associated `TxOut` is replaced with
-- the supplied `TxOut`.
insert :: TxIn -> TxOut CtxUTxO era -> UTxO era -> UTxO era
insert k v = UTxO . Map.insert k v . toMap

{--------------------------------------------------------------------
  Delete/Update
--------------------------------------------------------------------}

-- | Delete a `TxIn` and `TxOut` from the `UTxO` if it exists. When the `TxIn` is not
-- a member of the `UTxO`, the original `UTxO` is returned.
delete :: TxIn -> UTxO era -> UTxO era
delete k = UTxO . Map.delete k . toMap

-- | Update a `TxOut` corresponding to a specific `TxIn` with the result of the
-- provided function. When the `TxIn` is not a member of the `UTxO`, the
-- original `UTxO` is returned.
adjust :: (TxOut CtxUTxO era -> TxOut CtxUTxO era) -> TxIn -> UTxO era -> UTxO era
adjust f k = UTxO . Map.adjust f k . toMap

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}

-- | Left-biased union of two `UTxO`.
union :: UTxO era -> UTxO era -> UTxO era
union a b = UTxO $ Map.union (toMap a) (toMap b)

-- | The union of a list of `UTxO`.
unions :: [UTxO era] -> UTxO era
unions = UTxO . Map.unions . fmap toMap

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- | Difference of two `UTxO`. Returns elements of the first `UTxO` not existing
-- in the second `UTxO`.
difference :: UTxO era -> UTxO era -> UTxO era
difference a b = UTxO $ Map.difference (toMap a) (toMap b)

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

intersection :: UTxO era -> UTxO era -> UTxO era
intersection a b = UTxO $ Map.intersection (toMap a) (toMap b)

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | Map a function over all `TxOut` in the `UTxO`.
map :: (TxOut CtxUTxO era -> TxOut CtxUTxO era) -> UTxO era -> UTxO era
map f = UTxO . Map.map f . toMap

-- | Map a function over all `TxIn`/`TxOut` in the `UTxO`.
mapWithKey :: (TxIn -> TxOut CtxUTxO era -> TxOut CtxUTxO era) -> UTxO era -> UTxO era
mapWithKey f = UTxO . Map.mapWithKey f . toMap

-- | Map a function over the `TxIn` keys in the `UTxO`.
mapKeys :: (TxIn -> TxIn) -> UTxO era -> UTxO era
mapKeys f = UTxO . Map.mapKeys f . toMap

{--------------------------------------------------------------------
   Filter
--------------------------------------------------------------------}

-- | Filter all `TxOut` that satisfy the predicate.
filter :: (TxOut CtxUTxO era -> Bool) -> UTxO era -> UTxO era
filter fn = UTxO . Map.filter fn . toMap

-- | Filter all UTxO to only include 'out's satisfying given predicate.
filterWithKey :: (TxIn -> TxOut CtxUTxO era -> Bool) -> UTxO era -> UTxO era
filterWithKey fn = UTxO . Map.filterWithKey fn . toMap

{--------------------------------------------------------------------
   Fold
--------------------------------------------------------------------}

-- | Fold the `TxOut`s to a monoid and combine the results.
foldMap :: Monoid m => (TxOut CtxUTxO era -> m) -> UTxO era -> m
foldMap fn = Prelude.foldMap fn . toMap

{--------------------------------------------------------------------
   Find
--------------------------------------------------------------------}

-- | Find the first `TxIn`/`TxOut` pair in `UTxO` using the predicate.
find :: (TxOut CtxUTxO era -> Bool) -> UTxO era -> Maybe (TxIn, TxOut CtxUTxO era)
find f = findWithKey (const f)

-- | Find the first `TxIn`/`TxOut` pair in `UTxO` using the predicate.
findWithKey :: (TxIn -> TxOut CtxUTxO era -> Bool) -> UTxO era -> Maybe (TxIn, TxOut CtxUTxO era)
findWithKey f = Data.List.find (uncurry f) . toList

{--------------------------------------------------------------------
   Conversion
--------------------------------------------------------------------}

-- | Get the `UTxO`'s `TxIn` `Set`.
inputSet :: UTxO era -> Set TxIn
inputSet = Map.keysSet . unUTxO

-- | Get the `UTxO`'s `TxOut` `Set`.
txOutputs :: UTxO era -> [TxOut CtxUTxO era]
txOutputs = Map.elems . unUTxO

{--------------------------------------------------------------------
   Lists
--------------------------------------------------------------------}

-- | Convert to a `List` of `TxIn`/`TxOut` pairs.
fromList :: [(TxIn, TxOut CtxUTxO era)] -> UTxO era
fromList = UTxO . Map.fromList

-- | Convert from a `List` of `TxIn`/`TxOut` pairs.
toList :: UTxO era -> [(TxIn, TxOut CtxUTxO era)]
toList (UTxO xs) = Map.toList xs

{--------------------------------------------------------------------
   Maps
--------------------------------------------------------------------}

-- | Convert to a `Map` of `TxIn`/`TxOut`.
toMap :: UTxO era -> Map TxIn (TxOut CtxUTxO era)
toMap = unUTxO

-- | Convert from a `Map` of `TxIn`/`TxOut`.
fromMap :: Map TxIn (TxOut CtxUTxO era) -> UTxO era
fromMap = UTxO

{--------------------------------------------------------------------
   Shelley
--------------------------------------------------------------------}

-- | Convert from a `cardano-api` `UTxO` to a `cardano-ledger` UTxO.
toShelleyUTxO :: ShelleyBasedEra era -> UTxO era -> Ledger.UTxO (ShelleyLedgerEra era)
toShelleyUTxO sbe =
  Ledger.UTxO . Map.foldMapWithKey f . unUTxO
 where
  f i o =
    Map.singleton (toShelleyTxIn i) (toShelleyTxOut sbe o)

-- | Convert from a `cardano-ledger` `UTxO` to a `cardano-api` UTxO.
fromShelleyUTxO :: ShelleyBasedEra era -> Ledger.UTxO (ShelleyLedgerEra era) -> UTxO era
fromShelleyUTxO sbe =
  UTxO . Map.foldMapWithKey f . Ledger.unUTxO
 where
  f i o =
    Map.singleton (fromShelleyTxIn i) (fromShelleyTxOut sbe o)
