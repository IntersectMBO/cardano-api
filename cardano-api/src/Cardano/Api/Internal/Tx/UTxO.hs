{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Internal.Tx.UTxO where

import Cardano.Api.Internal.Eon.ShelleyBasedEra
  ( IsShelleyBasedEra
  , ShelleyBasedEra
  , ShelleyLedgerEra
  )
import Cardano.Api.Internal.Eras.Core (IsCardanoEra)
import Cardano.Api.Internal.Tx.Body
  ( CtxUTxO
  , TxOut (..)
  , fromShelleyTxIn
  , fromShelleyTxOut
  , toShelleyTxIn
  , toShelleyTxOut
  )
import Cardano.Api.Internal.TxIn (TxIn (..))

import Cardano.Ledger.Babbage ()
import Cardano.Ledger.Shelley.UTxO qualified as Ledger

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts qualified as GHC

newtype UTxO era = UTxO {unUTxO :: Map TxIn (TxOut CtxUTxO era)}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, GHC.IsList)

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m
  toEncoding (UTxO m) = toEncoding m

instance
  IsShelleyBasedEra era
  => FromJSON (UTxO era)
  where
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

-- | Infix version of `difference`.
(\\) :: UTxO era -> UTxO era -> UTxO era
a \\ b = difference a b

-- | Create an empty `UTxO`.
empty :: UTxO era
empty = UTxO Map.empty

-- | Create a `UTxO` from a single unspent transaction output.
singleton :: TxIn -> TxOut CtxUTxO era -> UTxO era
singleton i o = UTxO $ Map.singleton i o

-- | Find a 'TxOut' for a given 'TxIn'.
lookup :: TxIn -> UTxO era -> Maybe (TxOut CtxUTxO era)
lookup k = Map.lookup k . unUTxO

-- | Synonym for `lookup`.
resolveTxIn :: TxIn -> UTxO era -> Maybe (TxOut CtxUTxO era)
resolveTxIn = Cardano.Api.Internal.Tx.UTxO.lookup

-- | Filter all `TxOut` that satisfy the predicate.
filter :: (TxOut CtxUTxO era -> Bool) -> UTxO era -> UTxO era
filter fn = UTxO . Map.filter fn . unUTxO

-- | Filter all UTxO to only include 'out's satisfying given predicate.
filterWithKey :: (TxIn -> TxOut CtxUTxO era -> Bool) -> UTxO era -> UTxO era
filterWithKey fn = UTxO . Map.filterWithKey fn . unUTxO

-- | Get the 'UTxO domain input's set
inputSet :: UTxO era -> Set TxIn
inputSet = Map.keysSet . unUTxO

-- | Get the UTxO output set.
txOutputs :: UTxO era -> [TxOut CtxUTxO era]
txOutputs = Map.elems . unUTxO

-- | Remove the right hand side from the left hand side.
difference :: UTxO era -> UTxO era -> UTxO era
difference a b = UTxO $ Map.difference (unUTxO a) (unUTxO b)

-- | Convert from a list of key/value pairs.
fromList :: [(TxIn, TxOut CtxUTxO era)] -> UTxO era
fromList = UTxO . Map.fromList

-- | Convert to a list of key/value pairs.
toList :: UTxO era -> [(TxIn, TxOut CtxUTxO era)]
toList (UTxO xs) = Map.toList xs

-- | Convert to a Map of TxIn/TxOut.
toMap :: UTxO era -> Map TxIn (TxOut CtxUTxO era)
toMap = unUTxO

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
