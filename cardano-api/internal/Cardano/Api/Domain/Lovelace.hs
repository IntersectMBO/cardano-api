{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Api.Domain.Lovelace
  ( Lovelace(..)
  , toByronLovelace
  , fromByronLovelace
  , toShelleyLovelace
  , fromShelleyLovelace
  , fromShelleyDeltaLovelace
  , reLedgerCoinL
  , unLedgerCoinL
  ) where

import           Cardano.Api.SerialiseCBOR

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Ledger.Coin as Shelley

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro (Lens', lens)

newtype Lovelace = Lovelace Integer
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Real, Integral, Num, ToJSON, FromJSON, ToCBOR, FromCBOR)

instance Semigroup Lovelace where
  Lovelace a <> Lovelace b = Lovelace (a + b)

instance Monoid Lovelace where
  mempty = Lovelace 0

toByronLovelace :: Lovelace -> Maybe Byron.Lovelace
toByronLovelace (Lovelace x) =
    case Byron.integerToLovelace x of
      Left  _  -> Nothing
      Right x' -> Just x'

fromByronLovelace :: Byron.Lovelace -> Lovelace
fromByronLovelace = Lovelace . Byron.lovelaceToInteger

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l
--TODO: validate bounds

fromShelleyLovelace :: Shelley.Coin -> Lovelace
fromShelleyLovelace (Shelley.Coin l) = Lovelace l

fromShelleyDeltaLovelace :: Shelley.DeltaCoin -> Lovelace
fromShelleyDeltaLovelace (Shelley.DeltaCoin d) = Lovelace d

reLedgerCoinL :: Lens' Lovelace Shelley.Coin
reLedgerCoinL = lens toShelleyLovelace (const fromShelleyLovelace)

unLedgerCoinL :: Lens' Shelley.Coin Lovelace
unLedgerCoinL = lens fromShelleyLovelace (const toShelleyLovelace)
