{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Script execution unit prices and cost models
module Cardano.Api.Domain.ExecutionUnitPrices
  ( ExecutionUnitPrices(..)
  , toAlonzoPrices
  , fromAlonzoPrices
  ) where

import           Cardano.Api.Domain.CostModel
import           Cardano.Api.Json (toRationalJSON)
import           Cardano.Api.Orphans ()
import           Cardano.Api.SerialiseCBOR

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))


-- | The prices for 'ExecutionUnits' as a fraction of a 'Lovelace'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
data ExecutionUnitPrices = ExecutionUnitPrices
  { priceExecutionSteps  :: Rational
  , priceExecutionMemory :: Rational
  }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnitPrices where
  toCBOR ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR priceExecutionSteps
   <> toCBOR priceExecutionMemory

instance FromCBOR ExecutionUnitPrices where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnitPrices" 2
    ExecutionUnitPrices
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnitPrices where
  toJSON ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    object
      [ "priceSteps"  .= toRationalJSON priceExecutionSteps
      , "priceMemory" .= toRationalJSON priceExecutionMemory
      ]

instance FromJSON ExecutionUnitPrices where
  parseJSON =
    withObject "ExecutionUnitPrices" $ \o ->
      ExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"

toAlonzoPrices :: ExecutionUnitPrices -> Either ProtocolParametersConversionError Alonzo.Prices
toAlonzoPrices
    ExecutionUnitPrices
    { priceExecutionSteps
    , priceExecutionMemory
    } = do
  prSteps <- boundRationalEither "Steps" priceExecutionSteps
  prMem   <- boundRationalEither "Mem" priceExecutionMemory

  return Alonzo.Prices
    { Alonzo.prSteps
    , Alonzo.prMem
    }

fromAlonzoPrices :: Alonzo.Prices -> ExecutionUnitPrices
fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} =
  ExecutionUnitPrices
  { priceExecutionSteps  = Ledger.unboundRational prSteps
  , priceExecutionMemory = Ledger.unboundRational prMem
  }
