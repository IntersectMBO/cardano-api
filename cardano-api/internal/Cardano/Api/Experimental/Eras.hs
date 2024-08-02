{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | This module defines the protocol versions corresponding to the eras in the Cardano blockchain.
module Cardano.Api.Experimental.Eras
  ( BabbageEra
  , ConwayEra
  , Era (..)
  , ToConstrainedLedgerEra
  , UseEra
  , ApiErasToLedgerEras
  , AvailableErasToSbe
  , ApiEraToExperimentalEra
  , DeprecatedEra (..)
  , useEra
  , protocolVersionToSbe
  , babbageEraOnwardsToEra
  , sbeToEra
  )
where

import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import qualified Cardano.Api.Eras.Core as Api
import           Cardano.Api.Via.ShowOf

import qualified Cardano.Ledger.Babbage as Ledger
import qualified Cardano.Ledger.Conway as Ledger

import           Data.Kind
import           Prettyprinter

-- | Users typically interact with the latest features on the mainnet or experiment with features
-- from the upcoming era. Hence, the protocol versions are limited to the current mainnet era
-- and the next era (upcoming era).
data BabbageEra

data ConwayEra

-- Allows us to gradually change the api without breaking things.
-- This will eventually be removed.
type family AvailableErasToSbe era = (r :: Type) | r -> era where
  AvailableErasToSbe BabbageEra = Api.BabbageEra
  AvailableErasToSbe ConwayEra = Api.ConwayEra

type family ApiEraToExperimentalEra era = (r :: Type) | r -> era where
  ApiEraToExperimentalEra Api.BabbageEra = BabbageEra
  ApiEraToExperimentalEra Api.ConwayEra = ConwayEra

type family ToConstrainedLedgerEra era = (r :: Type) | r -> era where
  ToConstrainedLedgerEra BabbageEra = Ledger.Babbage
  ToConstrainedLedgerEra ConwayEra = Ledger.Conway

type family ApiErasToLedgerEras era = (r :: Type) | r -> era where
  ApiErasToLedgerEras Api.BabbageEra = Ledger.Babbage
  ApiErasToLedgerEras Api.ConwayEra = Ledger.Conway

-- | Represents the eras in Cardano's blockchain.
-- This type represents eras currently on mainnet and new eras which are
-- in development.
--
-- After a hardfork, the from which we hardfork from gets deprecated and
-- after deprecation period, gets removed. During deprecation period,
-- consumers of cardano-api should update their codebase to the mainnet era.
data Era era where
  -- | The era currently active on Cardano's mainnet.
  BabbageEra :: Era BabbageEra
  -- | The upcoming era in development.
  ConwayEra :: Era ConwayEra

-- | How to deprecate an era
--
--   1. Add DEPRECATED pragma to the era type tag and the era constructor at the same time:
-- @
-- {-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
-- data BabbageEra
-- @
--
--   2. Update haddock for the constructor of the deprecated era, mentioning deprecation.
--
-- @
-- data Era era where
--   {-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
--   BabbageEra :: Era BabbageEra
--   -- | The era currently active on Cardano's mainnet.
--   ConwayEra :: Era ConwayEra
-- @
--
--   3. Add new 'UseEra' instance and update the deprecated era instance to produce a compile-time error:
-- @
-- instance TypeError ('Text "UseEra BabbageEra: Deprecated. Update to ConwayEra") => UseEra BabbageEra where
--   useEra = error "unreachable"
--
-- instance UseEra ConwayEra where
--   useEra = ConwayEra
-- @
protocolVersionToSbe
  :: Era era
  -> ShelleyBasedEra (AvailableErasToSbe era)
protocolVersionToSbe BabbageEra = ShelleyBasedEraBabbage
protocolVersionToSbe ConwayEra = ShelleyBasedEraConway

newtype DeprecatedEra era
  = DeprecatedEra (ShelleyBasedEra era)
  deriving Show

deriving via (ShowOf (DeprecatedEra era)) instance Pretty (DeprecatedEra era)

sbeToEra :: ShelleyBasedEra era -> Either (DeprecatedEra era) (Era (ApiEraToExperimentalEra era))
sbeToEra ShelleyBasedEraConway = return ConwayEra
sbeToEra ShelleyBasedEraBabbage = return BabbageEra
sbeToEra e@ShelleyBasedEraAlonzo = Left $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraMary = Left $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraAllegra = Left $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraShelley = Left $ DeprecatedEra e

babbageEraOnwardsToEra :: BabbageEraOnwards era -> Era (ApiEraToExperimentalEra era)
babbageEraOnwardsToEra BabbageEraOnwardsBabbage = BabbageEra
babbageEraOnwardsToEra BabbageEraOnwardsConway = ConwayEra

-------------------------------------------------------------------------

-- | Type class interface for the 'Era' type.
class UseEra era where
  useEra :: Era era

instance UseEra BabbageEra where
  useEra = BabbageEra

instance UseEra ConwayEra where
  useEra = ConwayEra
