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
{-# LANGUAGE TypeOperators #-}

-- | This module defines the protocol versions corresponding to the eras in the Cardano blockchain.
module Cardano.Api.Experimental.Eras
  ( BabbageEra
  , ConwayEra
  , Era (..)
  , LedgerEra
  , IsEra
  , ApiEraToLedgerEra
  , ExperimentalEraToApiEra
  , ApiEraToExperimentalEra
  , DeprecatedEra (..)
  , EraCommonConstraints
  , EraShimConstraints
  , obtainCommonConstraints
  , obtainShimConstraints
  , useEra
  , eraToSbe
  , babbageEraOnwardsToEra
  , sbeToEra
  )
where

import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import           Cardano.Api.Eras.Core (BabbageEra, ConwayEra)
import qualified Cardano.Api.Eras.Core as Api
import qualified Cardano.Api.ReexposeLedger as L
import           Cardano.Api.Via.ShowOf

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage as Ledger
import qualified Cardano.Ledger.Conway as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Hashes
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.UTxO as L

import           Control.Monad.Error.Class
import           Data.Kind
import           Prettyprinter

-- | Users typically interact with the latest features on the mainnet or experiment with features
-- from the upcoming era. Hence, the protocol versions are limited to the current mainnet era
-- and the next era (upcoming era).

-- Allows us to gradually change the api without breaking things.
-- This will eventually be removed.
type family ExperimentalEraToApiEra era = (r :: Type) | r -> era where
  ExperimentalEraToApiEra BabbageEra = Api.BabbageEra
  ExperimentalEraToApiEra ConwayEra = Api.ConwayEra

type family ApiEraToExperimentalEra era = (r :: Type) | r -> era where
  ApiEraToExperimentalEra Api.BabbageEra = BabbageEra
  ApiEraToExperimentalEra Api.ConwayEra = ConwayEra

type family LedgerEra era = (r :: Type) | r -> era where
  LedgerEra BabbageEra = Ledger.Babbage
  LedgerEra ConwayEra = Ledger.Conway

type family ApiEraToLedgerEra era = (r :: Type) | r -> era where
  ApiEraToLedgerEra Api.BabbageEra = Ledger.Babbage
  ApiEraToLedgerEra Api.ConwayEra = Ledger.Conway

-- | Represents the eras in Cardano's blockchain.
-- This type represents eras currently on mainnet and new eras which are
-- in development.
--
-- After a hardfork, the era from which we hardfork from gets deprecated and
-- after deprecation period, gets removed. During deprecation period,
-- consumers of cardano-api should update their codebase to the mainnet era.
data Era era where
  -- | The era currently active on Cardano's mainnet.
  BabbageEra :: Era BabbageEra
  -- | The upcoming era in development.
  ConwayEra :: Era ConwayEra

deriving instance Show (Era era)

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
--   3. Add new 'IsEra' instance and update the deprecated era instance to produce a compile-time error:
-- @
-- instance TypeError ('Text "IsEra BabbageEra: Deprecated. Update to ConwayEra") => IsEra BabbageEra where
--   useEra = error "unreachable"
--
-- instance IsEra ConwayEra where
--   useEra = ConwayEra
-- @
eraToSbe
  :: Era era
  -> ShelleyBasedEra (ExperimentalEraToApiEra era)
eraToSbe BabbageEra = ShelleyBasedEraBabbage
eraToSbe ConwayEra = ShelleyBasedEraConway

newtype DeprecatedEra era
  = DeprecatedEra (ShelleyBasedEra era)
  deriving Show

deriving via (ShowOf (DeprecatedEra era)) instance Pretty (DeprecatedEra era)

sbeToEra
  :: MonadError (DeprecatedEra era) m => ShelleyBasedEra era -> m (Era (ApiEraToExperimentalEra era))
sbeToEra ShelleyBasedEraConway = return ConwayEra
sbeToEra ShelleyBasedEraBabbage = return BabbageEra
sbeToEra e@ShelleyBasedEraAlonzo = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraMary = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraAllegra = throwError $ DeprecatedEra e
sbeToEra e@ShelleyBasedEraShelley = throwError $ DeprecatedEra e

babbageEraOnwardsToEra :: BabbageEraOnwards era -> Era (ApiEraToExperimentalEra era)
babbageEraOnwardsToEra BabbageEraOnwardsBabbage = BabbageEra
babbageEraOnwardsToEra BabbageEraOnwardsConway = ConwayEra

-------------------------------------------------------------------------

-- | Type class interface for the 'Era' type.
class IsEra era where
  useEra :: Era era

instance IsEra BabbageEra where
  useEra = BabbageEra

instance IsEra ConwayEra where
  useEra = ConwayEra

obtainShimConstraints
  :: BabbageEraOnwards era
  -> (EraShimConstraints era => a)
  -> a
obtainShimConstraints BabbageEraOnwardsBabbage x = x
obtainShimConstraints BabbageEraOnwardsConway x = x

-- We need these constraints in order to propagate the new
-- experimental api without changing the existing api
type EraShimConstraints era =
  ( LedgerEra (ApiEraToExperimentalEra era) ~ ShelleyLedgerEra era
  , ExperimentalEraToApiEra (ApiEraToExperimentalEra era) ~ era
  , L.EraTx (ApiEraToLedgerEra era)
  )

obtainCommonConstraints
  :: Era era
  -> (EraCommonConstraints era => a)
  -> a
obtainCommonConstraints BabbageEra x = x
obtainCommonConstraints ConwayEra x = x

type EraCommonConstraints era =
  ( L.AlonzoEraTx (LedgerEra era)
  , L.BabbageEraTxBody (LedgerEra era)
  , L.EraTx (LedgerEra era)
  , L.EraUTxO (LedgerEra era)
  , Ledger.EraCrypto (LedgerEra era) ~ L.StandardCrypto
  , ShelleyLedgerEra (ExperimentalEraToApiEra era) ~ LedgerEra era
  , L.HashAnnotated (Ledger.TxBody (LedgerEra era)) EraIndependentTxBody L.StandardCrypto
  )
