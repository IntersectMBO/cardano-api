{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}


-- | Cardano eras, sometimes we have to distinguish them.
--
module Cardano.Api.Eras
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , CardanoEra(..)
  , IsCardanoEra(..)
  , AnyCardanoEra(..)
  , anyCardanoEra
  , cardanoEraConstraints
  , InAnyCardanoEra(..)
  , CardanoLedgerEra

    -- * FeatureInEra
  , FeatureInEra(..)
  , inEraFeature
  , maybeFeatureInEra
  , featureInShelleyBasedEra
  , inShelleyBasedEraFeature

    -- * Deprecated aliases
  , Byron
  , Shelley
  , Allegra
  , Mary

    -- * Shelley-based eras
  , ShelleyBasedEra(..)
  , IsShelleyBasedEra(..)
  , AnyShelleyBasedEra(..)
  , InAnyShelleyBasedEra(..)
  , shelleyBasedEraConstraints
  , shelleyBasedToCardanoEra

    -- ** Mapping to era types from the Shelley ledger library
  , ShelleyLedgerEra
  , eraProtVerLow

    -- * Cardano eras, as Byron vs Shelley-based
  , CardanoEraStyle(..)
  , cardanoEraStyle

    -- * Data family instances
  , AsType(AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra, AsAlonzoEra, AsBabbageEra, AsConwayEra,
           AsByron,    AsShelley,    AsAllegra,    AsMary,    AsAlonzo,    AsBabbage, AsConway)

    -- * Assertions on era
  , requireShelleyBasedEra

  , majorProtocolVersionToEra
  , withShelleyBasedEraConstraintsForLedger
  ) where

import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
