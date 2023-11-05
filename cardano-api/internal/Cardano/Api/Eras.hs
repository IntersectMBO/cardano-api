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

    -- * CardanoEra
  , CardanoEra(..)
  , IsCardanoEra(..)
  , AnyCardanoEra(..)
  , anyCardanoEra
  , InAnyCardanoEra(..)
  , inAnyCardanoEra
  , cardanoEraConstraints
  , CardanoLedgerEra
  , ToCardanoEra(..)

    -- * IsEon
  , Eon(..)
  , EraInEon(..)

  , inEonForEraMaybe
  , forEraInEon
  , forEraInEonMaybe
  , forEraMaybeEon
  , maybeEon
  , monoidForEraInEon
  , monoidForEraInEonA

    -- * Data family instances
  , AsType(AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra, AsAlonzoEra, AsBabbageEra, AsConwayEra)

    -- * Era case handling

    -- ** Case on CardanoEra
  , caseByronOrShelleyBasedEra

    -- ** Case on ShelleyBasedEra
  , caseByronToAllegraOrMaryEraOnwards
  , caseByronToAlonzoOrBabbageEraOnwards
  , caseShelleyToAllegraOrMaryEraOnwards
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards
  ) where

import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Core
