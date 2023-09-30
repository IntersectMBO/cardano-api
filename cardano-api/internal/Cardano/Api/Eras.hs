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
  , cardanoEraConstraints
  , InAnyCardanoEra(..)
  , CardanoLedgerEra
  , ToCardanoEra(..)

    -- * IsEon
  , Eon(..)
  , AnyEon(..)
  , AnyEraInEon(..)

  , inEonForEraMaybe
  , forEraInEon
  , forEraInEonMaybe
  , forEraMaybeEon
  , maybeEon

    -- * Data family instances
  , AsType(AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra, AsAlonzoEra, AsBabbageEra, AsConwayEra)

  , withShelleyBasedEraConstraintsForLedger

    -- * Era case handling

    -- ** Case on CardanoEra
  , caseByronOrShelleyBasedEra

    -- ** Case on ShelleyBasedEra
  , caseByronToAllegraOrMaryEraOnwards
  , caseByronToMaryOrAlonzoEraOnwards
  , caseByronToAlonzoOrBabbageEraOnwards
  , caseShelleyToAllegraOrMaryEraOnwards
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards
  ) where

import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
