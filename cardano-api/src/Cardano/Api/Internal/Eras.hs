-- | Cardano eras, sometimes we have to distinguish them.
module Cardano.Api.Internal.Eras
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra

    -- * CardanoEra
  , CardanoEra (..)
  , IsCardanoEra (..)
  , AnyCardanoEra (..)
  , anyCardanoEra
  , InAnyCardanoEra (..)
  , inAnyCardanoEra
  , cardanoEraConstraints
  , CardanoLedgerEra
  , ToCardanoEra (..)

    -- * IsEon
  , Eon (..)
  , EraInEon (..)
  , inEonForEraMaybe
  , forEraInEon
  , forEraInEonMaybe
  , forEraMaybeEon
  , maybeEon
  , monoidForEraInEon
  , monoidForEraInEonA
  , Convert (..)
  , Inject (..)

    -- * Data family instances
  , AsType (AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra, AsAlonzoEra, AsBabbageEra, AsConwayEra)

    -- * Era case handling

    -- ** Case on CardanoEra
  , caseByronOrShelleyBasedEra

    -- ** Case on ShelleyBasedEra
  , caseByronToAlonzoOrBabbageEraOnwards
  , caseShelleyToAllegraOrMaryEraOnwards
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards
  )
where

import           Cardano.Api.Internal.Eon.Convert
import           Cardano.Api.Internal.Eras.Case
import           Cardano.Api.Internal.Eras.Core
