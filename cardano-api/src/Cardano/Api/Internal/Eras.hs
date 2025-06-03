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

    -- * Eons
  , module Cardano.Api.Internal.Eon.ShelleyBasedEra
  , module Cardano.Api.Internal.Eon.AllegraEraOnwards
  , module Cardano.Api.Internal.Eon.BabbageEraOnwards
  , module Cardano.Api.Internal.Eon.ByronToAlonzoEra
  , module Cardano.Api.Internal.Eon.MaryEraOnwards
  , module Cardano.Api.Internal.Eon.ShelleyEraOnly
  , module Cardano.Api.Internal.Eon.ShelleyToAllegraEra
  , module Cardano.Api.Internal.Eon.ShelleyToAlonzoEra
  , module Cardano.Api.Internal.Eon.ShelleyToBabbageEra
  , module Cardano.Api.Internal.Eon.ShelleyToMaryEra
  , module Cardano.Api.Internal.Eon.ConwayEraOnwards
  , module Cardano.Api.Internal.Eon.AlonzoEraOnwards

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

    -- * Era-dependend features
  , module Cardano.Api.Internal.Feature

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

import Cardano.Api.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Internal.Eon.ByronToAlonzoEra
import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Internal.Eon.MaryEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eon.ShelleyEraOnly
import Cardano.Api.Internal.Eon.ShelleyToAllegraEra
import Cardano.Api.Internal.Eon.ShelleyToAlonzoEra
import Cardano.Api.Internal.Eon.ShelleyToBabbageEra
import Cardano.Api.Internal.Eon.ShelleyToMaryEra
import Cardano.Api.Internal.Eras.Case
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.Feature
