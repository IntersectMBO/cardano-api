-- | Cardano eras, sometimes we have to distinguish them.
module Cardano.Api.Era
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra

    -- * Eons
  , module Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
  , module Cardano.Api.Era.Internal.Eon.AllegraEraOnwards
  , module Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
  , module Cardano.Api.Era.Internal.Eon.ByronToAlonzoEra
  , module Cardano.Api.Era.Internal.Eon.MaryEraOnwards
  , module Cardano.Api.Era.Internal.Eon.ShelleyEraOnly
  , module Cardano.Api.Era.Internal.Eon.ShelleyToAllegraEra
  , module Cardano.Api.Era.Internal.Eon.ShelleyToAlonzoEra
  , module Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra
  , module Cardano.Api.Era.Internal.Eon.ShelleyToMaryEra
  , module Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
  , module Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards

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
  , module Cardano.Api.Era.Internal.Feature

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

import Cardano.Api.Era.Internal.Case
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.ByronToAlonzoEra
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.MaryEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyEraOnly
import Cardano.Api.Era.Internal.Eon.ShelleyToAllegraEra
import Cardano.Api.Era.Internal.Eon.ShelleyToAlonzoEra
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra
import Cardano.Api.Era.Internal.Eon.ShelleyToMaryEra
import Cardano.Api.Era.Internal.Feature
