module Cardano.Api.Class.IsMaryEraOnwards where

import Cardano.Api.Eras (AlonzoEra, BabbageEra, ConwayEra, MaryEra)
import Cardano.Api.Eon.MaryEraOnwards (MaryEraOnwards (..))

-- | Type class to produce 'MaryEraOnwards' witness values while staying
-- parameterized by era.
class IsMaryEraOnwards era where
  maryEraOnwards :: MaryEraOnwards era

instance IsMaryEraOnwards MaryEra where
  maryEraOnwards = MaryEraOnwardsMary

instance IsMaryEraOnwards AlonzoEra where
  maryEraOnwards = MaryEraOnwardsAlonzo

instance IsMaryEraOnwards BabbageEra where
  maryEraOnwards = MaryEraOnwardsBabbage

instance IsMaryEraOnwards ConwayEra where
  maryEraOnwards = MaryEraOnwardsConway
