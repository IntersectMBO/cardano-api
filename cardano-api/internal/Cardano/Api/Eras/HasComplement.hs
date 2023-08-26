{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eras.HasComplement
  ( HasComplement(..)
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Feature.ConwayEraOnwards
import           Cardano.Api.Feature.ShelleyToBabbageEra

import           Data.Kind (Type)

class HasComplement (whole :: Type -> Type) (part :: Type -> Type) where
  type Complement whole part :: Type -> Type

instance HasComplement ShelleyBasedEra ConwayEraOnwards where
  type Complement ShelleyBasedEra ConwayEraOnwards = ShelleyToBabbageEra

instance HasComplement ShelleyBasedEra ShelleyToBabbageEra where
  type Complement ShelleyBasedEra ShelleyToBabbageEra = ConwayEraOnwards
