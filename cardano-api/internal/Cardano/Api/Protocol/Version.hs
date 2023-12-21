{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- UndecidableInstances needed for 9.2.7 and 8.10.7
{-# LANGUAGE UndecidableInstances #-}

-- Only for UninhabitableType
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | This module defines the protocol versions corresponding to the eras in the Cardano blockchain.
module Cardano.Api.Protocol.Version
  ( BabbageEra
  , ConwayEra
  , pattern CurrentEra
  , pattern UpcomingEra
  , Era (..)
  , UseEra
  , VersionToSbe
  , useEra
  , protocolVersionToSbe
  ) where

import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import qualified Cardano.Api.Eras.Core as Api

import           GHC.TypeLits

-- | Users typically interact with the latest features on the mainnet or experiment with features
-- from the upcoming era. Hence, the protocol versions are limited to the current mainnet era
-- and the next era (upcoming era).
data BabbageEra
data ConwayEra

-- Allows us to gradually change the api without breaking things.
-- This will eventually be removed.
type family VersionToSbe version where
  VersionToSbe BabbageEra = Api.BabbageEra
  VersionToSbe ConwayEra = Api.ConwayEra

{- | Represents the eras in Cardano's blockchain.

Instead of enumerating every possible era, we use two constructors:
'CurrentEra' and 'UpcomingEra'. This design simplifies the handling
of eras, especially for 'cardano-api' consumers who are primarily concerned
with the current mainnet era and the next era for an upcoming hardfork.

Usage:
- 'CurrentEra': Reflects the era currently active on mainnet.
- 'UpcomingEra': Represents the era planned for the next hardfork.

After a hardfork, 'cardano-api' should be updated promptly to reflect
the new mainnet era in 'CurrentEra'.

-}
data Era version where
  -- | The era currently active on Cardano's mainnet.
  CurrentEraInternal :: Era BabbageEra
  -- | The era planned for the next hardfork on Cardano's mainnet.
  UpcomingEraInternal :: Era ConwayEra

{- | How to deprecate an era

  1. Add DEPRECATED pragma to the era type tag.
@
{-# DEPRECATED BabbageEra "BabbageEra no longer supported, use ConwayEra" #-}
data BabbageEra
@

  2. Add a new era type tag.
@
data Era version where
  -- | The era currently active on Cardano's mainnet.
  CurrentEraInternal :: Era ConwayEra
  -- | The era planned for the next hardfork on Cardano's mainnet.
  UpcomingEraInternal :: Era (UninhabitableType EraCurrentlyNonExistent)
@

  3. Update pattern synonyms.
@
pattern CurrentEra :: Era ConwayEra
pattern CurrentEra = CurrentEraInternal

pattern UpcomingEra :: Era (UninhabitableType EraCurrentlyNonExistent)
pattern UpcomingEra = UpcomingEraInternal
@

  4. Add new 'UseEra' instance and keep the deprecated era's instance.
@
instance UseEra BabbageEra where
  useEra = error "useEra: BabbageEra no longer supported, use ConwayEra"

instance UseEra ConwayEra where
  useEra = CurrentEra
@

  5. Update 'protocolVersionToSbe' as follows:
@
protocolVersionToSbe
  :: Era version
  -> Maybe (ShelleyBasedEra (VersionToSbe version))
protocolVersionToSbe CurrentEraInternal = Just ShelleyBasedEraBabbage
protocolVersionToSbe UpcomingEraInternal = Nothing
@
-}


{- | 'CurrentEraInternal' and 'UpcomingEraInternal' are for internal use only.
The above restriction combined with the following pattern synonyms
prevents a user from pattern matching on 'Era era' and
avoids the following situation:

@
doThing :: Era era -> ()
doThing = \case
  CurrentEraInternal -> enableFeature
  UpcomingEraInternal -> disableFeature
@

Consumers of this library must pick one of the two eras while
this library is responsibile for what happens at the boundary of the eras.
-}

pattern CurrentEra :: Era BabbageEra
pattern CurrentEra = CurrentEraInternal

pattern UpcomingEra :: Era ConwayEra
pattern UpcomingEra = UpcomingEraInternal

{-# COMPLETE CurrentEra, UpcomingEra #-}

protocolVersionToSbe
  :: Era version
  -> Maybe (ShelleyBasedEra (VersionToSbe version))
protocolVersionToSbe CurrentEraInternal = Just ShelleyBasedEraBabbage
protocolVersionToSbe UpcomingEraInternal = Nothing

-------------------------------------------------------------------------

-- | Type class interface for the 'Era' type.

class UseEra version where
  useEra :: Era version

instance UseEra BabbageEra where
  useEra = CurrentEra

instance UseEra ConwayEra where
  useEra = UpcomingEra


-- | After a hardfork there is usually no planned upcoming era
-- that we are able to experiment with. We force a type era
-- in this instance. See docs above.
data EraCurrentlyNonExistent

type family UninhabitableType a  where
  UninhabitableType EraCurrentlyNonExistent = TypeError ('Text "There is currently no planned upcoming era. Use CurrentEra instead.")


