{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | \"Some era in a range\", as a 'Vary' of era singletons.
--
-- @'SomeEraIn' range@ is the existential counterpart of
-- @'EraIn' range era@: it holds the singleton of one (statically unknown)
-- era of the range. It replaces bespoke existentials like @AnyCardanoEra@
-- and @AnyShelleyBasedEra@, one per range, and inherits 'Vary''s instances
-- ('Show', 'Eq', 'Ord') and combinators: in particular 'Vary.morph' widens a
-- @'SomeEraIn' range@ to any wider range in O(1).
module Cardano.Api.Experimental.Era.Range.Some
  ( Sings
  , SomeEraIn
  , SingIn
  , someEra
  , injectEra
  , VaryEras (..)

    -- * Pattern matching on single eras
  , onEra
  , noMoreEras
  , isEra
  )
where

import Cardano.Api.Era.Internal.Core (CardanoEra, IsCardanoEra (..))
import Cardano.Api.Experimental.Era.Range.Internal

import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)

import Vary (Vary, (:|))
import Vary qualified

-- | Map a list of eras to their 'CardanoEra' singleton types. The family is
-- injective, so the range can be inferred back from the singleton list.
type family Sings (eras :: [Type]) = (r :: [Type]) | r -> eras where
  Sings '[] = '[]
  Sings (era ': eras) = CardanoEra era ': Sings eras

-- | The singleton of some era in the range @eras@.
type SomeEraIn eras = Vary (Sings eras)

-- | Membership of @era@'s singleton in the range's 'Vary', as a constraint.
-- Holds for every era of a concrete range; used through
-- @'All' ('SingIn' eras) eras@ by 'injectEra'.
class CardanoEra era :| Sings eras => SingIn eras era

instance CardanoEra era :| Sings eras => SingIn eras era

-- | Forget which era a membership witness is for, keeping the range. Use
-- this form when the era is statically known; 'injectEra' handles the
-- general case.
someEra :: CardanoEra era :| Sings eras => EraIn eras era -> SomeEraIn eras
someEra = Vary.from . eraInToCardanoEra

-- | Forget which era a membership witness is for, keeping the range. The
-- @'All' ('SingIn' eras) eras@ constraint is discharged by GHC for any
-- concrete range.
injectEra :: forall eras era. All (SingIn eras) eras => EraIn eras era -> SomeEraIn eras
injectEra w = withRange @(SingIn eras) w $ someEra w

-- | Ranges whose existential form can be eliminated back into a membership
-- witness. Instances exist for every type-level list of eras.
class KnownEras eras => VaryEras (eras :: [Type]) where
  -- | Bring the era of a @'SomeEraIn' eras@ back into scope, with its
  -- membership witness and its 'IsCardanoEra' instance.
  withSomeEra :: SomeEraIn eras -> (forall era. IsCardanoEra era => EraIn eras era -> r) -> r

instance VaryEras '[] where
  withSomeEra v _ = Vary.exhaustiveCase v

instance (IsCardanoEra e, Typeable eras, VaryEras eras) => VaryEras (e ': eras) where
  withSomeEra v f =
    case Vary.pop v of
      Right era -> f $ EraHere era
      Left rest -> withSomeEra rest $ \w -> f $ EraThere w

-- | Handle one specific era of a range, chosen by type application, passing
-- the remaining range on. The 'CardanoEra' wrapper is implicit. Chains of
-- 'onEra' terminated by 'noMoreEras' are exhaustive for exactly the range -
-- adding an era to the range is a compile error until a handler is added:
--
-- @
-- v & (onEra \@ConwayEra "mainnet" . onEra \@DijkstraEra "upcoming" $ noMoreEras)
-- @
onEra
  :: forall era eras r
   . r
  -> (SomeEraIn eras -> r)
  -> SomeEraIn (era ': eras)
  -> r
onEra matched continue v =
  case Vary.pop v of
    Right _ -> matched
    Left rest -> continue rest

-- | Terminate an 'onEra' chain: an empty range holds no era. Analogous to
-- 'Vary.exhaustiveCase'.
noMoreEras :: SomeEraIn '[] -> r
noMoreEras = Vary.exhaustiveCase

-- | Check whether the era held by a 'SomeEraIn' is @era@, chosen by type
-- application. The 'CardanoEra' wrapper is implicit.
isEra :: forall era eras. CardanoEra era :| Sings eras => SomeEraIn eras -> Bool
isEra = isJust . Vary.into @(CardanoEra era)
