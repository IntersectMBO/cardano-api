{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Era ranges as type-level lists.
--
-- This module contains the implementation details of
-- "Cardano.Api.Experimental.Era.Range". Import that module instead unless you
-- need access to the internals.
module Cardano.Api.Experimental.Era.Range.Internal
  ( -- * Timeline
    ShelleyBasedEras
  , SupportedEras

    -- * Range constructors
  , From
  , UpTo
  , Onwards
  , type (:-:)
  , type (++)

    -- * Membership witness
  , EraIn (..)
  , eraInToCardanoEra
  , KnownEras (..)
  , maybeEraIn
  , HasEraIn (..)

    -- * Constraints over a range
  , All
  , type (:&:)
  , withRange
  , rangeDict

    -- * Splitting a range
  , SplitEras (..)
  , splitRange

    -- * Compile-time consistency checks
  , ErasDownFrom
  , ToLedgerEras
  , LedgerTimeline
  , supportedErasAligned
  , timelineMatchesLedger
  )
where

import Cardano.Api.Era.Internal.Core
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , CardanoEra
  , ConwayEra
  , DijkstraEra
  , Eon (..)
  , IsCardanoEra (..)
  , MaryEra
  , ShelleyEra
  , ToCardanoEra (..)
  )
import Cardano.Api.Era.Internal.Eon.Convert (Convert (..))
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyLedgerEra)
import Cardano.Api.Experimental.Era qualified as Exp

import Cardano.Ledger.Api.Era qualified as L

import Data.Constraint (Dict (..))
import Data.Kind (Constraint, Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Typeable (Typeable)
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | All Shelley-based eras, in chronological order.
--
-- This is the single place where the era timeline is spelled out. Era ranges
-- ('Onwards', ':-:') are computed slices of this list, so they pick up new
-- eras automatically. 'timelineMatchesLedger' proves at compile time that
-- this list stays in sync with the ledger's own era chain.
--
-- Byron is excluded, exactly as 'Cardano.Api.ShelleyBasedEra' excludes it.
type ShelleyBasedEras =
  '[ShelleyEra, AllegraEra, MaryEra, AlonzoEra, BabbageEra, ConwayEra, DijkstraEra]

-- | The eras supported by the experimental API: the era currently on mainnet
-- and the upcoming one. This tracks the 'Exp.Era' GADT; see
-- 'supportedErasAligned' for the compile-time proof that it stays in sync
-- with @'Onwards' 'ConwayEra'@.
--
-- Spelled out as a literal list (rather than via 'Onwards') so that it can
-- appear in instance heads.
type SupportedEras = '[ConwayEra, DijkstraEra]

-- | The suffix of the timeline @eras@ starting at @era@.
type family From (era :: Type) (eras :: [Type]) :: [Type] where
  From era (era ': eras) = era ': eras
  From era (_ ': eras) = From era eras
  From era '[] =
    TypeError ('Text "From: era " ':<>: 'ShowType era ':<>: 'Text " is not in the timeline")

-- | The prefix of the timeline @eras@ ending at @era@ (inclusive).
type family UpTo (era :: Type) (eras :: [Type]) :: [Type] where
  UpTo era (era ': _) = '[era]
  UpTo era (e ': eras) = e ': UpTo era eras
  UpTo era '[] =
    TypeError ('Text "UpTo: era " ':<>: 'ShowType era ':<>: 'Text " is not in the timeline")

-- | All eras from @era@ onwards. A new era added to 'ShelleyBasedEras'
-- automatically joins every range defined this way.
--
-- >>> -- Onwards AlonzoEra ~ '[AlonzoEra, BabbageEra, ConwayEra, DijkstraEra]
type Onwards era = From era ShelleyBasedEras

-- | The closed era interval from @era1@ to @era2@, both inclusive.
--
-- >>> -- ShelleyEra :-: MaryEra ~ '[ShelleyEra, AllegraEra, MaryEra]
type era1 :-: era2 = UpTo era2 (From era1 ShelleyBasedEras)

-- | Type-level list append. @xs '++' ys@ is the range that covers @xs@
-- followed by @ys@; see 'splitRange' for the inverse.
type family (eras1 :: [Type]) ++ (eras2 :: [Type]) :: [Type] where
  '[] ++ eras = eras
  (e ': eras1) ++ eras2 = e ': (eras1 ++ eras2)

infixr 5 ++

-- | A witness that @era@ is a member of the era range @eras@, carrying the
-- 'CardanoEra' singleton for @era@.
--
-- This is the generic counterpart of the hand-written eon GADTs:
-- @'EraIn' ('Onwards' 'AlonzoEra') era@ plays the role of
-- @AlonzoEraOnwards era@, but works for any range without a dedicated type.
--
-- Obtain a witness with 'eraIn' (statically) or 'checkMember' (dynamically),
-- constraints with 'withRange', and dispatch with 'splitRange'.
data EraIn (eras :: [Type]) (era :: Type) where
  EraHere :: CardanoEra era -> EraIn (era ': eras) era
  EraThere :: EraIn eras era -> EraIn (e ': eras) era

instance Show (EraIn eras era) where
  showsPrec d = showsPrec d . eraInToCardanoEra

-- | Extract the era singleton from a range membership witness.
eraInToCardanoEra :: EraIn eras era -> CardanoEra era
eraInToCardanoEra = \case
  EraHere era -> era
  EraThere w -> eraInToCardanoEra w

instance ToCardanoEra (EraIn eras) where
  toCardanoEra = eraInToCardanoEra

instance Convert (EraIn eras) CardanoEra where
  convert = eraInToCardanoEra

-- | An era range whose members can be enumerated and tested against.
-- Instances exist for every type-level list of eras; no per-range code is
-- needed.
class KnownEras (eras :: [Type]) where
  -- | Check whether @era@ is in the range, returning the membership witness
  -- if so.
  checkMember :: CardanoEra era -> Maybe (EraIn eras era)

  -- | All eras in the range, in chronological order.
  knownEras :: [Exp.Some (EraIn eras)]

instance KnownEras '[] where
  checkMember _ = Nothing
  knownEras = []

instance (IsCardanoEra e, Typeable eras, KnownEras eras) => KnownEras (e ': eras) where
  checkMember era =
    case testEquality era (cardanoEra @e) of
      Just Refl -> Just $ EraHere era
      Nothing -> EraThere <$> checkMember era
  knownEras =
    Exp.Some (EraHere $ cardanoEra @e)
      : map (\(Exp.Some w) -> Exp.Some $ EraThere w) (knownEras @eras)

-- | Obtain a membership witness for a statically known era.
maybeEraIn :: (KnownEras eras, IsCardanoEra era) => Maybe (EraIn eras era)
maybeEraIn = checkMember cardanoEra

-- | Every 'EraIn' range is an 'Eon', so the existing eon combinators
-- ('Cardano.Api.forEraInEon', 'Cardano.Api.forEraMaybeEon',
-- 'Cardano.Api.Featured', ...) work with era ranges unchanged.
instance KnownEras eras => Eon (EraIn eras) where
  inEonForEra no yes = maybe no yes . checkMember

-- | Statically known membership of @era@ in the range @eras@: the generic
-- counterpart of the @Is*BasedEra@ class ladder.
class HasEraIn (eras :: [Type]) (era :: Type) where
  -- | The membership witness for @era@.
  eraIn :: EraIn eras era

instance {-# OVERLAPPING #-} IsCardanoEra era => HasEraIn (era ': eras) era where
  eraIn = EraHere cardanoEra

instance HasEraIn eras era => HasEraIn (e ': eras) era where
  eraIn = EraThere eraIn

-- | @'All' c eras@ holds when the constraint @c@ holds for every era in the
-- range. GHC discharges it instance by instance for a concrete range, so no
-- hand-written constraint tables are needed - and an era lacking an instance
-- is a compile error naming that era, not a runtime error.
type family All (c :: Type -> Constraint) (eras :: [Type]) :: Constraint where
  All _ '[] = ()
  All c (era ': eras) = (c era, All c eras)

-- | Constraint product: @(c ':&:' d) era@ holds when both @c era@ and
-- @d era@ hold. Defined once so that use sites can combine constraints for
-- 'withRange' without declaring an ad-hoc bundle class:
--
-- @
-- withRange \@(SomeBundleC :&: OtherBundleC) w $ ...
-- @
--
-- (A type synonym would not do here: 'withRange' takes its constraint
-- parameter unapplied, and type synonyms cannot be passed unsaturated.)
class (c era, d era) => (c :&: d) era

instance (c era, d era) => (c :&: d) era

infixr 7 :&:

-- | Bring the constraint @c era@ into scope, given that @c@ holds for every
-- era in the range. This is the generic replacement for the per-eon
-- @*Constraints@ functions:
--
-- @
-- shelleyBasedEraConstraints sbe f   -- old, per-eon
-- withRange \@ShelleyBasedC w f      -- new, any range, any constraint
-- @
withRange
  :: forall c eras era a
   . All c eras
  => EraIn eras era
  -> (c era => a)
  -> a
withRange w f =
  case w of
    EraHere _ -> f
    EraThere w' -> withRange @c w' f

-- | Like 'withRange', but returning first-class evidence that can be stored
-- in data structures and applied later with 'Data.Constraint.withDict'.
rangeDict :: forall c eras era. All c eras => EraIn eras era -> Dict (c era)
rangeDict w = withRange @c w Dict

-- | Ranges that a wider range can be split at. Instances exist for every
-- type-level list; no per-boundary code is needed.
class SplitEras (eras1 :: [Type]) where
  -- | See 'splitRange'.
  splitEras
    :: forall eras2 era
     . EraIn (eras1 ++ eras2) era
    -> Either (EraIn eras1 era) (EraIn eras2 era)

instance SplitEras '[] where
  splitEras = Right

instance SplitEras eras1 => SplitEras (e ': eras1) where
  splitEras
    :: forall eras2 era
     . EraIn ((e ': eras1) ++ eras2) era
    -> Either (EraIn (e ': eras1) era) (EraIn eras2 era)
  splitEras = \case
    EraHere era -> Left $ EraHere era
    EraThere w -> either (Left . EraThere) Right $ splitEras @eras1 @eras2 w

-- | Split a range at a boundary: the total, generic replacement for the
-- @caseShelleyToXOrYEraOnwards@ dispatchers.
--
-- @
-- case splitRange \@(ShelleyEra :-: BabbageEra) \@(Onwards ConwayEra) w of
--   Left shelleyToBabbage -> ...
--   Right conwayOnwards -> ...
-- @
--
-- Both range arguments must be given by type application, since @eras1 '++'
-- eras2@ cannot be decomposed by inference.
splitRange
  :: forall eras1 eras2 era
   . SplitEras eras1
  => EraIn (eras1 ++ eras2) era
  -> Either (EraIn eras1 era) (EraIn eras2 era)
splitRange = splitEras @eras1 @eras2

-- | Widen the experimental two-era window to its range form.
instance Convert Exp.Era (EraIn SupportedEras) where
  convert = \case
    Exp.ConwayEra -> EraHere cardanoEra
    Exp.DijkstraEra -> EraThere $ EraHere cardanoEra

-- | Narrow the range form back to the experimental 'Exp.Era' GADT.
instance Convert (EraIn SupportedEras) Exp.Era where
  convert = \case
    EraHere _ -> Exp.ConwayEra
    EraThere (EraHere _) -> Exp.DijkstraEra
    EraThere (EraThere w) -> case w of {}

-- | The ledger's own era chain, derived by walking 'L.PreviousEra' downwards
-- from an era. The walk stops at Shelley: @'L.PreviousEra' 'L.ShelleyEra'@ is
-- Byron, whose own predecessor is not exported by the ledger.
type family ErasDownFrom (era :: Type) :: [Type] where
  ErasDownFrom L.ShelleyEra = '[L.ShelleyEra]
  ErasDownFrom era = era ': ErasDownFrom (L.PreviousEra era)

-- | Map a list of cardano-api eras to their ledger eras.
type family ToLedgerEras (eras :: [Type]) :: [Type] where
  ToLedgerEras '[] = '[]
  ToLedgerEras (era ': eras) = ShelleyLedgerEra era ': ToLedgerEras eras

type family Reverse' (acc :: [Type]) (eras :: [Type]) :: [Type] where
  Reverse' acc '[] = acc
  Reverse' acc (e ': eras) = Reverse' (e ': acc) eras

-- | The ledger's era chain in chronological order, anchored on
-- 'L.LatestKnownEra' so that a new ledger era extends it automatically.
type LedgerTimeline = Reverse' '[] (ErasDownFrom L.LatestKnownEra)

-- | Compile-time proof that 'SupportedEras' tracks
-- @'Onwards' 'ConwayEra'@. When a new era is appended to 'ShelleyBasedEras',
-- this stops compiling exactly until 'SupportedEras' (and the 'Exp.Era'
-- GADT it mirrors) catches up.
supportedErasAligned :: SupportedEras :~: Onwards ConwayEra
supportedErasAligned = Refl

-- | Compile-time proof that 'ShelleyBasedEras' matches the ledger's own era
-- chain. When the ledger bumps 'L.LatestKnownEra', this stops compiling
-- exactly where cardano-api era support must be added.
timelineMatchesLedger :: ToLedgerEras ShelleyBasedEras :~: LedgerTimeline
timelineMatchesLedger = Refl
