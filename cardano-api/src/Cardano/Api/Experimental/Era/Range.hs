{-# LANGUAGE ExplicitNamespaces #-}

-- | Era ranges: eon-like spans of eras as type-level lists, with generic
-- membership witnesses, constraint reification and dispatch.
--
-- An era range is a type-level list of eras, usually written as a slice of
-- the timeline:
--
-- @
-- Onwards AlonzoEra         -- Alonzo and everything after it
-- ShelleyEra :-: BabbageEra -- the closed interval from Shelley to Babbage
-- @
--
-- A value of type @'EraIn' range era@ witnesses that @era@ is in @range@,
-- like an eon witness, but generic in the range. Constraints holding for
-- every era of a range are brought into scope with 'withRange'. Bundles are
-- written once, as a plain constraint alias, and made passable to
-- 'withRange' with a two-line class\/instance eta-expansion:
--
-- @
-- type MintConstraints era = (IsShelleyBasedEra era, L.MaryEraTxBody (ShelleyLedgerEra era))
--
-- class MintConstraints era => MintC era
--
-- instance MintConstraints era => MintC era
--
-- f :: EraIn (Onwards MaryEra) era -> ...
-- f w = withRange \@MintC w $ ...
-- @
--
-- Unlike the eon @*Constraints@ bundles, @'All' c range@ is discharged by
-- GHC from the per-era instances, so an era lacking support is a compile
-- error at the use site instead of a runtime error.
module Cardano.Api.Experimental.Era.Range
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
  , EraIn
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
  , SplitEras
  , splitRange
  )
where

import Cardano.Api.Experimental.Era.Range.Internal
