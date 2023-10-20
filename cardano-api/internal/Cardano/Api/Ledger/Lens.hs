{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Eta reduce" -}

module Cardano.Api.Ledger.Lens
  ( strictMaybeL
  , invalidBeforeL
  , invalidHereAfterL
  , invalidBeforeStrictL
  , invalidHereAfterStrictL
  , invalidBeforeTxBodyL
  , invalidHereAfterTxBodyL
  , ttlAsInvalidHereAfterTxBodyL
  ) where

import           Cardano.Api.Eon.AllegraEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyEraOnly
import           Cardano.Api.Eras.Case

import qualified Cardano.Ledger.Allegra.Core as L
import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.BaseTypes (SlotNo, StrictMaybe (..))

import           Lens.Micro

strictMaybeL :: Lens' (StrictMaybe a) (Maybe a)
strictMaybeL = lens g s
  where
    g :: StrictMaybe a -> Maybe a
    g SNothing  = Nothing
    g (SJust x) = Just x

    s :: StrictMaybe a -> Maybe a -> StrictMaybe a
    s _ = maybe SNothing SJust

invalidBeforeTxBodyL :: AllegraEraOnwards era -> Lens' (L.TxBody (ShelleyLedgerEra era)) (Maybe SlotNo)
invalidBeforeTxBodyL w = allegraEraOnwardsConstraints w $ L.vldtTxBodyL . invalidBeforeL

-- | Compatibility lens that provides a consistent interface over 'ttlTxBodyL' and
-- 'vldtTxBodyL . invalidHereAfterStrictL' across all shelley based eras.
--
-- The ledger uses 'ttlTxBodyL' in 'Shelley' only and from Allegra onwards uses 'vldtTxBodyL' instead.
--
-- The former is a 'SlotNo' with no limit represented as 'maxBound'.
--
-- The latter is a 'ValidityInterval' which is a pair of 'SlotNo's that represent the lower and upper
-- bounds.
--
-- The upper bound field is similar t 'ttlTxBodyL' except it is a 'StrictMaybe SlotNo' type where
-- no bounds is represented by 'SNothing'.
--
-- 'invalidHereAfterTxBodyL' lens over both with a 'Maybe SlotNo' type representation.  Withing the
-- Shelley era, setting Nothing will set the ttl to 'maxBound' in the underlying ledger type.
invalidHereAfterTxBodyL :: ShelleyBasedEra era -> Lens' (L.TxBody (ShelleyLedgerEra era)) (Maybe SlotNo)
invalidHereAfterTxBodyL =
  caseShelleyEraOnlyOrAllegraEraOnwards
    ttlAsInvalidHereAfterTxBodyL
    (const $ L.vldtTxBodyL . invalidHereAfterL)

-- | Compatibility lens over 'ttlTxBodyL' which represents 'maxBound' as Nothing and all other values as 'Just'.
ttlAsInvalidHereAfterTxBodyL :: ShelleyEraOnly era -> Lens' (L.TxBody (ShelleyLedgerEra era)) (Maybe SlotNo)
ttlAsInvalidHereAfterTxBodyL w = lens (g w) (s w)
  where
    g :: ShelleyEraOnly era -> L.TxBody (ShelleyLedgerEra era) -> Maybe SlotNo
    g w' txBody =
      shelleyEraOnlyConstraints w' $
        let ttl = txBody ^. L.ttlTxBodyL in if ttl == maxBound then Nothing else Just ttl

    s :: ShelleyEraOnly era -> L.TxBody (ShelleyLedgerEra era) -> Maybe SlotNo -> L.TxBody (ShelleyLedgerEra era)
    s w' txBody mSlotNo =
      shelleyEraOnlyConstraints w' $
        case mSlotNo of
          Nothing -> txBody & L.ttlTxBodyL .~ maxBound
          Just ttl -> txBody & L.ttlTxBodyL .~ ttl

invalidBeforeL :: Lens' L.ValidityInterval (Maybe SlotNo)
invalidBeforeL = invalidBeforeStrictL . strictMaybeL

invalidHereAfterL :: Lens' L.ValidityInterval (Maybe SlotNo)
invalidHereAfterL = invalidHereAfterStrictL . strictMaybeL

-- | Lens to access the 'invalidBefore' field of a 'ValidityInterval' as a 'StrictMaybe SlotNo'.
-- Ideally this should be defined in cardano-ledger
invalidBeforeStrictL :: Lens' L.ValidityInterval (StrictMaybe SlotNo)
invalidBeforeStrictL = lens g s
  where
    g :: L.ValidityInterval -> StrictMaybe SlotNo
    g (L.ValidityInterval a _) = a

    s :: L.ValidityInterval -> StrictMaybe SlotNo -> L.ValidityInterval
    s (L.ValidityInterval _ b) a = L.ValidityInterval a b

-- | Lens to access the 'invalidHereAfter' field of a 'ValidityInterval' as a 'StrictMaybe SlotNo'.
-- Ideally this should be defined in cardano-ledger
invalidHereAfterStrictL :: Lens' L.ValidityInterval (StrictMaybe SlotNo)
invalidHereAfterStrictL = lens g s
  where
    g :: L.ValidityInterval -> StrictMaybe SlotNo
    g (L.ValidityInterval _ b) = b

    s :: L.ValidityInterval -> StrictMaybe SlotNo -> L.ValidityInterval
    s (L.ValidityInterval a _) b = L.ValidityInterval a b
