{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Eras.Core2 where

import           Data.Function
import           Data.GADT.Show
import           Data.Kind
import           Data.WorldPeace hiding (Z)
-- import Data.Some


-- -- foo :: OpenUnion '[Double, Int, Bool]
-- -- foo = unionLift 42
-- --
-- sommeera :: Union CardanoEra '[ByronEra, ShelleyEra]
-- sommeera  = unionLift ByronEra

-- Type-level Peano-style natural numbers enabling traversing over eras on a type level

data S a
data Z


-- Give each era a type-level number
-- TODO: maybe we should get rid of Era suffixes?

type ByronEra = Z
type ShelleyEra = S ByronEra
type AllegraEra = S ShelleyEra
type MaryEra = S AllegraEra
type AlonzoEra = S MaryEra
type BabbageEra = S AlonzoEra
type ConwayEra = S BabbageEra
-- Represents indefinite future era - used as a type-level boundary - not meant to be used in 'CardanoEra'
type FutureEra = S ConwayEra

-- type family ToPeano a
-- type family FromPeano a
--
-- data ByronEra
-- type ByronEraNat = Z
-- type instance ToPeano ByronEra = ByronEraNat
-- type instance FromPeano ByronEraNat = ByronEra
--
-- data ShelleyEra
-- type ShelleyEraNat = S ByronEraNat
-- type instance ToPeano ShelleyEra = ShelleyEraNat
-- type instance FromPeano ShelleyEraNat = ShelleyEra
--
-- data AllegraEra
-- type AllegraEraNat = S ShelleyEraNat
-- type instance ToPeano AllegraEra = AllegraEraNat
-- type instance FromPeano AllegraEraNat = AllegraEra
--
-- data MaryEra
-- type MaryEraNat = S AllegraEraNat
-- type instance ToPeano MaryEra = MaryEraNat
-- type instance FromPeano MaryEraNat = MaryEra
--
-- data AlonzoEra
-- type AlonzoEraNat = S MaryEraNat
-- type instance ToPeano AlonzoEra = AlonzoEraNat
-- type instance FromPeano AlonzoEraNat = AlonzoEra
--
-- data BabbageEra
-- type BabbageEraNat = S AlonzoEraNat
-- type instance ToPeano BabbageEra = BabbageEraNat
-- type instance FromPeano BabbageEraNat = BabbageEra
--
-- data ConwayEra
-- type ConwayEraNat = S BabbageEraNat
-- type instance ToPeano ConwayEra = ConwayEraNat
-- type instance FromPeano ConwayEraNat = ConwayEra
--
-- data FutureEra
-- type FutureEraNat  = S ConwayEra
-- type instance ToPeano FutureEra = FutureEraNat
-- type instance FromPeano FutureEraNat = FutureEra

-- This does not change
data CardanoEra era where
  ByronEra   :: CardanoEra ByronEra
  ShelleyEra :: CardanoEra ShelleyEra
  AllegraEra :: CardanoEra AllegraEra
  MaryEra    :: CardanoEra MaryEra
  AlonzoEra  :: CardanoEra AlonzoEra
  BabbageEra :: CardanoEra BabbageEra
  ConwayEra  :: CardanoEra ConwayEra

deriving instance Eq (CardanoEra era)
deriving instance Show (CardanoEra era)
instance GShow CardanoEra where
  gshowsPrec = showsPrec

deriving instance Ord (CardanoEra era)

-- 'Some' is a generalization of our AnyCardanoEra - this is really not needed, but seems a bit cleaner to me than Any* types

-- anyCardanoEra :: Some CardanoEra -- AnyCardanoEra
-- anyCardanoEra = Some $ ByronEra
--
-- instance Enum (Some CardanoEra) where
--   fromEnum c = withSome c $ \case
--     ByronEra   -> 0
--     ShelleyEra -> 1
--     AllegraEra -> 2
--     MaryEra    -> 3
--     AlonzoEra  -> 4
--     BabbageEra -> 5
--     ConwayEra  -> 6
--   toEnum = \case
--     0 -> Some ByronEra
--     1 -> Some ShelleyEra
--     2 -> Some AllegraEra
--     3 -> Some MaryEra
--     4 -> Some AlonzoEra
--     5 -> Some BabbageEra
--     6 -> Some ConwayEra
--     _ -> error "toEnum @CardanoEra: bad argument"
--
-- instance Bounded (Some CardanoEra) where
--   minBound = Some ByronEra
--   maxBound = Some ConwayEra


-- The union type allows us to represent an era, which belongs to a particular range
-- denoted by union. A type
--    Union CardanoEra '[ByronEra, AllegraEra]
-- means that CardanoEra can be only in those two eras

-- You can lift existing CardanoEra into union type:
-- >>> unionLift BabbageEra :: Union CardanoEra '[BabbageEra, ConwayEra]
-- BabbageEra

-- a replacement for ShelleyToMaryEra type
shelleyToMaryAllegra :: CardanoEraFromTo ShelleyEra MaryEra AllegraEra
shelleyToMaryAllegra = eraToEraIn @ShelleyEra @MaryEra AllegraEra

-- similar: shelley - alonzo range
shelleyToAlonzoAlonzo :: CardanoEraFromTo ShelleyEra AlonzoEra AlonzoEra
shelleyToAlonzoAlonzo = eraToEraIn @ShelleyEra @AlonzoEra AlonzoEra

-- all eras from conway onwards
babbageToConway :: CardanoEraFromTo BabbageEra ConwayEra ConwayEra -- Union CardanoEra '[ConwayEra, XEra, FutureEra]
babbageToConway = eraToEraIn @BabbageEra @ConwayEra ConwayEra

-- all eras from conway onwards
conwayToConway :: CardanoEraFromTo ConwayEra ConwayEra ConwayEra -- Union CardanoEra '[ConwayEra, XEra, FutureEra]
conwayToConway = eraToEraIn @ConwayEra @ConwayEra ConwayEra

-- all eras from conway onwards
conwayOnwardsConway :: CardanoEraOnwards ConwayEra ConwayEra -- Union CardanoEra '[ConwayEra, XEra, FutureEra]
conwayOnwardsConway = eraToEraIn @ConwayEra @FutureEra ConwayEra

-- You can try to retrieve CardanoEra from an unionEra
-- >>> unionMatch shelleyToAlonzoAlonzo :: Maybe (CardanoEra AlonzoEra)
-- Just AlonzoEra
-- >>> unionMatch shelleyToAlonzoAlonzo :: Maybe (CardanoEra AllegraEra)
-- Nothing


-- Represent era ranges for the type level era tags
type family EraRange e1 e2 where
  -- TODO: there are no checks if e1 <= e2
  EraRange e e = '[e]
  EraRange e1 e2 = e1 ': EraRange (S e1) e2

-- * handy aliases
-- denotes eras range between e1 and e2 (including e1 and e2)
type CardanoEraFromToOLD e1 e2 = Union CardanoEra (EraRange e1 e2)
-- eras from e1 till infinity
type CardanoEraOnwardsOLD e1 = Union CardanoEra (EraRange e1 FutureEra)
-- alias for a constraint telling that era is in range
type EraInRange e1 e2 e = IsMember e (EraRange e1 e2)


newtype CardanoEraIn es e = UnsafeCardanoEraIn { unCardanoEraIn :: IsMember e es => Union CardanoEra es}
type CardanoEraFromTo eFrom eTo era = EraInRange eFrom eTo era => CardanoEraIn (EraRange eFrom eTo) era
type CardanoEraOnwards e1 era = CardanoEraFromTo e1 FutureEra era

eraToEraIn :: forall eFrom eTo era. CardanoEra era -> CardanoEraFromTo eFrom eTo era
eraToEraIn = UnsafeCardanoEraIn . unionLift

eraHandle :: ElemRemove e es
          => IsMember era es
          => (CardanoEraIn (Remove e es) era -> b)
          -> (CardanoEra e -> b)
          -> CardanoEraIn es era
          -> b
eraHandle hu fa (UnsafeCardanoEraIn u) = unionHandle (hu . UnsafeCardanoEraIn) fa u

absurdEras :: CardanoEraIn '[] era -> b
absurdEras _ = error "absurd"

-- pattern matching on eras
caseMatch :: String
caseMatch = do
  let printConway = show :: CardanoEra ConwayEra -> String
      printBabbage = show :: CardanoEra BabbageEra -> String
      hrest :: IsMember era '[BabbageEra] => CardanoEraIn '[BabbageEra] era -> String
      hrest = eraHandle absurdEras printBabbage
  eraHandle hrest printConway babbageToConway

  -- let printShelley    = show :: CardanoEra ShelleyEra -> String
  --     printAllegraEra = show :: CardanoEra AllegraEra -> String
  --     printMary       = show :: CardanoEra MaryEra -> String
  -- absurdEras
  --   `eraHandle` printMary
  --   `eraHandle` printAllegraEra
  --   `eraHandle` printShelley
  --   $ shelleyToMaryAllegra

-- >>> caseMatch
-- "AllegraEra"

-- widen an union to include more eras
widenUnion :: String
widenUnion = do
  let shelleyToBabbage = relaxUnion shelleyToAlonzoAlonzo :: CardanoEraFromToOLD ShelleyEra BabbageEra
  show shelleyToBabbage

-- >>> widenUnion
-- "AlonzoEra"

-- union shrinking utility class
class UnionShrink as bs where
  unionShrink :: Union f as -> Maybe (Union f bs)

instance UnionShrink as '[] where
  unionShrink _ = Nothing

instance
  ( Contains bs (b ': bs)
  , Contains (Remove b as) as
  , ElemRemove b as
  , UnionShrink as bs
  ) => UnionShrink as (b ': bs) where
  unionShrink =
    unionHandle
      (fmap relaxUnion . unionShrink @as @bs . relaxUnion)
      (Just . unionLift @_ @b @(b ': bs))


-- shrink an era range into a narrower one
doInConwayOnwards :: CardanoEraOnwardsOLD ShelleyEra -> Maybe String
doInConwayOnwards era = do
  _ :: CardanoEraFromToOLD ShelleyEra BabbageEra <- unionShrink era
  _ :: CardanoEraOnwardsOLD ConwayEra <- unionShrink era
  pure "CONWAY!"

-- >>> doInConwayOnwards (unionLift ShelleyEra :: CardanoEraOnwards ShelleyEra)
-- Nothing

-- >>> doInConwayOnwards (unionLift ConwayEra :: CardanoEraOnwards ShelleyEra)
-- Just "CONWAY!"

-- * CAVEATS
-- Because eras are now type aliases, type errors become slightly less readable:
-- >>> doInConwayOnwards (unionLift BabbageEra :: CardanoEraOnwards BabbageEra)
-- Couldn't match type: '[]
--                with: '[ESucc (ESucc (ESucc (ESucc ByronEra))),
--                        ESucc (ESucc (ESucc (ESucc (ESucc ByronEra)))),
--                        ESucc (ESucc (ESucc (ESucc (ESucc (ESucc ByronEra))))),
--                        ESucc (ESucc (ESucc (ESucc (ESucc (ESucc (ESucc ByronEra))))))]
-- Expected: CardanoEraOnwards ShelleyEra
--   Actual: CardanoEraOnwards BabbageEra
-- In the first argument of `doInConwayOnwards', namely
--   `(unionLift BabbageEra :: CardanoEraOnwards BabbageEra)'
-- In the expression:
--   doInConwayOnwards
--     (unionLift BabbageEra :: CardanoEraOnwards BabbageEra)
-- In an equation for `it_absAA':
--     it_absAA
--       = doInConwayOnwards
--           (unionLift BabbageEra :: CardanoEraOnwards BabbageEra)

-- # # # # # # # # # # # #

-- Remarks from John:

-- 1. change representation of eras to avoid ESucc in the type errors
--    - if we want to use type family to navigate between eras, that doesn't improve error messages much
-- 2. add era type parameter to Union
-- 3. Case matching on era ranges
-- 4. Keep an eye out on compile time
