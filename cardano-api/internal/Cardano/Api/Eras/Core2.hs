{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

import           Data.GADT.Show
import           Data.WorldPeace
-- import Data.Some


-- -- foo :: OpenUnion '[Double, Int, Bool]
-- -- foo = unionLift 42
-- --
-- sommeera :: Union CardanoEra '[ByronEra, ShelleyEra]
-- sommeera  = unionLift ByronEra

-- Type level natural numbers enabling traversing over eras on a type level
data EZero
data ESucc a

-- Give each era a type-level number
-- TODO: maybe we should get rid of Era suffixes?
type ByronEra   = EZero
type ShelleyEra = ESucc ByronEra
type AllegraEra = ESucc ShelleyEra
type MaryEra    = ESucc AllegraEra
type AlonzoEra  = ESucc MaryEra
type BabbageEra = ESucc AlonzoEra
type ConwayEra  = ESucc BabbageEra
type XEra = ESucc ConwayEra

-- Represents indefinite future era - used as a type-level boundary - not meant to be used in 'CardanoEra'
type FutureEra  = ESucc XEra

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
shelleyToMaryAllegra :: CardanoEraFromTo' ShelleyEra MaryEra AllegraEra
shelleyToMaryAllegra = unionLift AllegraEra

shelleyToMaryAllegra1 :: CardanoEraFromTo' ShelleyEra MaryEra AllegraEra
shelleyToMaryAllegra1 = unionLift MaryEra

-- similar: shelley - alonzo range
shelleyToAlonzoAlonzo :: CardanoEraFromTo' ShelleyEra AlonzoEra AlonzoEra
shelleyToAlonzoAlonzo = unionLift AlonzoEra

-- all eras from conway onwards
conwayOnwardsConway :: CardanoEraOnwards' ConwayEra ConwayEra -- Union CardanoEra '[ConwayEra, XEra, FutureEra]
conwayOnwardsConway = unionLift ConwayEra

-- You can try to retrieve CardanoEra from an unionEra
-- >>> unionMatch shelleyToAlonzoAlonzo :: Maybe (CardanoEra AlonzoEra)
-- Just AlonzoEra
-- >>> unionMatch shelleyToAlonzoAlonzo :: Maybe (CardanoEra AllegraEra)
-- Nothing


-- Represent era ranges for the type level era tags
type family EraRange e1 e2 where
  -- TODO: there are no checks if e1 <= e2
  EraRange e e = '[e]
  EraRange e1 (ESucc e2) = e1 ': EraRange (ESucc e1) (ESucc e2)

-- * handy aliases
-- denotes eras range between e1 and e2 (including e1 and e2)
type CardanoEraFromTo e1 e2 = Union CardanoEra (EraRange e1 e2)
-- eras from e1 till infinity
type CardanoEraOnwards e1 = Union CardanoEra (EraRange e1 FutureEra)
-- alias for a constraint telling that era is in range
type EraIn e e1 e2 = IsMember e (EraRange e1 e2)


-- TODO: replace with newtype
type CardanoEraFromTo' e1 e2 era = EraIn era e1 e2 => Union CardanoEra (EraRange e1 e2)
type CardanoEraOnwards' e1 era = EraIn era e1 FutureEra => Union CardanoEra (EraRange e1 FutureEra)


-- pattern matching on eras
caseMatch :: String
caseMatch = do
  let printMary       = show :: CardanoEra MaryEra -> String
      printShelly     = show :: CardanoEra ShelleyEra -> String
      printAllegraEra = show :: CardanoEra AllegraEra -> String
  absurdUnion -- this will raise compile error if you forget one era
    `unionHandle` printShelly
    `unionHandle` printMary
    `unionHandle` printAllegraEra
    $ shelleyToMaryAllegra

-- >>> caseMatch
-- "AllegraEra"

-- widen an union to include more eras
widenUnion :: String
widenUnion = do
  let shelleyToBabbage = relaxUnion shelleyToAlonzoAlonzo :: CardanoEraFromTo ShelleyEra BabbageEra
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
doInConwayOnwards :: CardanoEraOnwards ShelleyEra -> Maybe String
doInConwayOnwards era = do
  _ :: CardanoEraFromTo ShelleyEra BabbageEra <- unionShrink era
  _ :: CardanoEraOnwards ConwayEra <- unionShrink era
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
-- 2. add era type parameter to Union
-- 3. Case matching on era ranges
-- 4. Keep an eye out on compile time
