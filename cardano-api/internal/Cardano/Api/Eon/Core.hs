{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.Core where

import           Data.Kind

data Byron = Byron deriving (Eq, Show)
data Shelley = Shelley deriving (Eq, Show)
data Allegra = Allegra deriving (Eq, Show)
data Mary = Mary deriving (Eq, Show)
data Alonzo = Alonzo deriving (Eq, Show)
data Babbage = Babbage deriving (Eq, Show)
data Conway = Conway deriving (Eq, Show)

type ByronOnly          = '[Byron                                                 ]
type ShelleyOnly        = '[       Shelley                                        ]
type AllegraOnly        = '[                Allegra                               ]
type MaryOnly           = '[                         Mary                         ]
type AlonzoOnly         = '[                               Alonzo                 ]
type BabbageOnly        = '[                                       Babbage        ]
type ConwayOnly         = '[                                                Conway]

type ByronEraOnwards    = '[Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway]
type ShelleyEraOnwards  = '[       Shelley, Allegra, Mary, Alonzo, Babbage, Conway]
type AllegraEraOnwards  = '[                Allegra, Mary, Alonzo, Babbage, Conway]
type MaryEraOnwards     = '[                         Mary, Alonzo, Babbage, Conway]
type AlonzoEraOnwards   = '[                               Alonzo, Babbage, Conway]
type BabbageEraOnwards  = '[                                       Babbage, Conway]
type ConwayEraOnwards   = '[                                                Conway]

type ByronToBabbage     = '[Byron, Shelley, Allegra, Mary, Alonzo, Babbage]
type ShelleyToBabbage   = '[       Shelley, Allegra, Mary, Alonzo, Babbage]
type AllegraToBabbage   = '[                Allegra, Mary, Alonzo, Babbage]
type MaryToBabbage      = '[                         Mary, Alonzo, Babbage]
type AlonzoToBabbage    = '[                               Alonzo, Babbage]

type ByronToAlonzo      = '[Byron, Shelley, Allegra, Mary, Alonzo]
type ShelleyToAlonzo    = '[       Shelley, Allegra, Mary, Alonzo]
type AllegraToAlonzo    = '[                Allegra, Mary, Alonzo]
type MaryToAlonzo       = '[                         Mary, Alonzo]

type ByronToMary        = '[Byron, Shelley, Allegra, Mary]
type ShelleyToMary      = '[       Shelley, Allegra, Mary]
type AllegraToMary      = '[                Allegra, Mary]

type ByronToAllegra     = '[Byron, Shelley, Allegra]
type ShelleyToAllegra   = '[       Shelley, Allegra]

type ByronToShelley     = '[Byron, Shelley]

data Elem t where
  Elem :: Elem t

data (∈) (a :: Type) (as :: [Type]) where
  MemberHead :: a ∈ (a ': as)
  MemberTail :: a ∈ as -> a ∈ (b ': as)

data (⊆) (as :: [Type]) (bs :: [Type]) where
  SubsetCons
    :: a ∈ bs
    -> as ⊆ bs
    -> (a:as) ⊆ bs

  SubsetNil
    :: '[] ⊆ bs

data Eon (eras :: [Type]) era where
  Eon
    :: era ∈ eras
    -> era
    -> Eon eras era

class Member (a :: Type) (as :: [Type]) where
  member :: a ∈ as

instance Member a (a ': as) where
  member = MemberHead

instance Member a as => Member a (b ': as) where
  member = MemberTail member

class Subset (as :: [Type]) (bs :: [Type]) where
  subset :: as ⊆ bs

instance Subset '[] bs where
  subset = SubsetNil

-- instance (Member a as, Subset as bs) => Subset (a ': as) bs where
--   subset = SubsetCons member subset

example1 :: ()
  => Eon ConwayEraOnwards era
  -> Eon ConwayEraOnwards era
example1 (Eon m era) = Eon m era

example2 :: ()
  => Eon ConwayEraOnwards era
  -> Eon BabbageEraOnwards era
example2 (Eon m era) =
  case m of
    MemberHead -> Eon (MemberTail MemberHead) era
    MemberTail m' -> case m' of {}

example3 :: ()
  => Eon ConwayEraOnwards era
  -> Eon AlonzoEraOnwards era
example3 (Eon m era) =
  case m of
    MemberHead -> Eon (MemberTail (MemberTail MemberHead)) era
    MemberTail m' -> case m' of {}

example4 :: ()
  => Eon ByronOnly era
  -> Eon ByronToShelley era
example4 (Eon m era) =
  case m of
    MemberHead -> Eon MemberHead era
    MemberTail m' -> case m' of {}

example5 :: ()
  => Eon ShelleyOnly era
  -> Eon ByronToAllegra era
example5 (Eon m era) =
  case m of
    MemberHead -> Eon (MemberTail MemberHead) era
    MemberTail m' -> case m' of {}

example6 :: ()
  => Eon ShelleyToAllegra era
  -> Eon ByronToAllegra era
example6 (Eon m era) =
  case m of
    MemberHead -> Eon (MemberTail MemberHead) era
    MemberTail MemberHead -> Eon (MemberTail (MemberTail MemberHead)) era
    MemberTail (MemberTail m') -> case m' of {}

example7 :: ()
  => ShelleyToAllegra ⊆ ByronToAllegra
  -> Eon ShelleyToAllegra era
  -> Eon ByronToAllegra era
example7 _ (Eon m era) =
  case m of
    MemberHead -> Eon (MemberTail MemberHead) era
    MemberTail MemberHead -> Eon (MemberTail (MemberTail MemberHead)) era
    MemberTail (MemberTail m') -> case m' of {}

moo :: ShelleyToAllegra ⊆ ByronToAllegra -> ()
moo p = case p of
  SubsetCons _pShelley p' -> case p' of
    SubsetCons _pAllegra p'' -> case p'' of
      SubsetNil -> ()

example8 :: ()
  => ShelleyToAllegra ⊆ ByronToAllegra
  -> Eon ShelleyToAllegra era
  -> Eon ByronToAllegra era
example8 p (Eon m era) =
  case m of
    MemberHead ->
      case p of
        SubsetCons pShelley _ ->
          Eon pShelley era
    MemberTail MemberHead ->
      case p of
        SubsetCons _ p' -> case p' of
          SubsetCons pAllegra _ -> Eon pAllegra era
    MemberTail (MemberTail m') ->
      case m' of {}


-- data (∈) (a :: Type) (as :: [Type]) where
--   MemberHead :: a ∈ (a ': as)
--   MemberTail :: a ∈ as -> a ∈ (b ': as)

memberSubsetEx1 :: ()
  => Byron      ∈ ByronOnly
  -> ByronOnly  ⊆ ByronToShelley
  -> Byron      ∈ ByronToShelley
memberSubsetEx1 mp sp =
  case mp of
    MemberHead ->
      case sp of
        SubsetCons pm _ ->
          pm
    MemberTail mz ->
      case mz of {}

memberSubsetEx2 :: ()
  => Shelley     ∈ ShelleyOnly
  -> ShelleyOnly ⊆ ByronToShelley
  -> Shelley     ∈ ByronToShelley
memberSubsetEx2 mp sp =
  case mp of
    MemberHead ->
      case sp of
        SubsetCons pm _ ->
          pm
    MemberTail mz ->
      case mz of {}

-- memberSubset :: ()
--   => a  ∈ as
--   -> as ⊆ bs
--   -> a  ∈ bs
-- memberSubset mp sp =
--   case mp of
--     MemberHead ->
--       case sp of
--         SubsetCons pm _ ->
--           pm
--     MemberTail mpm ->
--       memberSubset mpm sp
--       -- case sp of
--       --   SubsetCons _ ssn ->


relaxEon :: ()
  => as ⊆ bs
  -> Eon as a
  -> Eon bs a
relaxEon p (Eon m era) =
  case m of
    MemberHead ->
      case p of
        SubsetCons pm _ ->
          Eon pm era
    MemberTail mpm ->
      case p of
        SubsetCons _ ssn ->
          relaxEon ssn (Eon mpm era)

-- example2 :: ()
--   => IsMember era ByronEraOnwards -- These constraints are due to relaxEon.  They shouldn't be needed.
--   => Eon ShelleyEraOnwards era
--   -> Eon ByronEraOnwards era
-- example2 = relaxEon
