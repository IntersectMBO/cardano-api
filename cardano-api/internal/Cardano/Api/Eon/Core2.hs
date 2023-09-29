{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Eon.Core2 where

data E = Y | N deriving (Eq, Show)

type Byron              = '[Y, N, N, N, N, N, N]
type Shelley            = '[N, Y, N, N, N, N, N]
type Allegra            = '[N, N, Y, N, N, N, N]
type Mary               = '[N, N, N, Y, N, N, N]
type Alonzo             = '[N, N, N, N, Y, N, N]
type Babbage            = '[N, N, N, N, N, Y, N]
type Conway             = '[N, N, N, N, N, N, Y]

type ByronOnwards       = '[Y, Y, Y, Y, Y, Y, Y]
type ShelleyOnwards     = '[N, Y, Y, Y, Y, Y, Y]
type AllegraOnwards     = '[N, N, Y, Y, Y, Y, Y]
type MaryOnwards        = '[N, N, N, Y, Y, Y, Y]
type AlonzoOnwards      = '[N, N, N, N, Y, Y, Y]
type BabbageOnwards     = '[N, N, N, N, N, Y, Y]
type ConwayOnwards      = '[N, N, N, N, N, N, Y]

type family Intersect (as :: [E]) (bs :: [E]) :: [E] where
  Intersect '[] '[] = '[]
  Intersect (Y ': as) (Y ': bs) = Y ': Intersect as bs
  Intersect (a ': as) (b ': bs) = N ': Intersect as bs

type family Union (as :: [E]) (bs :: [E]) :: [E] where
  Union '[] '[] = '[]
  Union (Y ': as) (b ': bs) = Y ': Union as bs
  Union (a ': as) (Y ': bs) = Y ': Union as bs
  Union (N ': as) (N ': bs) = N ': Union as bs

type family From (as :: [E]) :: [E] where
  From '[] = '[]
  From (Y ': a ': as) = Y ': From (Y ': as)
  From (N ': b ': as) = N ': From (b ': as)

type family ReverseAcc (as :: [E]) (bs :: [E]) :: [E] where
  ReverseAcc '[]       bs = bs
  ReverseAcc (a ': as) bs = ReverseAcc as (a ': bs)

type family Reverse (as :: [E]) :: [E] where
  Reverse as = ReverseAcc as '[]

type family Until (as :: [E]) :: [E] where
  Until as = Reverse (From (Reverse as))
