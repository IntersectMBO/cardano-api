{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

type family Subset (as :: [E]) (bs :: [E]) :: Bool where
  Subset '[] '[] = 'True
  Subset (Y ': as) (N ': bs) = 'False
  Subset (a ': as) (a ': bs) = Subset as bs

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

type family Minus (as :: [E]) (bs :: [E]) :: [E] where
  Minus '[] '[] = '[]
  Minus (a ': as) (Y ': bs) = N ': Minus as bs
  Minus (a ': as) (N ': bs) = a ': Minus as bs

data SubsetProof (as :: [E]) (bs :: [E]) where
  SubsetProof :: IsSubset as bs => SubsetProof as bs

class IsSubset (as :: [E]) (bs :: [E]) where
  subsetProof :: SubsetProof as bs

instance IsSubset '[] '[] where
  subsetProof = SubsetProof

instance IsSubset as bs => IsSubset (a ': as) (Y ': bs) where
  subsetProof = SubsetProof

instance {-# OVERLAPPING #-} IsSubset as bs => IsSubset (N ': as) (Y ': bs) where
  subsetProof = SubsetProof

data Era (eon :: [E]) where
  Byron   :: SubsetProof Byron   eon -> Era eon
  Shelley :: SubsetProof Shelley eon -> Era eon
  Allegra :: SubsetProof Allegra eon -> Era eon
  Mary    :: SubsetProof Mary    eon -> Era eon
  Alonzo  :: SubsetProof Alonzo  eon -> Era eon
  Babbage :: SubsetProof Babbage eon -> Era eon
  Conway  :: SubsetProof Conway  eon -> Era eon

data Eon (eras :: [E]) era where
  Eon :: Era era -> Eon eras era

relaxW :: SubsetProof as bs -> Eon as a -> Eon bs a
relaxW _ (Eon a) = Eon a

relax :: forall bs as a. IsSubset as bs => Eon as a -> Eon bs a
relax (Eon a) = relaxW @as @bs subsetProof (Eon a)

-- subset :: forall as bs. Proxy (as :: [E]) -> Proxy (bs :: [E]) -> Bool
-- subset Proxy Proxy = demote @(Subset as bs :: Bool)

-- case2 :: forall (bs :: [E]) (as :: [E]) era r eon. IsSubset bs as => (Eon bs era -> r) -> (Eon (Minus as bs) era -> r) -> Eon as era -> r
-- case2 l r (Eon a) =
--   if demote @(Subset as bs :: Bool)
--     then l (Eon a)
--     else r (Eon a)

example1 :: Eon ByronOnwards Byron -> Eon ByronOnwards Byron
example1 = relax

example2 :: Eon ShelleyOnwards Byron -> Eon ByronOnwards Byron
example2 = relax

-- -- Fails with: No instance for (Subset ByronOnwards ShelleyOnwards)
-- example3 :: Eon ByronOnwards Byron -> Eon ShelleyOnwards Byron
-- example3 = relax
