{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Use camelCase" -}

module Cardano.Api.Eon.Core3 where

import           Data.Coerce

import           GDP

newtype Rev xs = Dev Defn

type Defining f = (Coercible Defn f, Coercible f Defn)

reverse :: ([a] ~~ xs) -> ([a] ~~ Rev xs)
reverse xs = defn (Prelude.reverse (the xs))

rev_rev :: Proof (Rev (Rev xs) == xs)
rev_rev = axiom

newtype Union a b = Union Defn
newtype Subset a b = Subset Defn

type (∪) = Union
type (⊆) = Subset

subset_tr :: Proof (a ⊆ b) -> Proof (b ⊆ c) -> Proof (b ⊆ c)
subset_tr _ _ = axiom
