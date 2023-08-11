{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.Eras.InAnyEra
  ( InAnyEra(..)
  ) where

import           Data.Kind

-- | This pairs up some era-dependent type with a 'feature' value that tells
-- us what feature involves, but hides the era type.
data InAnyEra (feature :: Type -> Type) (t :: Type -> Type) where
  InAnyEra :: feature era -> t era -> InAnyEra feature t
