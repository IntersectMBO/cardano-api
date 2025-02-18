{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Internal.Eon.Convert
  ( Convert (..)
  )
where

import Data.Kind (Type)

-- | The Convert class is aimed at exposing a single interface that lets us
-- convert between eons. However this is generalizable to any injective
-- relationship between types.
class Convert (f :: a -> Type) (g :: a -> Type) where
  convert :: forall era. f era -> g era
