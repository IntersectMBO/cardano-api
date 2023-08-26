{-# LANGUAGE GADTs #-}

module Cardano.Api.Eras.Mandatory
  ( Mandatory(..)
  ) where

import           Cardano.Api.Eras.HasComplement

data Mandatory whole feature era a where
  Mandatory :: feature era -> a -> Mandatory whole feature era a
  Absent :: Complement whole feature era -> () -> Mandatory whole feature era a
