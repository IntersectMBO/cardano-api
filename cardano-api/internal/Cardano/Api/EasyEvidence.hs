{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Api.EasyEvidence
  ( EasyEvidence(..)
  ) where

import           Cardano.Api.Eras.Core

-- | This is a convenience class for accessing the various
-- Eon related types.
class EasyEvidence f era where
  easyEvidence :: CardanoEra era -> Maybe (f era)
