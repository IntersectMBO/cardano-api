{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api.Experimental
  ( -- * New Era interface
  --  BabbageEra
  --, ConwayEra
    AvailableEras(..)
  , Era
  , pattern CurrentEra
  , pattern UpcomingEra
  , UseEra
  , AvailableErasToSbe
  , ToConstrainedEra
  , useEra
  , protocolVersionToSbe
  ) where

import           Cardano.Api.Protocol.AvailableEras
