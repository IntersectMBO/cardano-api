-- We disable missing signature because DijkstraEra type is not exported yet
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Module providing constants for the current and experimental eras
-- used throughout the cardano-wasm library.
module Cardano.Wasm.Internal.Api.Era
  ( currentEra
  , experimentalEra
  )
where

import Cardano.Api.Experimental qualified as Exp

-- | The current era used in mainnet.
currentEra :: Exp.Era Exp.ConwayEra
currentEra = Exp.ConwayEra

-- | The experimental era, still under development or testing.
experimentalEra = Just Exp.DijkstraEra
