module Main (main) where

import Cardano.Api.Experimental qualified as Exp

main :: IO ()
main =
  print Exp.ConwayEra
