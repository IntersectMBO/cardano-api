{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Gen.Cardano.Api.Orphans
  ( 
  )
where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams)

import Data.Functor.Identity (Identity)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Common (Arbitrary (..))
import Test.Cardano.Ledger.Conway.Arbitrary ()


instance Arbitrary (DijkstraPParams Identity DijkstraEra) where
  arbitrary = genericArbitraryU

instance Arbitrary (DijkstraPParams StrictMaybe DijkstraEra) where
  arbitrary = genericArbitraryU