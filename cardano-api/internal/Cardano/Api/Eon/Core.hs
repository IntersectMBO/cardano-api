{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.Core where

import           Data.Kind
import           Data.WorldPeace

data Eon (eras :: [Type]) era where
  Eon :: Contains '[era] eras => OpenUnion eras -> Eon eras era

data Byron = Byron deriving (Eq, Show)
data Shelley = Shelley deriving (Eq, Show)
data Allegra = Allegra deriving (Eq, Show)
data Mary = Mary deriving (Eq, Show)
data Alonzo = Alonzo deriving (Eq, Show)
data Babbage = Babbage deriving (Eq, Show)
data Conway = Conway deriving (Eq, Show)

type ByronOnly        = '[Byron                                                 ]
type ShelleyOnly      = '[       Shelley                                        ]
type AllegraOnly      = '[                Allegra                               ]
type MaryOnly         = '[                         Mary                         ]
type AlonzoOnly       = '[                               Alonzo                 ]
type BabbageOnly      = '[                                       Babbage        ]
type ConwayOnly       = '[                                                Conway]

type ByronOnwards     = '[Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway]
type ShelleyOnwards   = '[       Shelley, Allegra, Mary, Alonzo, Babbage, Conway]
type AllegraOnwards   = '[                Allegra, Mary, Alonzo, Babbage, Conway]
type MaryOnwards      = '[                         Mary, Alonzo, Babbage, Conway]
type AlonzoOnwards    = '[                               Alonzo, Babbage, Conway]
type BabbageOnwards   = '[                                       Babbage, Conway]
type ConwayOnwards    = '[                                                Conway]

relaxEon :: ()
  => Contains as bs
  => IsMember a bs
  => Eon as a
  -> Eon bs a
relaxEon (Eon a) = Eon (relaxOpenUnion a)

example1 :: IsMember era ByronOnwards => Eon ByronOnwards era -> Eon ByronOnwards era
example1 = relaxEon

example2 :: IsMember era ByronOnwards => Eon ShelleyOnwards era -> Eon ByronOnwards era
example2 = relaxEon
