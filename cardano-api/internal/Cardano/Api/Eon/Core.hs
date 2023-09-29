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

type ByronEraOnly       = '[Byron                                                 ]
type ShelleyEraOnly     = '[       Shelley                                        ]
type AllegraEraOnly     = '[                Allegra                               ]
type MaryEraOnly        = '[                         Mary                         ]
type AlonzoEraOnly      = '[                               Alonzo                 ]
type BabbageEraOnly     = '[                                       Babbage        ]
type ConwayEraOnly      = '[                                                Conway]

type ByronEraOnwards    = '[Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway]
type ShelleyEraOnwards  = '[       Shelley, Allegra, Mary, Alonzo, Babbage, Conway]
type AllegraEraOnwards  = '[                Allegra, Mary, Alonzo, Babbage, Conway]
type MaryEraOnwards     = '[                         Mary, Alonzo, Babbage, Conway]
type AlonzoEraOnwards   = '[                               Alonzo, Babbage, Conway]
type BabbageEraOnwards  = '[                                       Babbage, Conway]
type ConwayEraOnwards   = '[                                                Conway]

relaxEon :: ()
  => Contains as bs
  => IsMember a bs
  => Eon as a
  -> Eon bs a
relaxEon (Eon a) = Eon (relaxOpenUnion a)

example1 :: IsMember era ByronEraOnwards => Eon ByronEraOnwards era -> Eon ByronEraOnwards era
example1 = relaxEon

example2 :: IsMember era ByronEraOnwards => Eon ShelleyEraOnwards era -> Eon ByronEraOnwards era
example2 = relaxEon
