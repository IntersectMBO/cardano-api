{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Tx.Internal.BuildTxWith
  ( BuildTxWith (..)
  , BuildTx
  , ViewTx
  , buildTxWithToMaybe
  )
where

-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data ViewTx

data BuildTx

data BuildTxWith build a where
  ViewTx :: BuildTxWith ViewTx a
  BuildTxWith :: a -> BuildTxWith BuildTx a

instance Functor (BuildTxWith build) where
  fmap _ ViewTx = ViewTx
  fmap f (BuildTxWith x) = BuildTxWith (f x)

instance Applicative (BuildTxWith ViewTx) where
  pure _ = ViewTx
  _ <*> _ = ViewTx

instance Applicative (BuildTxWith BuildTx) where
  pure = BuildTxWith
  (BuildTxWith f) <*> (BuildTxWith a) = BuildTxWith (f a)

instance Semigroup a => Semigroup (BuildTxWith build a) where
  ViewTx <> ViewTx = ViewTx
  (BuildTxWith a) <> (BuildTxWith b) = BuildTxWith (a <> b)

instance (Applicative (BuildTxWith build), Monoid a) => Monoid (BuildTxWith build a) where
  mempty = pure mempty

buildTxWithToMaybe :: BuildTxWith build a -> Maybe a
buildTxWithToMaybe ViewTx = Nothing
buildTxWithToMaybe (BuildTxWith a) = Just a

deriving instance Eq a => Eq (BuildTxWith build a)

deriving instance Show a => Show (BuildTxWith build a)
