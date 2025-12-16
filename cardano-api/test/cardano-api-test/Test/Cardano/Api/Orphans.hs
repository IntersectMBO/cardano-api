{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Orphans () where

import Cardano.Api.Byron
import Cardano.Api.Key

import Test.Cardano.Crypto.Orphans ()

-- Signing Key instances

deriving instance Eq (SigningKey ByronKey)

deriving instance Eq (SigningKey PaymentKey)

deriving instance Eq (SigningKey StakeKey)

deriving instance Eq (SigningKey StakePoolKey)

deriving instance Eq (SigningKey GenesisKey)

deriving instance Eq (SigningKey GenesisDelegateKey)

deriving instance Eq (SigningKey GenesisUTxOKey)

deriving instance Eq (SigningKey KesKey)

deriving instance Eq (SigningKey VrfKey)
