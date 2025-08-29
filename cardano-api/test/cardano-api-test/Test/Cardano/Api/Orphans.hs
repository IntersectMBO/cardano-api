{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Orphans () where

import Cardano.Api.Byron
import Cardano.Api.Key

import Cardano.Crypto qualified as Crypto
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Conway.Governance (GovActionId)

import Test.QuickCheck

-- Note that we /only/ provide these Eq and Ord instances for test suites.
instance Eq Crypto.SigningKey where
  a == b = Crypto.serializeCborHash a == Crypto.serializeCborHash b

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

instance Arbitrary GovActionId

instance Arbitrary Addr
