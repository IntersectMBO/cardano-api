{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Orphans () where

import Cardano.Api.Byron
import Cardano.Api.Key

import Cardano.Crypto qualified as Crypto
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Credential (Ptr (..), SlotNo32 (..), StakeReference (..))

import Data.Word

import Test.Gen.Cardano.Api.Orphans ()
import Test.Gen.Cardano.Api.Typed ()

import Test.QuickCheck

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

instance Arbitrary StakeReference where
  arbitrary = genStakeRefWith arbitrary
  shrink = genericShrink

deriving instance Arbitrary SlotNo32

instance Arbitrary Ptr where
  arbitrary = Ptr <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary L.TxIx where
  -- starting with Conway, we only deserialize TxIx within Word16 range
  arbitrary = L.TxIx . fromIntegral <$> arbitrary @Word16

instance Arbitrary L.CertIx where
  -- starting with Conway, we only deserialize CertIx within Word16 range
  arbitrary = L.CertIx . fromIntegral <$> arbitrary @Word16

genStakeRefWith :: Gen Ptr -> Gen StakeReference
genStakeRefWith genPtr =
  frequency
    [ (80, StakeRefBase <$> arbitrary)
    , (5, StakeRefPtr <$> genPtr)
    , (15, pure StakeRefNull)
    ]
