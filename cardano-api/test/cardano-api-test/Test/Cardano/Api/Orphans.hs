{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Orphans () where

import Cardano.Api.Shelley

import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Mary.Value qualified as L

import Data.String (IsString (..))

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

deriving instance IsString L.AssetName

deriving instance IsString (L.KeyHash r)
