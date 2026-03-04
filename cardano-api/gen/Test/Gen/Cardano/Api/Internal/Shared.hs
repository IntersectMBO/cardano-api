{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Gen.Cardano.Api.Internal.Shared 
  ( genCostModels
  , genEpochNo
  , genSeed
  , genSigningKey
  , genVerificationKey
  , genVerificationKeyHash
  )
  where

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Cardano.Api.Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Api
import Hedgehog.Gen.QuickCheck qualified as Q
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto 
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 10)

genCostModels :: MonadGen m => m Alonzo.CostModels
genCostModels = Q.arbitrary

genVerificationKeyHash
  :: ()
  => HasTypeProxy keyrole
  => Key keyrole
  => AsType keyrole
  -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken


genVerificationKey
  :: ()
  => HasTypeProxy keyrole
  => Key keyrole
  => AsType keyrole
  -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
  seed <- genSeed (fromIntegral seedSize)
  let sk = deterministicSigningKey roletoken seed
  return sk
 where
  seedSize :: Word
  seedSize = deterministicSigningKeySeedSize roletoken


genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)
