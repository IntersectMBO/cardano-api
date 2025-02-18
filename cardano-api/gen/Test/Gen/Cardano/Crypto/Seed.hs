module Test.Gen.Cardano.Crypto.Seed
  ( genSeed
  , genSeedForKey
  )
where

import Cardano.Api (AsType, Key)
import Cardano.Api qualified as API

import Cardano.Crypto.Seed (Seed)
import Cardano.Crypto.Seed qualified as C

import Hedgehog (MonadGen, Range)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R

genSeed :: MonadGen m => Range Int -> m Seed
genSeed r = C.mkSeedFromBytes <$> G.bytes r

genSeedForKey :: (Key key, MonadGen m) => AsType key -> m Seed
genSeedForKey keyRole = genSeed (R.singleton (fromIntegral (API.deterministicSigningKeySeedSize keyRole)))
