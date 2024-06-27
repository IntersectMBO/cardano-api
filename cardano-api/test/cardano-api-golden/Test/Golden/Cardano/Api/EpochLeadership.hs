{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Cardano.Api.EpochLeadership
  ( test_golden_currentEpochEligibleLeadershipSlots
  ) where

import           Cardano.Api (deterministicSigningKey)
import           Cardano.Api.Block (EpochNo (..), Hash (StakePoolKeyHash), SlotNo (..))
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import           Cardano.Api.Genesis (shelleyGenesisDefaults)
import           Cardano.Api.GenesisParameters (EpochSize (..))
import           Cardano.Api.Ledger (KeyHash (..), StandardCrypto)
import           Cardano.Api.LedgerState (currentEpochEligibleLeadershipSlots)
import           Cardano.Api.Query (ProtocolState (..),
                   SerialisedPoolDistribution (SerialisedPoolDistribution))
import           Cardano.Api.Shelley (VrfKey, proxyToAsType, unStakePoolKeyHash)

import           Cardano.Binary (serialize)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import           Cardano.Ledger.Api.PParams (emptyPParams)
import           Cardano.Ledger.Binary.Encoding (toByronCBOR)
import           Cardano.Slotting.EpochInfo (EpochInfo (..))
import           Cardano.Slotting.Time (RelativeTime (..), mkSlotLength)
import           Ouroboros.Consensus.Shelley.Ledger.Query.Types (PoolDistr (..))
import           Ouroboros.Network.Block (Serialised (..))

import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToNominalDiffTime)

import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

test_golden_currentEpochEligibleLeadershipSlots :: TestTree
test_golden_currentEpochEligibleLeadershipSlots = testProperty "golden EpochLeadership" $
  H.property $ do
    let sbe = ShelleyBasedEraShelley
        sGen = shelleyGenesisDefaults
        eInfo = EpochInfo { epochInfoSize_ = const (Right (EpochSize 10))
                          , epochInfoFirst_ = \(EpochNo x) -> pure $ SlotNo (x * 10)
                          , epochInfoEpoch_ = \(SlotNo x) -> pure $ EpochNo (x `div` 10)
                          , epochInfoSlotToRelativeTime_ = \(SlotNo x) -> pure $ RelativeTime (secondsToNominalDiffTime (fromIntegral x * 10))
                          , epochInfoSlotLength_ = const (pure $ mkSlotLength 10)
                          }
        pp = emptyPParams
        ptclState = ProtocolState (Serialised "dummyProtocolState")
        poolid = StakePoolKeyHash { unStakePoolKeyHash = KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8" }
        vrskey = deterministicSigningKey (proxyToAsType (Proxy :: Proxy VrfKey)) (mkSeedFromBytes "")
        poolDistr :: PoolDistr StandardCrypto = PoolDistr Map.empty
        serPoolDistr = SerialisedPoolDistribution (Serialised (serialize (toByronCBOR poolDistr)))
        currentEpoch = EpochNo 4
        eEligibileSlots = currentEpochEligibleLeadershipSlots sbe sGen eInfo pp ptclState poolid vrskey serPoolDistr currentEpoch
        expectedEligibleSlots = [SlotNo 2, SlotNo 6]
    eligibileSlots <- H.evalEither eEligibileSlots
    eligibileSlots H.=== Set.fromList expectedEligibleSlots
