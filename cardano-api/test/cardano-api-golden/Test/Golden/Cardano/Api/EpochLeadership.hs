{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Golden.Cardano.Api.EpochLeadership
  ( test_golden_currentEpochEligibleLeadershipSlots
  ) where

import           Cardano.Api (Key (verificationKeyHash), deterministicSigningKey,
                   getVerificationKey)
import           Cardano.Api.Block (EpochNo (..), Hash (StakePoolKeyHash), SlotNo (..))
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import           Cardano.Api.Genesis (shelleyGenesisDefaults)
import           Cardano.Api.GenesisParameters (EpochSize (..))
import           Cardano.Api.Ledger (KeyHash (..), StandardCrypto, toCBOR)
import           Cardano.Api.LedgerState (currentEpochEligibleLeadershipSlots)
import           Cardano.Api.Modes (ConsensusProtocol)
import           Cardano.Api.Query (ProtocolState (..),
                   SerialisedPoolDistribution (SerialisedPoolDistribution))
import           Cardano.Api.Shelley (Hash (VrfKeyHash), VrfKey, proxyToAsType, unStakePoolKeyHash)

import           Cardano.Binary (ToCBOR, serialize)
import           Cardano.Crypto.Seed (mkSeedFromBytes)
import           Cardano.Ledger.Api.PParams (emptyPParams)
import           Cardano.Ledger.BaseTypes (Nonce (..), WithOrigin (..))
import           Cardano.Ledger.Binary.Encoding (toByronCBOR)
import qualified Cardano.Protocol.TPraos.API as API
import           Cardano.Slotting.EpochInfo (EpochInfo (..))
import           Cardano.Slotting.Time (RelativeTime (..), mkSlotLength)
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import           Ouroboros.Consensus.Protocol.TPraos (TPraosState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Query.Types (IndividualPoolStake (..),
                   PoolDistr (..))
import           Ouroboros.Network.Block (Serialised (..))

import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Data.Ratio ((%))
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToNominalDiffTime)

import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

encodeProtocolState
  :: ToCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  => Consensus.ChainDepState (ConsensusProtocol era)
  -> ProtocolState era
encodeProtocolState cds = ProtocolState (Serialised pbs)
  where pbs = serialize (toCBOR cds)

test_golden_currentEpochEligibleLeadershipSlots :: TestTree
test_golden_currentEpochEligibleLeadershipSlots =
  testProperty "golden EpochLeadership" $
  H.property $ do
    let sbe = ShelleyBasedEraShelley
        sGen = shelleyGenesisDefaults
        eInfo = EpochInfo { epochInfoSize_ = const (Right (EpochSize 100))
                          , epochInfoFirst_ = \(EpochNo x) -> pure $ SlotNo (x * 100)
                          , epochInfoEpoch_ = \(SlotNo x) -> pure $ EpochNo (x `div` 100)
                          , epochInfoSlotToRelativeTime_ = \(SlotNo x) -> pure $ RelativeTime (secondsToNominalDiffTime (fromIntegral x * 60))
                          , epochInfoSlotLength_ = const (pure $ mkSlotLength 100)
                          }
        pp = emptyPParams
        chainDepState = TPraosState Origin (API.initialChainDepState NeutralNonce Map.empty)
        ptclState = encodeProtocolState chainDepState
        poolid = StakePoolKeyHash { unStakePoolKeyHash = KeyHash "83c5da842d7437e411d3c4db8aaa7a7d2c1642aee932108c9857282d" }
        vrskey1 = deterministicSigningKey (proxyToAsType (Proxy :: Proxy VrfKey)) (mkSeedFromBytes "V5UlALekTHL9bIbe3Yb0Kk4T49gn9smf")
        VrfKeyHash hash1 = verificationKeyHash $ getVerificationKey vrskey1
        vrskey2 = deterministicSigningKey (proxyToAsType (Proxy :: Proxy VrfKey)) (mkSeedFromBytes "OLjPbWC6JCjSwO4lqUms0EgkinoLoIhz")
        VrfKeyHash hash2 = verificationKeyHash $ getVerificationKey vrskey2
        vrskey3 = deterministicSigningKey (proxyToAsType (Proxy :: Proxy VrfKey)) (mkSeedFromBytes "eF0R2dENRrHM8iyb9q7puTw4y2l8e2z4")
        VrfKeyHash hash3 = verificationKeyHash $ getVerificationKey vrskey3
        poolDistr :: PoolDistr StandardCrypto = PoolDistr $
          Map.fromList [ ( KeyHash "a2927c1e43974b036d8e6838d410279266946e8a094895cfc748c91d"
                         , IndividualPoolStake { individualPoolStake = 1 % 3
                                               , individualPoolStakeVrf = hash1
                                               }
                         )
                       , ( KeyHash "83c5da842d7437e411d3c4db8aaa7a7d2c1642aee932108c9857282d"
                         , IndividualPoolStake { individualPoolStake = 1 % 3
                                               , individualPoolStakeVrf = hash2
                                               }
                         )
                       , ( KeyHash "362c2c2128ee75ca39690c27b42e809301231098003443669e2b03f3"
                         , IndividualPoolStake { individualPoolStake = 1 % 3
                                               , individualPoolStakeVrf = hash3
                                               }
                         )
                       ]
        serPoolDistr = SerialisedPoolDistribution (Serialised (serialize (toByronCBOR poolDistr)))
        currentEpoch = EpochNo 4
        eEligibileSlots = currentEpochEligibleLeadershipSlots sbe sGen eInfo pp ptclState poolid vrskey1 serPoolDistr currentEpoch
        expectedEligibleSlots = [ SlotNo 406, SlotNo 432, SlotNo 437, SlotNo 443, SlotNo 484 ]
    eligibileSlots <- H.evalEither eEligibileSlots
    eligibileSlots H.=== Set.fromList expectedEligibleSlots
