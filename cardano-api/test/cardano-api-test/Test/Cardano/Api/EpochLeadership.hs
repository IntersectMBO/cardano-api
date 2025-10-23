{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.EpochLeadership
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Ledger (KeyHash (..), toCompactPartial)

import Cardano.Binary (serialize)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Api.PParams (emptyPParams)
import Cardano.Ledger.BaseTypes (Nonce (..), WithOrigin (..))
import Cardano.Ledger.Binary.Encoding (toByronCBOR)
import Cardano.Ledger.Hashes qualified as L
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.State qualified as L
import Cardano.Protocol.TPraos.API qualified as API
import Cardano.Slotting.EpochInfo (EpochInfo (..))
import Cardano.Slotting.Time (RelativeTime (..), mkSlotLength)
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.TPraos (TPraosState (..))

import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Time.Clock (secondsToNominalDiffTime)
import GHC.Exts (IsList (..))

import Hedgehog
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--
-- The list of all tests
--
tests :: TestTree
tests =
  testGroup
    "Epoch Leadership"
    [ test_currentEpochEligibleLeadershipSlots
    ]

test_currentEpochEligibleLeadershipSlots :: TestTree
test_currentEpochEligibleLeadershipSlots =
  testProperty "currentEpochEligibleLeadershipSlots happy path" $
    H.propertyOnce $ do
      let sbe = ShelleyBasedEraShelley
          sGen = shelleyGenesisDefaults
          eInfo =
            EpochInfo
              { epochInfoSize_ = const (Right (EpochSize 100))
              , epochInfoFirst_ = \(EpochNo x) -> pure $ SlotNo (x * 100)
              , epochInfoEpoch_ = \(SlotNo x) -> pure $ EpochNo (x `div` 100)
              , epochInfoSlotToRelativeTime_ = \(SlotNo x) -> pure $ RelativeTime (secondsToNominalDiffTime (fromIntegral x * 60))
              , epochInfoSlotLength_ = const (pure $ mkSlotLength 100)
              }
          pp = emptyPParams
          chainDepState = TPraosState Origin (API.initialChainDepState NeutralNonce Map.empty)
          ptclState = encodeProtocolState chainDepState
          poolid =
            StakePoolKeyHash
              { unStakePoolKeyHash = KeyHash "83c5da842d7437e411d3c4db8aaa7a7d2c1642aee932108c9857282d"
              }
          vrskey1 =
            deterministicSigningKey
              (proxyToAsType (Proxy :: Proxy VrfKey))
              (mkSeedFromBytes "V5UlALekTHL9bIbe3Yb0Kk4T49gn9smf")
          hash1 = L.toVRFVerKeyHash . unVrfKeyHash . verificationKeyHash $ getVerificationKey vrskey1
          vrskey2 =
            deterministicSigningKey
              (proxyToAsType (Proxy :: Proxy VrfKey))
              (mkSeedFromBytes "OLjPbWC6JCjSwO4lqUms0EgkinoLoIhz")
          hash2 = L.toVRFVerKeyHash . unVrfKeyHash . verificationKeyHash $ getVerificationKey vrskey2
          vrskey3 =
            deterministicSigningKey
              (proxyToAsType (Proxy :: Proxy VrfKey))
              (mkSeedFromBytes "eF0R2dENRrHM8iyb9q7puTw4y2l8e2z4")
          hash3 = L.toVRFVerKeyHash . unVrfKeyHash . verificationKeyHash $ getVerificationKey vrskey3
          poolDistr :: L.PoolDistr =
            L.PoolDistr
              { L.unPoolDistr =
                  [
                    ( KeyHash "a2927c1e43974b036d8e6838d410279266946e8a094895cfc748c91d"
                    , L.IndividualPoolStake
                        { L.individualPoolStake = 1 % 3
                        , L.individualTotalPoolStake = toCompactPartial 0
                        , L.individualPoolStakeVrf = hash1
                        }
                    )
                  ,
                    ( KeyHash "83c5da842d7437e411d3c4db8aaa7a7d2c1642aee932108c9857282d"
                    , L.IndividualPoolStake
                        { L.individualPoolStake = 1 % 3
                        , L.individualTotalPoolStake = toCompactPartial 0
                        , L.individualPoolStakeVrf = hash2
                        }
                    )
                  ,
                    ( KeyHash "362c2c2128ee75ca39690c27b42e809301231098003443669e2b03f3"
                    , L.IndividualPoolStake
                        { L.individualPoolStake = 1 % 3
                        , L.individualTotalPoolStake = toCompactPartial 0
                        , L.individualPoolStakeVrf = hash3
                        }
                    )
                  ]
              , L.pdTotalActiveStake = toCompactPartial 0
              }
          serPoolDistr = Serialised (serialize (toByronCBOR poolDistr))
          currentEpoch = EpochNo 4
          eEligibleSlots =
            currentEpochEligibleLeadershipSlots
              sbe
              sGen
              eInfo
              pp
              ptclState
              poolid
              vrskey1
              serPoolDistr
              currentEpoch
          expectedEligibleSlots = [SlotNo 406, SlotNo 432, SlotNo 437, SlotNo 443, SlotNo 484]
      eligibleSlots <- evalEither eEligibleSlots
      eligibleSlots === fromList expectedEligibleSlots
 where
  encodeProtocolState
    :: ToCBOR (Consensus.ChainDepState (ConsensusProtocol era))
    => Consensus.ChainDepState (ConsensusProtocol era)
    -> ProtocolState era
  encodeProtocolState cds = ProtocolState (Serialised pbs)
   where
    pbs = serialize (toCBOR cds)
