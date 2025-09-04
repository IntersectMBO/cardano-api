{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Golden.Cardano.Api.Genesis
  ( exampleShelleyGenesis
  )
where

import Cardano.Api.Genesis

import Cardano.Crypto.VRF (VerKeyVRF)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Network (..), knownNonZeroBounded)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
  ( Credential (..)
  , PaymentCredential
  , StakeCredential
  , StakeReference (..)
  )
import Cardano.Ledger.Keys (GenDelegPair (..))
import Cardano.Ledger.Shelley.Genesis (emptyGenesisStaking)
import Cardano.Protocol.Crypto (StandardCrypto)

import Data.ListMap (ListMap (ListMap))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Exts (IsList (..))
import Lens.Micro

exampleShelleyGenesis :: ShelleyGenesis
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer)
    , sgNetworkMagic = 4036000900
    , sgNetworkId = Testnet
    , sgActiveSlotsCoeff = unsafeBoundedRational 0.259
    , sgSecurityParam = knownNonZeroBounded @120842
    , sgEpochLength = EpochSize 1215
    , sgSlotsPerKESPeriod = 8541
    , sgMaxKESEvolutions = 28899
    , sgSlotLength = 8
    , sgUpdateQuorum = 16991
    , sgMaxLovelaceSupply = 71
    , sgProtocolParams =
        emptyPParams
          & ppDL .~ unsafeBoundedRational 1.9e-2
          & ppMaxBBSizeL .~ 65535
          & ppMaxBHSizeL .~ 65535
    , sgGenDelegs =
        fromList
          [
            ( genesisVerKeyHash
            , GenDelegPair delegVerKeyHash (toVRFVerKeyHash delegVrfKeyHash)
            )
          ]
    , sgInitialFunds = ListMap [(initialFundedAddress, initialFunds)]
    , sgStaking = emptyGenesisStaking
    }
 where
  -- hash of the genesis verification key
  genesisVerKeyHash :: KeyHash Genesis
  genesisVerKeyHash = KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
  -- hash of the delegators verification key
  delegVerKeyHash :: KeyHash GenesisDelegate
  delegVerKeyHash = KeyHash "839b047f56e50654bdb504832186dc1ee0c73c8de2daec7ae6273827"
  delegVrfKeyHash :: Hash HASH (VerKeyVRF StandardCrypto)
  delegVrfKeyHash = "231391e7ec1c450a8518134cf6fad1a8e0ed7ffd66d740f8e8271347a6de7bf2"
  initialFundedAddress :: Addr
  initialFundedAddress = Addr Testnet paymentCredential (StakeRefBase stakingCredential)
   where
    paymentCredential :: PaymentCredential
    paymentCredential =
      KeyHashObj $
        KeyHash
          "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"

    stakingCredential :: StakeCredential
    stakingCredential =
      KeyHashObj $
        KeyHash
          "e37a65ea2f9bcefb645de4312cf13d8ac12ae61cf242a9aa2973c9ee"

  initialFunds :: Coin
  initialFunds = Coin 12157196
