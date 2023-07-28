{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Orphans () where

import           Cardano.Binary (DecoderError (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Protocol.TPraos.API as Ledger
import           Cardano.Protocol.TPraos.BHeader (HashHeader (..))
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as Ledger
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Ledger
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Protocol.Praos (PraosState)
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import           Ouroboros.Consensus.Protocol.TPraos (TPraosState)
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus
import           Ouroboros.Network.Block (HeaderHash, Tip (..))

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Read as CBOR
import           Data.Aeson (KeyValue ((.=)), ToJSON (..), object, pairs, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import           Data.Data (Data)
import qualified Data.Text.Encoding as Text

deriving instance Data DecoderError
deriving instance Data CBOR.DeserialiseFailure
deriving instance Data Bech32.DecodingError
deriving instance Data Bech32.CharPosition

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance Crypto.Crypto crypto => ToJSON (Consensus.StakeSnapshots crypto) where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair :: (Aeson.KeyValue a, Crypto.Crypto crypto) => Consensus.StakeSnapshots crypto -> [a]
stakeSnapshotsToPair Consensus.StakeSnapshots
    { Consensus.ssStakeSnapshots
    , Consensus.ssMarkTotal
    , Consensus.ssSetTotal
    , Consensus.ssGoTotal
    } =
    [ "pools" .= ssStakeSnapshots
    , "total" .= object
      [ "stakeMark" .= ssMarkTotal
      , "stakeSet" .= ssSetTotal
      , "stakeGo" .= ssGoTotal
      ]
    ]

instance ToJSON (Consensus.StakeSnapshot crypto) where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue a => Consensus.StakeSnapshot crypto -> [a]
stakeSnapshotToPair Consensus.StakeSnapshot
    { Consensus.ssMarkPool
    , Consensus.ssSetPool
    , Consensus.ssGoPool
    } =
    [ "stakeMark" .= ssMarkPool
    , "stakeSet" .= ssSetPool
    , "stakeGo" .= ssGoPool
    ]

instance ToJSON (OneEraHash xs) where
  toJSON = toJSON
         . Text.decodeLatin1
         . Base16.encode
         . SBS.fromShort
         . getOneEraHash

deriving newtype instance ToJSON ByronHash

-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = Aeson.object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    Aeson.object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance CC.Crypto crypto => ToJSON (ShelleyHash crypto)
deriving newtype instance CC.Crypto crypto => ToJSON (HashHeader crypto)

deriving instance ToJSON (Ledger.PrtclState StandardCrypto)
deriving instance ToJSON Ledger.TicknState
deriving instance ToJSON (Ledger.ChainDepState StandardCrypto)

instance ToJSON (TPraosState StandardCrypto) where
  toJSON s = Aeson.object
    [ "lastSlot" .= Consensus.tpraosStateLastSlot s
    , "chainDepState" .= Consensus.tpraosStateChainDepState s
    ]

instance ToJSON (PraosState StandardCrypto) where
  toJSON s = Aeson.object
    [ "lastSlot" .= Consensus.praosStateLastSlot s
    , "oCertCounters" .= Consensus.praosStateOCertCounters s
    , "evolvingNonce" .= Consensus.praosStateEvolvingNonce s
    , "candidateNonce" .= Consensus.praosStateCandidateNonce s
    , "epochNonce" .= Consensus.praosStateEpochNonce s
    , "labNonce" .= Consensus.praosStateLabNonce s
    , "lastEpochBlockNonce" .= Consensus.praosStateLastEpochBlockNonce s
    ]
