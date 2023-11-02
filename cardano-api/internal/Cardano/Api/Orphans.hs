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
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Babbage.PParams as Ledger
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Conway.PParams as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.HKD (NoUpdate (..))
import qualified Cardano.Ledger.Shelley.PParams as Ledger
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
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Monoid
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

stakeSnapshotsToPair :: (Aeson.KeyValue e a, Crypto.Crypto crypto) => Consensus.StakeSnapshots crypto -> [a]
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

stakeSnapshotToPair :: Aeson.KeyValue e a => Consensus.StakeSnapshot crypto -> [a]
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



-- We wrap the individual records with Last and use Last's Semigroup instance.
-- In this instance we take the last 'Just' value or the only 'Just' value
instance Semigroup (Ledger.ShelleyPParams StrictMaybe era) where
  (<>) pp1 pp2 =
    let fsppMinFeeA = lastMappendWith Ledger.sppMinFeeA pp1 pp2
        fsppMinFeeB = lastMappendWith Ledger.sppMinFeeB pp1 pp2
        fsppMaxBBSize = lastMappendWith Ledger.sppMaxBBSize pp1 pp2
        fsppMaxTxSize = lastMappendWith Ledger.sppMaxTxSize pp1 pp2
        fsppMaxBHSize = lastMappendWith Ledger.sppMaxBHSize pp1 pp2
        fsppKeyDeposit = lastMappendWith Ledger.sppKeyDeposit pp1 pp2
        fsppPoolDeposit = lastMappendWith Ledger.sppPoolDeposit pp1 pp2
        fsppEMax = lastMappendWith Ledger.sppEMax pp1 pp2
        fsppNOpt = lastMappendWith Ledger.sppNOpt pp1 pp2
        fsppA0 = lastMappendWith Ledger.sppA0 pp1 pp2
        fsppRho = lastMappendWith Ledger.sppRho pp1 pp2
        fsppTau = lastMappendWith Ledger.sppTau pp1 pp2
        fsppD = lastMappendWith Ledger.sppD pp1 pp2
        fsppExtraEntropy = lastMappendWith Ledger.sppExtraEntropy pp1 pp2
        fsppProtocolVersion = lastMappendWith Ledger.sppProtocolVersion pp1 pp2
        fsppMinUTxOValue = lastMappendWith Ledger.sppMinUTxOValue pp1 pp2
        fsppMinPoolCost = lastMappendWith Ledger.sppMinPoolCost pp1 pp2
    in Ledger.ShelleyPParams
         { Ledger.sppMinFeeA = fsppMinFeeA
         , Ledger.sppMinFeeB = fsppMinFeeB
         , Ledger.sppMaxBBSize = fsppMaxBBSize
         , Ledger.sppMaxTxSize = fsppMaxTxSize
         , Ledger.sppMaxBHSize = fsppMaxBHSize
         , Ledger.sppKeyDeposit = fsppKeyDeposit
         , Ledger.sppPoolDeposit = fsppPoolDeposit
         , Ledger.sppEMax = fsppEMax
         , Ledger.sppNOpt = fsppNOpt
         , Ledger.sppA0 = fsppA0
         , Ledger.sppRho = fsppRho
         , Ledger.sppTau = fsppTau
         , Ledger.sppD = fsppD
         , Ledger.sppExtraEntropy = fsppExtraEntropy
         , Ledger.sppProtocolVersion = fsppProtocolVersion
         , Ledger.sppMinUTxOValue = fsppMinUTxOValue
         , Ledger.sppMinPoolCost = fsppMinPoolCost
         }

instance Semigroup (Ledger.AlonzoPParams StrictMaybe era) where
  (<>) p1 p2 =
    let fappMinFeeA = lastMappendWith Ledger.appMinFeeA p1 p2
        fappMinFeeB = lastMappendWith Ledger.appMinFeeB p1 p2
        fappMaxBBSize = lastMappendWith Ledger.appMaxBBSize p1 p2
        fappMaxTxSize = lastMappendWith Ledger.appMaxTxSize p1 p2
        fappMaxBHSize = lastMappendWith Ledger.appMaxBHSize p1 p2
        fappKeyDeposit = lastMappendWith Ledger.appKeyDeposit p1 p2
        fappPoolDeposit = lastMappendWith Ledger.appPoolDeposit p1 p2
        fappEMax = lastMappendWith Ledger.appEMax p1 p2
        fappNOpt = lastMappendWith Ledger.appNOpt p1 p2
        fappA0 = lastMappendWith Ledger.appA0 p1 p2
        fappRho = lastMappendWith Ledger.appRho p1 p2
        fappTau = lastMappendWith Ledger.appTau p1 p2
        fappD = lastMappendWith Ledger.appD p1 p2
        fappExtraEntropy = lastMappendWith Ledger.appExtraEntropy p1 p2
        fappProtocolVersion = lastMappendWith Ledger.appProtocolVersion p1 p2
        fappMinPoolCost = lastMappendWith Ledger.appMinPoolCost p1 p2
        fappCoinsPerUTxOWord = lastMappendWith Ledger.appCoinsPerUTxOWord p1 p2
        fappCostModels = lastMappendWith Ledger.appCostModels p1 p2
        fappPrices = lastMappendWith Ledger.appPrices p1 p2
        fappMaxTxExUnits = lastMappendWith Ledger.appMaxTxExUnits p1 p2
        fappMaxBlockExUnits = lastMappendWith Ledger.appMaxBlockExUnits p1 p2
        fappMaxValSize = lastMappendWith Ledger.appMaxValSize p1 p2
        fappCollateralPercentage = lastMappendWith Ledger.appCollateralPercentage p1 p2
        fappMaxCollateralInputs = lastMappendWith Ledger.appMaxCollateralInputs p1 p2
    in Ledger.AlonzoPParams
        { Ledger.appMinFeeA = fappMinFeeA
        , Ledger.appMinFeeB = fappMinFeeB
        , Ledger.appMaxBBSize = fappMaxBBSize
        , Ledger.appMaxTxSize = fappMaxTxSize
        , Ledger.appMaxBHSize = fappMaxBHSize
        , Ledger.appKeyDeposit = fappKeyDeposit
        , Ledger.appPoolDeposit = fappPoolDeposit
        , Ledger.appEMax = fappEMax
        , Ledger.appNOpt = fappNOpt
        , Ledger.appA0 = fappA0
        , Ledger.appRho = fappRho
        , Ledger.appTau = fappTau
        , Ledger.appD = fappD
        , Ledger.appExtraEntropy = fappExtraEntropy
        , Ledger.appProtocolVersion = fappProtocolVersion
        , Ledger.appMinPoolCost = fappMinPoolCost
        , Ledger.appCoinsPerUTxOWord = fappCoinsPerUTxOWord
        , Ledger.appCostModels = fappCostModels
        , Ledger.appPrices = fappPrices
        , Ledger.appMaxTxExUnits = fappMaxTxExUnits
        , Ledger.appMaxBlockExUnits = fappMaxBlockExUnits
        , Ledger.appMaxValSize = fappMaxValSize
        , Ledger.appCollateralPercentage = fappCollateralPercentage
        , Ledger.appMaxCollateralInputs = fappMaxCollateralInputs
        }

-- We're not interested in trying to mappend the underlying `Maybe` types
-- we only want to select one or the other therefore we use `Last`.
lastMappend :: StrictMaybe a -> StrictMaybe a -> StrictMaybe a
lastMappend a b = Ledger.maybeToStrictMaybe . getLast $ strictMaybeToLast a <> strictMaybeToLast b
 where
  strictMaybeToLast :: StrictMaybe a -> Last a
  strictMaybeToLast = Last . strictMaybeToMaybe

lastMappendWith :: (a -> StrictMaybe b) -> a -> a -> StrictMaybe b
lastMappendWith l = under2 l lastMappend
  where
    under2 :: (a -> c) -> (c -> c -> c) -> a -> a -> c
    under2 f g x y = g (f x) (f y)

instance Semigroup (Ledger.BabbagePParams StrictMaybe era) where
  (<>) p1 p2 =
    let fbppMinFeeA = lastMappendWith Ledger.bppMinFeeA p1 p2
        fbppMinFeeB = lastMappendWith Ledger.bppMinFeeB p1 p2
        fbppMaxBBSize = lastMappendWith Ledger.bppMaxBBSize p1 p2
        fbppMaxTxSize = lastMappendWith Ledger.bppMaxTxSize p1 p2
        fbppMaxBHSize = lastMappendWith Ledger.bppMaxBHSize p1 p2
        fbppKeyDeposit = lastMappendWith Ledger.bppKeyDeposit p1 p2
        fbppPoolDeposit = lastMappendWith Ledger.bppPoolDeposit p1 p2
        fbppEMax = lastMappendWith Ledger.bppEMax p1 p2
        fbppNOpt = lastMappendWith Ledger.bppNOpt p1 p2
        fbppA0 = lastMappendWith Ledger.bppA0 p1 p2
        fbppRho = lastMappendWith Ledger.bppRho p1 p2
        fbppTau = lastMappendWith Ledger.bppTau p1 p2
        fbppProtocolVersion = lastMappendWith Ledger.bppProtocolVersion p1 p2
        fbppMinPoolCost = lastMappendWith Ledger.bppMinPoolCost p1 p2
        fbppCoinsPerUTxOByte = lastMappendWith Ledger.bppCoinsPerUTxOByte p1 p2
        fbppCostModels = lastMappendWith Ledger.bppCostModels p1 p2
        fbppPrices = lastMappendWith Ledger.bppPrices p1 p2
        fbppMaxTxExUnits = lastMappendWith Ledger.bppMaxTxExUnits p1 p2
        fbppMaxBlockExUnits = lastMappendWith Ledger.bppMaxBlockExUnits p1 p2
        fbppMaxValSize = lastMappendWith Ledger.bppMaxValSize p1 p2
        fbppCollateralPercentage = lastMappendWith Ledger.bppCollateralPercentage p1 p2
        fbppMaxCollateralInputs = lastMappendWith Ledger.bppMaxCollateralInputs p1 p2
    in Ledger.BabbagePParams
         { Ledger.bppMinFeeA = fbppMinFeeA
         , Ledger.bppMinFeeB = fbppMinFeeB
         , Ledger.bppMaxBBSize = fbppMaxBBSize
         , Ledger.bppMaxTxSize = fbppMaxTxSize
         , Ledger.bppMaxBHSize = fbppMaxBHSize
         , Ledger.bppKeyDeposit = fbppKeyDeposit
         , Ledger.bppPoolDeposit = fbppPoolDeposit
         , Ledger.bppEMax = fbppEMax
         , Ledger.bppNOpt = fbppNOpt
         , Ledger.bppA0 = fbppA0
         , Ledger.bppRho = fbppRho
         , Ledger.bppTau = fbppTau
         , Ledger.bppProtocolVersion = fbppProtocolVersion
         , Ledger.bppMinPoolCost = fbppMinPoolCost
         , Ledger.bppCoinsPerUTxOByte = fbppCoinsPerUTxOByte
         , Ledger.bppCostModels = fbppCostModels
         , Ledger.bppPrices = fbppPrices
         , Ledger.bppMaxTxExUnits = fbppMaxTxExUnits
         , Ledger.bppMaxBlockExUnits = fbppMaxBlockExUnits
         , Ledger.bppMaxValSize = fbppMaxValSize
         , Ledger.bppCollateralPercentage = fbppCollateralPercentage
         , Ledger.bppMaxCollateralInputs = fbppMaxCollateralInputs
         }

instance Semigroup (Ledger.ConwayPParams StrictMaybe era) where
  (<>) p1 p2 = Ledger.ConwayPParams
    { Ledger.cppMinFeeA = lastMappendWithTHKD Ledger.cppMinFeeA p1 p2
    , Ledger.cppMinFeeB = lastMappendWithTHKD Ledger.cppMinFeeB p1 p2
    , Ledger.cppMaxBBSize = lastMappendWithTHKD Ledger.cppMaxBBSize p1 p2
    , Ledger.cppMaxTxSize = lastMappendWithTHKD Ledger.cppMaxTxSize p1 p2
    , Ledger.cppMaxBHSize = lastMappendWithTHKD Ledger.cppMaxBHSize p1 p2
    , Ledger.cppKeyDeposit = lastMappendWithTHKD Ledger.cppKeyDeposit p1 p2
    , Ledger.cppPoolDeposit = lastMappendWithTHKD Ledger.cppPoolDeposit p1 p2
    , Ledger.cppEMax = lastMappendWithTHKD Ledger.cppEMax p1 p2
    , Ledger.cppNOpt = lastMappendWithTHKD Ledger.cppNOpt p1 p2
    , Ledger.cppA0 = lastMappendWithTHKD Ledger.cppA0 p1 p2
    , Ledger.cppRho = lastMappendWithTHKD Ledger.cppRho p1 p2
    , Ledger.cppTau = lastMappendWithTHKD Ledger.cppTau p1 p2
    , Ledger.cppProtocolVersion = NoUpdate -- For conway, protocol version cannot be changed via `PParamsUpdate`
    , Ledger.cppMinPoolCost = lastMappendWithTHKD Ledger.cppMinPoolCost p1 p2
    , Ledger.cppCoinsPerUTxOByte = lastMappendWithTHKD Ledger.cppCoinsPerUTxOByte p1 p2
    , Ledger.cppCostModels = lastMappendWithTHKD Ledger.cppCostModels p1 p2
    , Ledger.cppPrices = lastMappendWithTHKD Ledger.cppPrices p1 p2
    , Ledger.cppMaxTxExUnits = lastMappendWithTHKD Ledger.cppMaxTxExUnits p1 p2
    , Ledger.cppMaxBlockExUnits = lastMappendWithTHKD Ledger.cppMaxBlockExUnits p1 p2
    , Ledger.cppMaxValSize = lastMappendWithTHKD Ledger.cppMaxValSize p1 p2
    , Ledger.cppCollateralPercentage = lastMappendWithTHKD Ledger.cppCollateralPercentage p1 p2
    , Ledger.cppMaxCollateralInputs = lastMappendWithTHKD Ledger.cppMaxCollateralInputs p1 p2
    , Ledger.cppPoolVotingThresholds = lastMappendWithTHKD Ledger.cppPoolVotingThresholds p1 p2
    , Ledger.cppDRepVotingThresholds = lastMappendWithTHKD Ledger.cppDRepVotingThresholds p1 p2
    , Ledger.cppCommitteeMinSize = lastMappendWithTHKD Ledger.cppCommitteeMinSize p1 p2
    , Ledger.cppCommitteeMaxTermLength = lastMappendWithTHKD Ledger.cppCommitteeMaxTermLength p1 p2
    , Ledger.cppGovActionLifetime = lastMappendWithTHKD Ledger.cppGovActionLifetime p1 p2
    , Ledger.cppGovActionDeposit = lastMappendWithTHKD Ledger.cppGovActionDeposit p1 p2
    , Ledger.cppDRepDeposit = lastMappendWithTHKD Ledger.cppDRepDeposit p1 p2
    , Ledger.cppDRepActivity = lastMappendWithTHKD Ledger.cppDRepActivity p1 p2
    }

lastMappendWithTHKD :: (a -> Ledger.THKD g StrictMaybe b) -> a -> a -> Ledger.THKD g StrictMaybe b
lastMappendWithTHKD f a b = Ledger.THKD $ lastMappendWith (Ledger.unTHKD . f) a b
