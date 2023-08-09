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
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import qualified Cardano.Ledger.Crypto as Crypto
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



-- We wrap the individual records with Last and use Last's Semigroup instance.
-- In this instance we take the last 'Just' value or the only 'Just' value
instance Semigroup (Ledger.ShelleyPParams StrictMaybe era) where
  (<>) pp1 pp2 =
    let fsppMinFeeA = lastMappend (Ledger.sppMinFeeA pp1) (Ledger.sppMinFeeA pp2)
        fsppMinFeeB = lastMappend (Ledger.sppMinFeeB pp1) (Ledger.sppMinFeeB pp2)
        fsppMaxBBSize = lastMappend (Ledger.sppMaxBBSize pp1) (Ledger.sppMaxBBSize pp2)
        fsppMaxTxSize = lastMappend (Ledger.sppMaxTxSize pp1) (Ledger.sppMaxTxSize pp2)
        fsppMaxBHSize = lastMappend (Ledger.sppMaxBHSize pp1) (Ledger.sppMaxBHSize pp2)
        fsppKeyDeposit = lastMappend (Ledger.sppKeyDeposit pp1) (Ledger.sppKeyDeposit pp2)
        fsppPoolDeposit = lastMappend (Ledger.sppPoolDeposit pp1) (Ledger.sppPoolDeposit pp2)
        fsppEMax = lastMappend (Ledger.sppEMax pp1) (Ledger.sppEMax pp2)
        fsppNOpt = lastMappend (Ledger.sppNOpt pp1) (Ledger.sppNOpt pp2)
        fsppA0 = lastMappend (Ledger.sppA0 pp1) (Ledger.sppA0 pp2)
        fsppRho = lastMappend (Ledger.sppRho pp1) (Ledger.sppRho pp2)
        fsppTau = lastMappend (Ledger.sppTau pp1) (Ledger.sppTau pp2)
        fsppD = lastMappend (Ledger.sppD pp1) (Ledger.sppD pp2)
        fsppExtraEntropy = lastMappend (Ledger.sppExtraEntropy pp1) (Ledger.sppExtraEntropy pp2)
        fsppProtocolVersion = lastMappend (Ledger.sppProtocolVersion pp1) (Ledger.sppProtocolVersion pp2)
        fsppMinUTxOValue = lastMappend (Ledger.sppMinUTxOValue pp1) (Ledger.sppMinUTxOValue pp2)
        fsppMinPoolCost = lastMappend (Ledger.sppMinPoolCost pp1) (Ledger.sppMinPoolCost pp2)
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
    let fappMinFeeA = lastMappend (Ledger.appMinFeeA p1) (Ledger.appMinFeeA p2)
        fappMinFeeB = lastMappend (Ledger.appMinFeeB p1) (Ledger.appMinFeeB p2)
        fappMaxBBSize = lastMappend (Ledger.appMaxBBSize p1) (Ledger.appMaxBBSize p2)
        fappMaxTxSize = lastMappend (Ledger.appMaxTxSize p1) (Ledger.appMaxTxSize p2)
        fappMaxBHSize = lastMappend (Ledger.appMaxBHSize p1) (Ledger.appMaxBHSize p2)
        fappKeyDeposit = lastMappend (Ledger.appKeyDeposit p1) (Ledger.appKeyDeposit p2)
        fappPoolDeposit = lastMappend (Ledger.appPoolDeposit p1) (Ledger.appPoolDeposit p2)
        fappEMax = lastMappend (Ledger.appEMax p1) (Ledger.appEMax p2)
        fappNOpt = lastMappend (Ledger.appNOpt p1) (Ledger.appNOpt p2)
        fappA0 = lastMappend (Ledger.appA0 p1) (Ledger.appA0 p2)
        fappRho = lastMappend (Ledger.appRho p1) (Ledger.appRho p2)
        fappTau = lastMappend (Ledger.appTau p1) (Ledger.appTau p2)
        fappD = lastMappend (Ledger.appD p1) (Ledger.appD p2)
        fappExtraEntropy = lastMappend (Ledger.appExtraEntropy p1) (Ledger.appExtraEntropy p2)
        fappProtocolVersion = lastMappend (Ledger.appProtocolVersion p1) (Ledger.appProtocolVersion p2)
        fappMinPoolCost = lastMappend (Ledger.appMinPoolCost p1) (Ledger.appMinPoolCost p2)
        fappCoinsPerUTxOWord = lastMappend (Ledger.appCoinsPerUTxOWord p1) (Ledger.appCoinsPerUTxOWord p2)
        fappCostModels = lastMappend (Ledger.appCostModels p1) (Ledger.appCostModels p2)
        fappPrices = lastMappend (Ledger.appPrices p1) (Ledger.appPrices p2)
        fappMaxTxExUnits = lastMappend (Ledger.appMaxTxExUnits p1) (Ledger.appMaxTxExUnits p2)
        fappMaxBlockExUnits = lastMappend (Ledger.appMaxBlockExUnits p1) (Ledger.appMaxBlockExUnits p2)
        fappMaxValSize = lastMappend (Ledger.appMaxValSize p1) (Ledger.appMaxValSize p2)
        fappCollateralPercentage = lastMappend (Ledger.appCollateralPercentage p1) (Ledger.appCollateralPercentage p2)
        fappMaxCollateralInputs = lastMappend (Ledger.appMaxCollateralInputs p1) (Ledger.appMaxCollateralInputs p2)
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

instance Semigroup (Ledger.BabbagePParams StrictMaybe era) where
  (<>) p1 p2 =
    let fbppMinFeeA = lastMappend (Ledger.bppMinFeeA p1) (Ledger.bppMinFeeA p2)
        fbppMinFeeB = lastMappend (Ledger.bppMinFeeB p1) (Ledger.bppMinFeeB p2)
        fbppMaxBBSize = lastMappend (Ledger.bppMaxBBSize p1) (Ledger.bppMaxBBSize p2)
        fbppMaxTxSize = lastMappend (Ledger.bppMaxTxSize p1) (Ledger.bppMaxTxSize p2)
        fbppMaxBHSize = lastMappend (Ledger.bppMaxBHSize p1) (Ledger.bppMaxBHSize p2)
        fbppKeyDeposit = lastMappend (Ledger.bppKeyDeposit p1) (Ledger.bppKeyDeposit p2)
        fbppPoolDeposit = lastMappend (Ledger.bppPoolDeposit p1) (Ledger.bppPoolDeposit p2)
        fbppEMax = lastMappend (Ledger.bppEMax p1) (Ledger.bppEMax p2)
        fbppNOpt = lastMappend (Ledger.bppNOpt p1) (Ledger.bppNOpt p2)
        fbppA0 = lastMappend (Ledger.bppA0 p1) (Ledger.bppA0 p2)
        fbppRho = lastMappend (Ledger.bppRho p1) (Ledger.bppRho p2)
        fbppTau = lastMappend (Ledger.bppTau p1) (Ledger.bppTau p2)
        fbppProtocolVersion = lastMappend (Ledger.bppProtocolVersion p1) (Ledger.bppProtocolVersion p2)
        fbppMinPoolCost = lastMappend (Ledger.bppMinPoolCost p1) (Ledger.bppMinPoolCost p2)
        fbppCoinsPerUTxOByte = lastMappend (Ledger.bppCoinsPerUTxOByte p1) (Ledger.bppCoinsPerUTxOByte p2)
        fbppCostModels = lastMappend (Ledger.bppCostModels p1) (Ledger.bppCostModels p2)
        fbppPrices = lastMappend (Ledger.bppPrices p1) (Ledger.bppPrices p2)
        fbppMaxTxExUnits = lastMappend (Ledger.bppMaxTxExUnits p1) (Ledger.bppMaxTxExUnits p2)
        fbppMaxBlockExUnits = lastMappend (Ledger.bppMaxBlockExUnits p1) (Ledger.bppMaxBlockExUnits p2)
        fbppMaxValSize = lastMappend (Ledger.bppMaxValSize p1) (Ledger.bppMaxValSize p2)
        fbppCollateralPercentage = lastMappend (Ledger.bppCollateralPercentage p1) (Ledger.bppCollateralPercentage p2)
        fbppMaxCollateralInputs = lastMappend (Ledger.bppMaxCollateralInputs p1) (Ledger.bppMaxCollateralInputs p2)
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
