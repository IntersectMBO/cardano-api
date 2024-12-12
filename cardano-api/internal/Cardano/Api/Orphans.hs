{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Cardano.Api.Orphans () where

import           Cardano.Api.Pretty (Pretty (..), prettyException, (<+>))
import           Cardano.Api.Via.ShowOf

import           Cardano.Binary (DecoderError (..))
import qualified Cardano.Chain.Byron.API as L
import qualified Cardano.Chain.Common as L
import qualified Cardano.Chain.Delegation.Validation.Scheduling as L.Scheduling
import qualified Cardano.Chain.Update as L
import qualified Cardano.Chain.Update.Validation.Endorsement as L.Endorsement
import qualified Cardano.Chain.Update.Validation.Interface as L.Interface
import qualified Cardano.Chain.Update.Validation.Registration as L.Registration
import qualified Cardano.Chain.Update.Validation.Voting as L.Voting
import qualified Cardano.Chain.UTxO.UTxO as L
import qualified Cardano.Chain.UTxO.Validation as L
import qualified Cardano.Ledger.Allegra.Rules as L
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Rules as L
import qualified Cardano.Ledger.Alonzo.Tx as L
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage.PParams as Ledger
import qualified Cardano.Ledger.Babbage.Rules as L
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.PParams as Ledger
import qualified Cardano.Ledger.Conway.Rules as L
import qualified Cardano.Ledger.Conway.TxCert as L
import qualified Cardano.Ledger.Core as L
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Crypto as L
import           Cardano.Ledger.HKD (NoUpdate (..))
import qualified Cardano.Ledger.Keys as L.Keys
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.Shelley.API.Mempool as L
import qualified Cardano.Ledger.Shelley.PParams as Ledger
import qualified Cardano.Ledger.Shelley.Rules as L
import qualified Cardano.Ledger.Shelley.TxBody as L
import qualified Cardano.Ledger.Shelley.TxCert as L
import qualified Cardano.Protocol.TPraos.API as Ledger
import           Cardano.Protocol.TPraos.BHeader (HashHeader (..))
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as L
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as Ledger
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Ledger
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import           Ouroboros.Consensus.Protocol.Praos (PraosState)
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import           Ouroboros.Consensus.Protocol.TPraos (TPraosState)
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Consensus
import           Ouroboros.Network.Block (HeaderHash, Tip (..))
import qualified Network.Mux as Mux
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V2 as V2

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Read as CBOR
import           Data.Aeson (KeyValue ((.=)), ToJSON (..), ToJSONKey (..), object, pairs)
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import           Data.Data (Data)
import           Data.Kind (Constraint, Type)
import           Data.ListMap (ListMap)
import qualified Data.ListMap as ListMap
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable)
import           GHC.Exts (IsList (..))
import           GHC.Generics
import           GHC.Stack (HasCallStack)
import           GHC.TypeLits
import           Lens.Micro

deriving instance Generic (L.ApplyTxError era)

deriving instance Generic (L.Registration.TooLarge a)

deriving instance Generic L.ApplicationNameError

deriving instance Generic L.ApplyMempoolPayloadErr

deriving instance Generic L.Endorsement.Error

deriving instance Generic L.Interface.Error

deriving instance Generic L.LovelaceError

deriving instance Generic L.Registration.Adopted

deriving instance Generic L.Registration.Error

deriving instance Generic L.Scheduling.Error

deriving instance Generic L.SoftwareVersionError

deriving instance Generic L.SystemTagError

deriving instance Generic L.TxValidationError

deriving instance Generic L.UTxOError

deriving instance Generic L.UTxOValidationError

deriving instance Generic L.Voting.Error

deriving anyclass instance ToJSON L.ApplicationNameError

deriving anyclass instance ToJSON L.ApplyMempoolPayloadErr

deriving anyclass instance ToJSON L.Endorsement.Error

deriving anyclass instance ToJSON L.Interface.Error

deriving anyclass instance ToJSON L.LovelaceError

deriving anyclass instance ToJSON L.Registration.Adopted

deriving anyclass instance ToJSON L.Registration.ApplicationVersion

deriving anyclass instance ToJSON L.Registration.Error

deriving anyclass instance ToJSON L.Scheduling.Error

deriving anyclass instance ToJSON L.SoftwareVersionError

deriving anyclass instance ToJSON L.SystemTagError

deriving anyclass instance ToJSON L.TxValidationError

deriving anyclass instance ToJSON L.UTxOError

deriving anyclass instance ToJSON L.UTxOValidationError

deriving anyclass instance ToJSON L.Voting.Error

deriving anyclass instance ToJSON L.VotingPeriod

deriving anyclass instance
  ( ToJSON (L.PredicateFailure (L.EraRule "UTXOW" ledgerera))
  , ToJSON (L.PredicateFailure (L.EraRule "DELEGS" ledgerera))
  )
  => ToJSON (L.ShelleyLedgerPredFailure ledgerera)

deriving anyclass instance
  ( L.Crypto (L.EraCrypto ledgerera)
  , ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  )
  => ToJSON (L.ShelleyUtxowPredFailure ledgerera)

deriving anyclass instance
  ( L.Crypto (L.EraCrypto ledgerera)
  , ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  )
  => ToJSON (L.ShelleyPpupPredFailure ledgerera)

deriving anyclass instance
  ( L.Crypto (L.EraCrypto ledgerera)
  , ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  , ToJSON (L.PlutusPurpose L.AsItem ledgerera)
  , ToJSON (L.PlutusPurpose L.AsIx ledgerera)
  )
  => ToJSON (L.AlonzoUtxowPredFailure ledgerera)

deriving anyclass instance
  ( L.Crypto (L.EraCrypto ledgerera)
  , ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  , ToJSON (L.TxCert ledgerera)
  , ToJSON (L.PlutusPurpose L.AsItem ledgerera)
  , ToJSON (L.PlutusPurpose L.AsIx ledgerera)
  )
  => ToJSON (L.BabbageUtxowPredFailure ledgerera)

deriving anyclass instance
  ToJSON (L.PredicateFailure (L.EraRule "LEDGER" ledgerera))
  => ToJSON (L.ApplyTxError ledgerera)

deriving via
  ShowOf (L.Keys.VKey L.Keys.Witness c)
  instance
    L.Crypto c => ToJSON (L.Keys.VKey L.Keys.Witness c)

deriving via
  ShowOf (L.AllegraUtxoPredFailure ledgerera)
  instance
    Show (L.AllegraUtxoPredFailure ledgerera) => ToJSON (L.AllegraUtxoPredFailure ledgerera)

deriving via
  ShowOf (L.AlonzoUtxoPredFailure ledgerera)
  instance
    Show (L.AlonzoUtxoPredFailure ledgerera) => ToJSON (L.AlonzoUtxoPredFailure ledgerera)

deriving via
  ShowOf (L.BabbageUtxoPredFailure ledgerera)
  instance
    Show (L.BabbageUtxoPredFailure ledgerera) => ToJSON (L.BabbageUtxoPredFailure ledgerera)

deriving via
  ShowOf (L.ConwayLedgerPredFailure ledgerera)
  instance
    Show (L.ConwayLedgerPredFailure ledgerera) => ToJSON (L.ConwayLedgerPredFailure ledgerera)

deriving via
  ShowOf (L.ShelleyDelegsPredFailure ledgerera)
  instance
    Show (L.ShelleyDelegsPredFailure ledgerera) => ToJSON (L.ShelleyDelegsPredFailure ledgerera)

deriving via
  ShowOf (L.ShelleyUtxoPredFailure ledgerera)
  instance
    Show (L.ShelleyUtxoPredFailure ledgerera) => ToJSON (L.ShelleyUtxoPredFailure ledgerera)

deriving instance ToJSON a => ToJSON (L.Registration.TooLarge a)

deriving via ShowOf L.KeyHash instance ToJSON L.KeyHash

deriving via ShowOf L.ApplicationName instance ToJSONKey L.ApplicationName

deriving instance Data DecoderError

deriving instance Data CBOR.DeserialiseFailure

deriving instance Data Bech32.DecodingError

deriving instance Data Bech32.CharPosition

-- | These instances originally existed on the Lovelace type.
-- As the Lovelace type is deleted and we use L.Coin instead,
-- these instances are added to L.Coin.  The instances are
-- purely for the convenience of writing expressions involving
-- L.Coin but be aware that not all uses of these typeclasses
-- are valid.
deriving newtype instance Real L.Coin

deriving newtype instance Integral L.Coin

deriving newtype instance Num L.Coin

instance Pretty L.Coin where
  pretty (L.Coin n) = pretty n <+> "Lovelace"

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance Crypto.Crypto crypto => ToJSON (Consensus.StakeSnapshots crypto) where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair
  :: (Aeson.KeyValue e a, Crypto.Crypto crypto) => Consensus.StakeSnapshots crypto -> [a]
stakeSnapshotsToPair
  Consensus.StakeSnapshots
    { Consensus.ssStakeSnapshots
    , Consensus.ssMarkTotal
    , Consensus.ssSetTotal
    , Consensus.ssGoTotal
    } =
    [ "pools" .= ssStakeSnapshots
    , "total"
        .= object
          [ "stakeMark" .= ssMarkTotal
          , "stakeSet" .= ssSetTotal
          , "stakeGo" .= ssGoTotal
          ]
    ]

instance ToJSON (Consensus.StakeSnapshot crypto) where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue e a => Consensus.StakeSnapshot crypto -> [a]
stakeSnapshotToPair
  Consensus.StakeSnapshot
    { Consensus.ssMarkPool
    , Consensus.ssSetPool
    , Consensus.ssGoPool
    } =
    [ "stakeMark" .= ssMarkPool
    , "stakeSet" .= ssSetPool
    , "stakeGo" .= ssGoPool
    ]

instance ToJSON (OneEraHash xs) where
  toJSON =
    toJSON
      . Text.decodeLatin1
      . Base16.encode
      . SBS.fromShort
      . getOneEraHash

deriving newtype instance ToJSON ByronHash

-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = Aeson.object ["genesis" .= True]
  toJSON (Tip slotNo headerHash blockNo) =
    Aeson.object
      [ "slotNo" .= slotNo
      , "headerHash" .= headerHash
      , "blockNo" .= blockNo
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
  toJSON s =
    Aeson.object
      [ "lastSlot" .= Consensus.tpraosStateLastSlot s
      , "chainDepState" .= Consensus.tpraosStateChainDepState s
      ]

instance ToJSON (PraosState StandardCrypto) where
  toJSON s =
    Aeson.object
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
  (<>) p1 p2 =
    Ledger.ConwayPParams
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
      , Ledger.cppMinFeeRefScriptCostPerByte =
          lastMappendWithTHKD Ledger.cppMinFeeRefScriptCostPerByte p1 p2
      }

lastMappendWithTHKD :: (a -> Ledger.THKD g StrictMaybe b) -> a -> a -> Ledger.THKD g StrictMaybe b
lastMappendWithTHKD f a b = Ledger.THKD $ lastMappendWith (Ledger.unTHKD . f) a b

instance Pretty Mux.Error where
  pretty err = "Mux layer error:" <+> prettyException err

instance A.FromJSON V2.ParamName where
  parseJSON = A.withText "ParamName" parsePlutusParamName

instance A.FromJSONKey V2.ParamName where
  fromJSONKey = A.FromJSONKeyTextParser parsePlutusParamName

parsePlutusParamName :: (P.IsParamName a, MonadFail f) => T.Text -> f a
parsePlutusParamName t =
  case P.readParamName t of
    Just p -> pure p
    Nothing -> fail $ "Cannot parse cost model parameter name: " <> T.unpack t

deriving instance Show V2.ParamName

-- TODO upstream to cardano-ledger
instance IsList (ListMap k a) where
  type Item (ListMap k a) = (k, a)
  fromList = ListMap.fromList
  toList = ListMap.toList
