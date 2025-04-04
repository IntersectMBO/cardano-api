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

module Cardano.Api.Internal.Orphans () where

import Cardano.Api.Internal.Pretty (Pretty (..), prettyException, (<+>))
import Cardano.Api.Internal.Via.ShowOf

import Cardano.Binary (DecoderError (..))
import Cardano.Chain.Byron.API qualified as L
import Cardano.Chain.Common qualified as L
import Cardano.Chain.Delegation.Validation.Scheduling qualified as L.Scheduling
import Cardano.Chain.UTxO.UTxO qualified as L
import Cardano.Chain.UTxO.Validation qualified as L
import Cardano.Chain.Update qualified as L
import Cardano.Chain.Update.Validation.Endorsement qualified as L.Endorsement
import Cardano.Chain.Update.Validation.Interface qualified as L.Interface
import Cardano.Chain.Update.Validation.Registration qualified as L.Registration
import Cardano.Chain.Update.Validation.Voting qualified as L.Voting
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Allegra.Rules qualified as L
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Alonzo.Rules qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.PParams qualified as Ledger
import Cardano.Ledger.Babbage.Rules qualified as L
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Conway.Rules qualified as L
import Cardano.Ledger.Conway.TxCert qualified as L
import Cardano.Ledger.Core qualified as L hiding (KeyHash)
import Cardano.Ledger.HKD (NoUpdate (..))
import Cardano.Ledger.Hashes qualified as L hiding (KeyHash)
import Cardano.Ledger.Keys qualified as L.Keys
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Shelley.API.Mempool qualified as L
import Cardano.Ledger.Shelley.PParams qualified as Ledger
import Cardano.Ledger.Shelley.Rules qualified as L
import Cardano.Ledger.Shelley.TxBody qualified as L
import Cardano.Ledger.Shelley.TxCert qualified as L
import Cardano.Protocol.Crypto qualified as P
import Cardano.Protocol.TPraos.API qualified as Ledger
import Cardano.Protocol.TPraos.BHeader (HashHeader (..))
import Cardano.Protocol.TPraos.Rules.Prtcl qualified as L
import Cardano.Protocol.TPraos.Rules.Prtcl qualified as Ledger
import Cardano.Protocol.TPraos.Rules.Tickn qualified as Ledger
import Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Consensus.Protocol.Praos (PraosState)
import Ouroboros.Consensus.Protocol.Praos qualified as Consensus
import Ouroboros.Consensus.Protocol.TPraos (TPraosState)
import Ouroboros.Consensus.Protocol.TPraos qualified as Consensus
import Ouroboros.Consensus.Shelley.Eras qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import Ouroboros.Consensus.Shelley.Ledger.Query qualified as Consensus
import Ouroboros.Network.Block (HeaderHash, Tip (..))
import PlutusLedgerApi.Common qualified as P
import PlutusLedgerApi.V2 qualified as V2

import Codec.Binary.Bech32 qualified as Bech32
import Codec.CBOR.Read qualified as CBOR
import Data.Aeson (KeyValue ((.=)), ToJSON (..), ToJSONKey (..), object, pairs)
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.ListMap (ListMap)
import Data.ListMap qualified as ListMap
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Typeable (Typeable)
import GHC.Exts (IsList (..), IsString (..))
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Lens.Micro
import Network.Mux qualified as Mux
import Prettyprinter (punctuate, viaShow)

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
  ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  => ToJSON (L.ShelleyUtxowPredFailure ledgerera)

deriving anyclass instance
  ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  => ToJSON (L.ShelleyPpupPredFailure ledgerera)

deriving anyclass instance
  ( ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  , ToJSON (L.PlutusPurpose L.AsItem ledgerera)
  , ToJSON (L.PlutusPurpose L.AsIx ledgerera)
  )
  => ToJSON (L.AlonzoUtxowPredFailure ledgerera)

deriving anyclass instance
  ( ToJSON (L.PredicateFailure (L.EraRule "UTXO" ledgerera))
  , ToJSON (L.TxCert ledgerera)
  , ToJSON (L.PlutusPurpose L.AsItem ledgerera)
  , ToJSON (L.PlutusPurpose L.AsIx ledgerera)
  )
  => ToJSON (L.BabbageUtxowPredFailure ledgerera)

deriving anyclass instance
  ToJSON (L.PredicateFailure (L.EraRule "LEDGER" ledgerera))
  => ToJSON (L.ApplyTxError ledgerera)

deriving via
  ShowOf (L.Keys.VKey L.Keys.Witness)
  instance
    ToJSON (L.Keys.VKey L.Keys.Witness)

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

instance Pretty L.MultiAsset where
  pretty (L.MultiAsset assetsMap) =
    mconcat $
      punctuate
        ", "
        [ pretty quantity <+> pretty pId <> "." <> pretty name
        | (pId, assets) <- toList assetsMap
        , (name, quantity) <- toList assets
        ]

instance Pretty L.PolicyID where
  pretty (L.PolicyID (L.ScriptHash sh)) = pretty $ Crypto.hashToStringAsHex sh

instance Pretty L.AssetName where
  pretty = pretty . L.assetNameToTextAsHex

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance ToJSON Consensus.StakeSnapshots where
  toJSON = object . stakeSnapshotsToPair
  toEncoding = pairs . mconcat . stakeSnapshotsToPair

stakeSnapshotsToPair
  :: Aeson.KeyValue e a => Consensus.StakeSnapshots -> [a]
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

instance ToJSON Consensus.StakeSnapshot where
  toJSON = object . stakeSnapshotToPair
  toEncoding = pairs . mconcat . stakeSnapshotToPair

stakeSnapshotToPair :: Aeson.KeyValue e a => Consensus.StakeSnapshot -> [a]
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

deriving newtype instance ToJSON ShelleyHash

deriving newtype instance ToJSON HashHeader

deriving instance ToJSON Ledger.PrtclState

deriving instance ToJSON Ledger.TicknState

deriving instance ToJSON Ledger.ChainDepState

instance ToJSON TPraosState where
  toJSON s =
    Aeson.object
      [ "lastSlot" .= Consensus.tpraosStateLastSlot s
      , "chainDepState" .= Consensus.tpraosStateChainDepState s
      ]

instance ToJSON PraosState where
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
