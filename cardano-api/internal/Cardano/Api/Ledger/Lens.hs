{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Eta reduce" -}

module Cardano.Api.Ledger.Lens
  ( -- *Types
    TxBody(..)

    -- * Constructors
  , mkAdaOnlyTxOut
  , mkAdaValue

    -- * Lenses
  , strictMaybeL
  , L.invalidBeforeL
  , L.invalidHereAfterL
  , invalidBeforeStrictL
  , invalidHereAfterStrictL
  , invalidBeforeTxBodyL
  , invalidHereAfterTxBodyL
  , ttlAsInvalidHereAfterTxBodyL
  , updateTxBodyL

  , txBodyL
  , bodyTxL
  , auxDataTxL
  , witsTxL
  , mintTxBodyL
  , scriptIntegrityHashTxBodyL
  , collateralInputsTxBodyL
  , reqSignerHashesTxBodyL
  , referenceInputsTxBodyL
  , collateralReturnTxBodyL
  , totalCollateralTxBodyL
  , certsTxBodyL
  , votingProceduresTxBodyL
  , proposalProceduresTxBodyL
  , adaAssetL
  , multiAssetL
  , valueTxOutL
  , valueTxOutAdaAssetL
  , coinTxOutL
  , scriptTxWitsL
  , setMinCoinTxOut
  , datsTxWitsL
  , rdmrsTxWitsL
  , isValidTxL
  , bootAddrTxWitsL
  , addrTxWitsL
  , dataHashTxOutL
  , binaryDataToData
  , datumTxOutL
  , referenceScriptTxOutL
  ) where

import           Cardano.Api.Eon.AllegraEraOnwards
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyEraOnly
import           Cardano.Api.Eon.ShelleyToAllegraEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eras.Case

import qualified Cardano.Ledger.Allegra.Core as L
import qualified Cardano.Ledger.Alonzo.Core as L
import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.BaseTypes (SlotNo, StrictMaybe (..))
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Keys as L
import qualified Cardano.Ledger.Mary.Value as L
import qualified Cardano.Ledger.Shelley.PParams as L
import qualified Cardano.Ledger.TxIn as L

import           Data.Map (Map)
import qualified Data.OSet.Strict as L
import qualified Data.Sequence.Strict as L
import           Data.Set (Set)
import           Lens.Micro

newtype TxBody era = TxBody
  { unTxBody :: L.TxBody (ShelleyLedgerEra era)
  }

strictMaybeL :: Lens' (StrictMaybe a) (Maybe a)
strictMaybeL = lens g s
  where
    g :: StrictMaybe a -> Maybe a
    g SNothing  = Nothing
    g (SJust x) = Just x

    s :: StrictMaybe a -> Maybe a -> StrictMaybe a
    s _ = maybe SNothing SJust

txBodyL :: Lens' (TxBody era) (L.TxBody (ShelleyLedgerEra era))
txBodyL = lens unTxBody (\_ x -> TxBody x)

invalidBeforeTxBodyL :: AllegraEraOnwards era -> Lens' (TxBody era) (Maybe SlotNo)
invalidBeforeTxBodyL w = allegraEraOnwardsConstraints w $ txBodyL . L.vldtTxBodyL . L.invalidBeforeL

-- | Compatibility lens that provides a consistent interface over 'ttlTxBodyL' and
-- 'vldtTxBodyL . invalidHereAfterStrictL' across all shelley based eras.
--
-- The ledger uses 'ttlTxBodyL' in 'Shelley' only and from Allegra onwards uses 'vldtTxBodyL' instead.
--
-- The former is a 'SlotNo' with no limit represented as 'maxBound'.
--
-- The latter is a 'ValidityInterval' which is a pair of 'SlotNo's that represent the lower and upper
-- bounds.
--
-- The upper bound field is similar t 'ttlTxBodyL' except it is a 'StrictMaybe SlotNo' type where
-- no bounds is represented by 'SNothing'.
--
-- 'invalidHereAfterTxBodyL' lens over both with a 'Maybe SlotNo' type representation.  Withing the
-- Shelley era, setting Nothing will set the ttl to 'maxBound' in the underlying ledger type.
invalidHereAfterTxBodyL :: ShelleyBasedEra era -> Lens' (TxBody era) (Maybe SlotNo)
invalidHereAfterTxBodyL =
  caseShelleyEraOnlyOrAllegraEraOnwards
    ttlAsInvalidHereAfterTxBodyL
    (const $ txBodyL . L.vldtTxBodyL . L.invalidHereAfterL)

-- | Compatibility lens over 'ttlTxBodyL' which represents 'maxBound' as Nothing and all other values as 'Just'.
ttlAsInvalidHereAfterTxBodyL :: ShelleyEraOnly era -> Lens' (TxBody era) (Maybe SlotNo)
ttlAsInvalidHereAfterTxBodyL w = lens (g w) (s w)
  where
    g :: ShelleyEraOnly era -> TxBody era -> Maybe SlotNo
    g w' txBody =
      shelleyEraOnlyConstraints w' $
        let ttl = txBody ^. txBodyL . L.ttlTxBodyL in if ttl == maxBound then Nothing else Just ttl

    s :: ShelleyEraOnly era -> TxBody era -> Maybe SlotNo -> TxBody era
    s w' txBody mSlotNo =
      shelleyEraOnlyConstraints w' $
        case mSlotNo of
          Nothing -> txBody & txBodyL . L.ttlTxBodyL .~ maxBound
          Just ttl -> txBody & txBodyL . L.ttlTxBodyL .~ ttl

-- | Lens to access the 'invalidBefore' field of a 'ValidityInterval' as a 'StrictMaybe SlotNo'.
-- Ideally this should be defined in cardano-ledger
invalidBeforeStrictL :: Lens' L.ValidityInterval (StrictMaybe SlotNo)
invalidBeforeStrictL = lens g s
  where
    g :: L.ValidityInterval -> StrictMaybe SlotNo
    g (L.ValidityInterval a _) = a

    s :: L.ValidityInterval -> StrictMaybe SlotNo -> L.ValidityInterval
    s (L.ValidityInterval _ b) a = L.ValidityInterval a b

-- | Lens to access the 'invalidHereAfter' field of a 'ValidityInterval' as a 'StrictMaybe SlotNo'.
-- Ideally this should be defined in cardano-ledger
invalidHereAfterStrictL :: Lens' L.ValidityInterval (StrictMaybe SlotNo)
invalidHereAfterStrictL = lens g s
  where
    g :: L.ValidityInterval -> StrictMaybe SlotNo
    g (L.ValidityInterval _ b) = b

    s :: L.ValidityInterval -> StrictMaybe SlotNo -> L.ValidityInterval
    s (L.ValidityInterval a _) b = L.ValidityInterval a b

updateTxBodyL :: ShelleyToBabbageEra era -> Lens' (TxBody era) (StrictMaybe (L.Update (ShelleyLedgerEra era)))
updateTxBodyL w = shelleyToBabbageEraConstraints w $ txBodyL . L.updateTxBodyL

mintTxBodyL :: MaryEraOnwards era -> Lens' (TxBody era) (L.MultiAsset L.StandardCrypto)
mintTxBodyL w = maryEraOnwardsConstraints w $ txBodyL . L.mintTxBodyL

scriptIntegrityHashTxBodyL :: AlonzoEraOnwards era -> Lens' (TxBody era) (StrictMaybe (L.ScriptIntegrityHash L.StandardCrypto))
scriptIntegrityHashTxBodyL w = alonzoEraOnwardsConstraints w $ txBodyL . L.scriptIntegrityHashTxBodyL

collateralInputsTxBodyL :: AlonzoEraOnwards era -> Lens' (TxBody era) (Set (L.TxIn L.StandardCrypto))
collateralInputsTxBodyL w = alonzoEraOnwardsConstraints w $ txBodyL . L.collateralInputsTxBodyL

reqSignerHashesTxBodyL :: AlonzoEraOnwards era -> Lens' (TxBody era) (Set (L.KeyHash L.Witness L.StandardCrypto))
reqSignerHashesTxBodyL w = alonzoEraOnwardsConstraints w $ txBodyL . L.reqSignerHashesTxBodyL

referenceInputsTxBodyL :: BabbageEraOnwards era -> Lens' (TxBody era) (Set (L.TxIn L.StandardCrypto))
referenceInputsTxBodyL w = babbageEraOnwardsConstraints w $ txBodyL . L.referenceInputsTxBodyL

collateralReturnTxBodyL :: BabbageEraOnwards era -> Lens' (TxBody era) (StrictMaybe (L.TxOut (ShelleyLedgerEra era)))
collateralReturnTxBodyL w = babbageEraOnwardsConstraints w $ txBodyL . L.collateralReturnTxBodyL

totalCollateralTxBodyL :: BabbageEraOnwards era -> Lens' (TxBody era) (StrictMaybe L.Coin)
totalCollateralTxBodyL w = babbageEraOnwardsConstraints w $ txBodyL . L.totalCollateralTxBodyL

certsTxBodyL :: ShelleyBasedEra era -> Lens' (TxBody era) (L.StrictSeq (L.TxCert (ShelleyLedgerEra era)))
certsTxBodyL w = shelleyBasedEraConstraints w $ txBodyL . L.certsTxBodyL

votingProceduresTxBodyL :: ConwayEraOnwards era -> Lens' (TxBody era) (L.VotingProcedures (ShelleyLedgerEra era))
votingProceduresTxBodyL w = conwayEraOnwardsConstraints w $ txBodyL . L.votingProceduresTxBodyL

proposalProceduresTxBodyL :: ConwayEraOnwards era -> Lens' (TxBody era) (L.OSet (L.ProposalProcedure (ShelleyLedgerEra era)))
proposalProceduresTxBodyL w = conwayEraOnwardsConstraints w $ txBodyL . L.proposalProceduresTxBodyL

mkAdaOnlyTxOut :: ShelleyBasedEra era -> L.Addr (L.EraCrypto (ShelleyLedgerEra era)) -> L.Coin -> L.TxOut (ShelleyLedgerEra era)
mkAdaOnlyTxOut sbe addr coin =
  mkBasicTxOut sbe addr (mkAdaValue sbe coin)

mkBasicTxOut :: ShelleyBasedEra era -> L.Addr (L.EraCrypto (ShelleyLedgerEra era)) -> L.Value (ShelleyLedgerEra era) -> L.TxOut (ShelleyLedgerEra era)
mkBasicTxOut sbe addr value =
  shelleyBasedEraConstraints sbe $ L.mkBasicTxOut addr value

mkAdaValue :: ShelleyBasedEra era -> L.Coin -> L.Value (ShelleyLedgerEra era)
mkAdaValue sbe coin =
  caseShelleyToAllegraOrMaryEraOnwards
    (const coin)
    (const (L.MaryValue (L.unCoin coin) mempty))
    sbe

adaAssetL :: ShelleyBasedEra era -> Lens' (L.Value (ShelleyLedgerEra era)) L.Coin
adaAssetL sbe =
  caseShelleyToAllegraOrMaryEraOnwards
    adaAssetShelleyToAllegraEraL
    adaAssetMaryEraOnwardsL
    sbe

adaAssetShelleyToAllegraEraL :: ShelleyToAllegraEra era -> Lens' (L.Value (ShelleyLedgerEra era)) L.Coin
adaAssetShelleyToAllegraEraL w =
  shelleyToAllegraEraConstraints w $ lens id const

adaAssetMaryEraOnwardsL :: MaryEraOnwards era -> Lens' (L.MaryValue L.StandardCrypto) L.Coin
adaAssetMaryEraOnwardsL w =
  maryEraOnwardsConstraints w $ lens
    (\(L.MaryValue c _) -> L.Coin c)
    (\(L.MaryValue _ ma) (L.Coin c) -> L.MaryValue c ma)

multiAssetL :: MaryEraOnwards era -> Lens' (L.MaryValue L.StandardCrypto) (L.MultiAsset L.StandardCrypto)
multiAssetL w =
  maryEraOnwardsConstraints w $ lens
    (\(L.MaryValue _ ma) -> ma)
    (\(L.MaryValue c _) ma -> L.MaryValue c ma)

valueTxOutL :: ShelleyBasedEra era -> Lens' (L.TxOut (ShelleyLedgerEra era)) (L.Value (ShelleyLedgerEra era))
valueTxOutL sbe = shelleyBasedEraConstraints sbe L.valueTxOutL

valueTxOutAdaAssetL :: ShelleyBasedEra era -> Lens' (L.TxOut (ShelleyLedgerEra era)) L.Coin
valueTxOutAdaAssetL sbe = valueTxOutL sbe . adaAssetL sbe

bodyTxL :: ShelleyBasedEra era -> Lens' (L.Tx (ShelleyLedgerEra era)) (L.TxBody (ShelleyLedgerEra era))
bodyTxL sbe = shelleyBasedEraConstraints sbe L.bodyTxL

auxDataTxL :: ShelleyBasedEra era -> Lens' (L.Tx (ShelleyLedgerEra era)) (StrictMaybe (L.TxAuxData (ShelleyLedgerEra era)))
auxDataTxL sbe = shelleyBasedEraConstraints sbe L.auxDataTxL

witsTxL :: ShelleyBasedEra era -> Lens' (L.Tx (ShelleyLedgerEra era)) (L.TxWits (ShelleyLedgerEra era))
witsTxL sbe = shelleyBasedEraConstraints sbe L.witsTxL

coinTxOutL :: ShelleyBasedEra era -> Lens' (L.TxOut (ShelleyLedgerEra era)) L.Coin
coinTxOutL sbe = shelleyBasedEraConstraints sbe L.coinTxOutL

setMinCoinTxOut :: ShelleyBasedEra era -> L.PParams (ShelleyLedgerEra era) -> L.TxOut (ShelleyLedgerEra era) -> L.TxOut (ShelleyLedgerEra era)
setMinCoinTxOut sbe = shelleyBasedEraConstraints sbe L.setMinCoinTxOut

scriptTxWitsL :: ShelleyBasedEra era -> Lens' (L.TxWits (ShelleyLedgerEra era)) (Map (L.ScriptHash L.StandardCrypto) (L.Script (ShelleyLedgerEra era)))
scriptTxWitsL sbe = shelleyBasedEraConstraints sbe L.scriptTxWitsL

datsTxWitsL :: AlonzoEraOnwards era -> Lens' (L.TxWits (ShelleyLedgerEra era)) (L.TxDats (ShelleyLedgerEra era))
datsTxWitsL eon = alonzoEraOnwardsConstraints eon L.datsTxWitsL

rdmrsTxWitsL :: AlonzoEraOnwards era -> Lens' (L.TxWits (ShelleyLedgerEra era)) (L.Redeemers (ShelleyLedgerEra era))
rdmrsTxWitsL eon = alonzoEraOnwardsConstraints eon L.rdmrsTxWitsL

isValidTxL :: AlonzoEraOnwards era -> Lens' (L.Tx (ShelleyLedgerEra era)) L.IsValid
isValidTxL eon = alonzoEraOnwardsConstraints eon L.isValidTxL

bootAddrTxWitsL :: ShelleyBasedEra era -> Lens' (L.TxWits (ShelleyLedgerEra era)) (Set (L.BootstrapWitness L.StandardCrypto))
bootAddrTxWitsL eon = shelleyBasedEraConstraints eon L.bootAddrTxWitsL

addrTxWitsL :: ShelleyBasedEra era -> Lens' (L.TxWits (ShelleyLedgerEra era)) (Set (L.WitVKey 'L.Witness L.StandardCrypto))
addrTxWitsL eon = shelleyBasedEraConstraints eon L.addrTxWitsL

dataHashTxOutL :: AlonzoEraOnwards era -> Lens' (L.TxOut (ShelleyLedgerEra era)) (StrictMaybe (L.DataHash L.StandardCrypto))
dataHashTxOutL eon = alonzoEraOnwardsConstraints eon L.dataHashTxOutL

binaryDataToData :: ShelleyBasedEra era -> L.BinaryData (ShelleyLedgerEra era) -> L.Data (ShelleyLedgerEra era)
binaryDataToData eon = shelleyBasedEraConstraints eon L.binaryDataToData

datumTxOutL :: BabbageEraOnwards era -> Lens' (L.TxOut (ShelleyLedgerEra era)) (L.Datum (ShelleyLedgerEra era))
datumTxOutL eon = babbageEraOnwardsConstraints eon L.datumTxOutL

referenceScriptTxOutL :: BabbageEraOnwards era -> Lens' (L.TxOut (ShelleyLedgerEra era)) (StrictMaybe (L.Script (ShelleyLedgerEra era)))
referenceScriptTxOutL eon = babbageEraOnwardsConstraints eon L.referenceScriptTxOutL
