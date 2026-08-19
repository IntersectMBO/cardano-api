{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Eta reduce" -}

-- TODO: Deprecate the remaining eon lenses (the validity-interval compatibility
-- lenses, adaAssetL and multiAssetL) once the ledger provides equivalents.

module Cardano.Api.Tx.Internal.Body.Lens
  ( -- * Types
    LedgerTxBody (..)

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
  , currentTreasuryValueTxBodyL
  , treasuryDonationTxBodyL
  , adaAssetL
  , multiAssetL
  , valueTxOutL
  , valueTxOutAdaAssetL
  )
where

import Cardano.Api.Era.Internal.Case
import Cardano.Api.Era.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert (Convert (convert))
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.MaryEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Era.Internal.Eon.ShelleyEraOnly
import Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra
import Cardano.Api.Experimental.Era (obtainCommonConstraints)
import Cardano.Api.Internal.Orphans ()

import Cardano.Ledger.Allegra.Core qualified as L
import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (SlotNo, StrictMaybe (..))
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Shelley.PParams qualified as L
import Cardano.Ledger.TxIn qualified as L
import Cardano.Ledger.Val qualified as L

import Data.OSet.Strict qualified as L
import Data.Sequence.Strict qualified as L
import Data.Set (Set)
import Lens.Micro

newtype LedgerTxBody era = LedgerTxBody
  { unTxBody :: L.TxBody L.TopTx (ShelleyLedgerEra era)
  }

strictMaybeL :: Lens' (StrictMaybe a) (Maybe a)
strictMaybeL = lens g s
 where
  g :: StrictMaybe a -> Maybe a
  g SNothing = Nothing
  g (SJust x) = Just x

  s :: StrictMaybe a -> Maybe a -> StrictMaybe a
  s _ = maybe SNothing SJust

txBodyL :: Lens' (LedgerTxBody era) (L.TxBody L.TopTx (ShelleyLedgerEra era))
txBodyL = lens unTxBody (\_ x -> LedgerTxBody x)

invalidBeforeTxBodyL :: AllegraEraOnwards era -> Lens' (LedgerTxBody era) (Maybe SlotNo)
invalidBeforeTxBodyL w = allegraEraOnwardsConstraints w $ txBodyL . L.vldtTxBodyL . L.invalidBeforeL . strictMaybeL

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
invalidHereAfterTxBodyL :: ShelleyBasedEra era -> Lens' (LedgerTxBody era) (Maybe SlotNo)
invalidHereAfterTxBodyL =
  caseShelleyEraOnlyOrAllegraEraOnwards
    ttlAsInvalidHereAfterTxBodyL
    (const $ txBodyL . L.vldtTxBodyL . L.invalidHereAfterL . strictMaybeL)

-- | Compatibility lens over 'ttlTxBodyL' which represents 'maxBound' as Nothing and all other values as 'Just'.
ttlAsInvalidHereAfterTxBodyL :: ShelleyEraOnly era -> Lens' (LedgerTxBody era) (Maybe SlotNo)
ttlAsInvalidHereAfterTxBodyL w = lens (g w) (s w)
 where
  g :: ShelleyEraOnly era -> LedgerTxBody era -> Maybe SlotNo
  g w' txBody =
    shelleyEraOnlyConstraints w' $
      let ttl = txBody ^. txBodyL . L.ttlTxBodyL in if ttl == maxBound then Nothing else Just ttl

  s :: ShelleyEraOnly era -> LedgerTxBody era -> Maybe SlotNo -> LedgerTxBody era
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

{-# DEPRECATED
  updateTxBodyL
  "Use updateTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
updateTxBodyL
  :: ShelleyToBabbageEra era -> Lens' (LedgerTxBody era) (StrictMaybe (L.Update (ShelleyLedgerEra era)))
updateTxBodyL w = shelleyToBabbageEraConstraints w $ txBodyL . L.updateTxBodyL

{-# DEPRECATED
  mintTxBodyL
  "Use mintTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
mintTxBodyL :: MaryEraOnwards era -> Lens' (LedgerTxBody era) L.MultiAsset
mintTxBodyL w = maryEraOnwardsConstraints w $ txBodyL . L.mintTxBodyL

{-# DEPRECATED
  scriptIntegrityHashTxBodyL
  "Use scriptIntegrityHashTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
scriptIntegrityHashTxBodyL
  :: AlonzoEraOnwards era -> Lens' (LedgerTxBody era) (StrictMaybe L.ScriptIntegrityHash)
scriptIntegrityHashTxBodyL w = alonzoEraOnwardsConstraints w $ txBodyL . L.scriptIntegrityHashTxBodyL

{-# DEPRECATED
  collateralInputsTxBodyL
  "Use collateralInputsTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
collateralInputsTxBodyL
  :: AlonzoEraOnwards era -> Lens' (LedgerTxBody era) (Set L.TxIn)
collateralInputsTxBodyL w = alonzoEraOnwardsConstraints w $ txBodyL . L.collateralInputsTxBodyL

{-# DEPRECATED
  reqSignerHashesTxBodyL
  "Use reqSignerHashesTxBodyL from Cardano.Api.Ledger (via txBodyL) instead, or reqSignerHashesTxBodyG for reads. The required-signer-hashes field does not exist in the Dijkstra era, where this lens errors."
  #-}
reqSignerHashesTxBodyL
  :: AlonzoEraOnwards era -> Lens' (LedgerTxBody era) (Set (L.KeyHash L.Guard))
reqSignerHashesTxBodyL w@AlonzoEraOnwardsAlonzo = alonzoEraOnwardsConstraints w $ txBodyL . L.reqSignerHashesTxBodyL
reqSignerHashesTxBodyL w@AlonzoEraOnwardsBabbage = alonzoEraOnwardsConstraints w $ txBodyL . L.reqSignerHashesTxBodyL
reqSignerHashesTxBodyL w@AlonzoEraOnwardsConway = alonzoEraOnwardsConstraints w $ txBodyL . L.reqSignerHashesTxBodyL
-- Dijkstra replaced required signer hashes with guards; the ledger gates its
-- lens to @AtMostEra "Conway"@ and stubs the instance with 'L.notSupportedInThisEraL'.
reqSignerHashesTxBodyL AlonzoEraOnwardsDijkstra = L.notSupportedInThisEraL

{-# DEPRECATED
  referenceInputsTxBodyL
  "Use referenceInputsTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
referenceInputsTxBodyL
  :: BabbageEraOnwards era -> Lens' (LedgerTxBody era) (Set L.TxIn)
referenceInputsTxBodyL w = babbageEraOnwardsConstraints w $ txBodyL . L.referenceInputsTxBodyL

{-# DEPRECATED
  collateralReturnTxBodyL
  "Use collateralReturnTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
collateralReturnTxBodyL
  :: BabbageEraOnwards era -> Lens' (LedgerTxBody era) (StrictMaybe (L.TxOut (ShelleyLedgerEra era)))
collateralReturnTxBodyL w = babbageEraOnwardsConstraints w $ txBodyL . L.collateralReturnTxBodyL

{-# DEPRECATED
  totalCollateralTxBodyL
  "Use totalCollateralTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
totalCollateralTxBodyL :: BabbageEraOnwards era -> Lens' (LedgerTxBody era) (StrictMaybe L.Coin)
totalCollateralTxBodyL w = babbageEraOnwardsConstraints w $ txBodyL . L.totalCollateralTxBodyL

{-# DEPRECATED
  certsTxBodyL
  "Use certsTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
certsTxBodyL
  :: ShelleyBasedEra era -> Lens' (LedgerTxBody era) (L.StrictSeq (L.TxCert (ShelleyLedgerEra era)))
certsTxBodyL w = shelleyBasedEraConstraints w $ txBodyL . L.certsTxBodyL

{-# DEPRECATED
  votingProceduresTxBodyL
  "Use votingProceduresTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
votingProceduresTxBodyL
  :: ConwayEraOnwards era -> Lens' (LedgerTxBody era) (L.VotingProcedures (ShelleyLedgerEra era))
votingProceduresTxBodyL w = obtainCommonConstraints (convert w) $ txBodyL . L.votingProceduresTxBodyL

{-# DEPRECATED
  proposalProceduresTxBodyL
  "Use proposalProceduresTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
proposalProceduresTxBodyL
  :: ConwayEraOnwards era
  -> Lens' (LedgerTxBody era) (L.OSet (L.ProposalProcedure (ShelleyLedgerEra era)))
proposalProceduresTxBodyL w = obtainCommonConstraints (convert w) $ txBodyL . L.proposalProceduresTxBodyL

{-# DEPRECATED
  currentTreasuryValueTxBodyL
  "Use currentTreasuryValueTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
currentTreasuryValueTxBodyL :: ConwayEraOnwards era -> Lens' (LedgerTxBody era) (StrictMaybe L.Coin)
currentTreasuryValueTxBodyL w = obtainCommonConstraints (convert w) $ txBodyL . L.currentTreasuryValueTxBodyL

{-# DEPRECATED
  treasuryDonationTxBodyL
  "Use treasuryDonationTxBodyL from Cardano.Api.Ledger (via txBodyL) instead."
  #-}
treasuryDonationTxBodyL :: ConwayEraOnwards era -> Lens' (LedgerTxBody era) L.Coin
treasuryDonationTxBodyL w = obtainCommonConstraints (convert w) $ txBodyL . L.treasuryDonationTxBodyL

mkAdaOnlyTxOut
  :: ShelleyBasedEra era
  -> L.Addr
  -> L.Coin
  -> L.TxOut (ShelleyLedgerEra era)
mkAdaOnlyTxOut sbe addr coin =
  mkBasicTxOut sbe addr (mkAdaValue sbe coin)

mkBasicTxOut
  :: ShelleyBasedEra era
  -> L.Addr
  -> L.Value (ShelleyLedgerEra era)
  -> L.TxOut (ShelleyLedgerEra era)
mkBasicTxOut sbe addr value =
  shelleyBasedEraConstraints sbe $ L.mkBasicTxOut addr value

mkAdaValue :: ShelleyBasedEra era -> L.Coin -> L.Value (ShelleyLedgerEra era)
mkAdaValue sbe coin =
  shelleyBasedEraConstraints sbe $
    L.modifyCoin (const coin) mempty

adaAssetL :: ShelleyBasedEra era -> Lens' (L.Value (ShelleyLedgerEra era)) L.Coin
adaAssetL sbe =
  shelleyBasedEraConstraints sbe $
    lens
      L.coin
      (\v c -> L.modifyCoin (const c) v)

multiAssetL
  :: MaryEraOnwards era -> Lens' L.MaryValue L.MultiAsset
multiAssetL w =
  maryEraOnwardsConstraints w $
    lens
      (\(L.MaryValue _ ma) -> ma)
      (\(L.MaryValue c _) ma -> L.MaryValue c ma)

{-# DEPRECATED
  valueTxOutL
  "Use valueTxOutL from Cardano.Api.Ledger instead."
  #-}
valueTxOutL
  :: ShelleyBasedEra era -> Lens' (L.TxOut (ShelleyLedgerEra era)) (L.Value (ShelleyLedgerEra era))
valueTxOutL sbe = shelleyBasedEraConstraints sbe L.valueTxOutL

{-# DEPRECATED
  valueTxOutAdaAssetL
  "Use coinTxOutL from Cardano.Api.Ledger instead."
  #-}
valueTxOutAdaAssetL :: ShelleyBasedEra era -> Lens' (L.TxOut (ShelleyLedgerEra era)) L.Coin
valueTxOutAdaAssetL sbe = valueTxOutL sbe . adaAssetL sbe
