{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A read-only view of a decoded transaction.
--
-- 'Cardano.Api.Experimental.Tx.TxBodyContent' is for building: it pairs
-- inputs, mints, certificates and so on with the witnesses that unlock them.
-- A decoded transaction does not record those pairings (what unlocks an
-- input is decided by the output it spends, which is not in the
-- transaction), so viewing has its own types. They hold only what the
-- transaction contains: the body fields, split by transaction level like
-- 'Cardano.Api.Experimental.Tx.BodyContent', and the witness set, kept
-- separate.
--
-- Fields use the types cardano-api uses for the same data when building. A
-- field has a ledger type only where building uses that ledger type too, or
-- where cardano-api has no type for the data.
module Cardano.Api.Experimental.Tx.Internal.View
  ( -- * A decoded transaction
    TxView (..)

    -- * Body fields
  , BodyView (..)
  , TopTxOnlyView (..)

    -- * Witness set
  , WitnessesView (..)
  , RedeemerView (..)
  , UndecodedPlutusScript (..)
  )
where

import Cardano.Api.Address (StakeAddress)
import Cardano.Api.Experimental.AnyScript (AnyScript (..))
import Cardano.Api.Experimental.Certificate qualified as Exp
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness (AnyWitnessable (..))
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness (PlutusScriptPurpose)
import Cardano.Api.Experimental.Simple.Script (SimpleScript (..))
import Cardano.Api.Experimental.Tx.Internal.BodyContent (TxOut (..))
import Cardano.Api.Key.Internal (Hash, PaymentKey)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script
  ( AnyPlutusScriptVersion (..)
  , ExecutionUnits
  , IsPlutusScriptLanguage
  , PlutusScript (..)
  , PlutusScriptVersion
  )
import Cardano.Api.Plutus.Internal.ScriptData (HashableScriptData)
import Cardano.Api.Tx.Internal.Sign (ScriptValidity)
import Cardano.Api.Tx.Internal.TxIn (TxId, TxIn)
import Cardano.Api.Tx.Internal.TxMetadata (TxMetadata (..))
import Cardano.Api.Value.Internal (PolicyAssets, PolicyId)

import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as L (TxLevel (..))

import Data.Map.Strict (Map)
import Data.Word (Word32)

-- | A decoded transaction at either level: its ID, its body fields and its
-- witness set. A top-level transaction is @'TxView' 'L.TopTx' era@; a
-- Dijkstra sub-transaction is @'TxView' 'L.SubTx' era@.
data TxView (l :: L.TxLevel) era = TxView
  { txViewId :: TxId
  -- ^ The transaction ID: the hash of the body.
  , txViewBody :: BodyView l era
  -- ^ The body fields, with the metadata and the auxiliary scripts.
  , txViewWitnesses :: WitnessesView era
  -- ^ The witness set: signatures, scripts, datums and redeemers.
  }

-- | The body fields of a decoded transaction, plus its metadata and
-- auxiliary scripts. The fields both levels have are here; the top-level only
-- fields are in 'TopTxOnlyView'.
--
-- Most fields have the type 'Cardano.Api.Experimental.Tx.BodyContent' uses
-- for the same data, without the witness it is paired with there. The
-- differences: total collateral, return collateral and extra key witnesses
-- are not wrapped in the builder's newtypes, and extra key witnesses are a
-- field of both levels (see 'bvExtraKeyWits'). The view functions leave the
-- fields that only exist from the Dijkstra era on empty in Conway.
data BodyView (l :: L.TxLevel) era = BodyView
  { bvIns :: [TxIn]
  -- ^ Inputs to spend, sorted. There is no witness here: the transaction does
  -- not say what unlocks each input. An input with no redeemer may still be
  -- locked by a native script.
  , bvInsReference :: [TxIn]
  -- ^ Reference inputs, sorted: outputs the transaction reads (for example
  -- for their scripts or datums) but does not spend.
  , bvOuts :: [TxOut era]
  -- ^ Outputs, in the body's order.
  , bvValidityLowerBound :: Maybe L.SlotNo
  -- ^ The first slot the transaction is valid in (inclusive). 'Nothing': no
  -- lower bound.
  , bvValidityUpperBound :: Maybe L.SlotNo
  -- ^ The slot the transaction stops being valid in (exclusive): it is valid
  -- only before it. 'Nothing': no upper bound.
  , bvMetadata :: TxMetadata
  -- ^ The metadata from the auxiliary data. Empty when there is none.
  , bvAuxScripts :: [SimpleScript era]
  -- ^ Native scripts in the auxiliary data. Nothing runs them.
  , bvAuxPlutusScripts :: [UndecodedPlutusScript]
  -- ^ Plutus scripts in the auxiliary data, as bytes: nothing runs them.
  -- Building cannot add these, so 'Cardano.Api.Experimental.Tx.BodyContent'
  -- has no field for them.
  , bvWithdrawals :: [(StakeAddress, L.Coin)]
  -- ^ Rewards to withdraw: each account and the amount in lovelace, sorted by
  -- account.
  , bvCertificates :: [Exp.Certificate era]
  -- ^ Certificates, in the body's order.
  , bvMintValue :: Map PolicyId PolicyAssets
  -- ^ Tokens to mint (positive amounts) or burn (negative amounts), by policy.
  , bvExtraKeyWits :: [Hash PaymentKey]
  -- ^ Keys that must sign the transaction (required signers). In Conway, the
  -- body's required signer hashes. From Dijkstra on, the key guards, as
  -- 'L.reqSignerHashesTxBodyG' reports them, so they are in 'bvGuards' too.
  -- Both levels have this field; the builder has it for top-level bodies
  -- only.
  , bvProposalProcedures :: [L.ProposalProcedure era]
  -- ^ Governance proposals, in the body's order.
  , bvVotingProcedures :: L.VotingProcedures era
  -- ^ Governance votes: for each voter, its vote on each governance action.
  , bvCurrentTreasuryValue :: Maybe L.Coin
  -- ^ The treasury amount, in lovelace, that the transaction expects. If it
  -- is there, the ledger rejects the transaction unless it equals the current
  -- treasury. 'Nothing': not in the body.
  , bvTreasuryDonation :: L.Coin
  -- ^ Lovelace donated to the treasury. Zero when the transaction donates
  -- nothing.
  , bvScriptIntegrityHash :: Maybe L.ScriptIntegrityHash
  -- ^ The hash of the redeemers, the datums, and the cost models of the
  -- Plutus versions the scripts use. 'Nothing': not in the body.
  , bvAuxDataHash :: Maybe L.TxAuxDataHash
  -- ^ The hash of the auxiliary data (metadata and auxiliary scripts).
  -- 'Nothing': not in the body.
  , bvNetworkId :: Maybe L.Network
  -- ^ The network the transaction is for. If it is there, the ledger rejects
  -- the transaction on other networks. 'Nothing': not in the body.
  , bvGuards :: [L.Credential L.Guard]
  -- ^ From Dijkstra on. Guards, in the body's order: keys that must sign and
  -- scripts that must approve. They replace required signers.
  , bvRequiredTopLevelGuards :: Map (L.Credential L.Guard) (L.StrictMaybe (L.Data era))
  -- ^ From Dijkstra on. Guards that the top-level transaction must have. A
  -- Plutus script guard comes with a datum; key and native script guards
  -- have none.
  , bvDirectDeposits :: L.DirectDeposits
  -- ^ From Dijkstra on. Lovelace to deposit into registered accounts, by
  -- account.
  , bvAccountBalanceIntervals :: L.AccountBalanceIntervals era
  -- ^ From Dijkstra on. Balances that accounts must have, in lovelace. For
  -- each account: a range (lower bound inclusive, upper bound exclusive) or an
  -- exact amount. Checked before this body's withdrawals, certificates and
  -- direct deposits: a top-level body checks after all its sub-transactions,
  -- and a sub-transaction after the ones before it.
  , bvTopTxOnly :: TopTxOnlyView l era
  -- ^ The fields only a top-level transaction has. 'NoTopTxFields' for a
  -- sub-transaction.
  }

-- | The fields only a top-level transaction has. Matching the constructor
-- tells you the level: 'TopTxView' for a top-level body, 'NoTopTxFields' for a
-- sub-transaction, which has none of these fields.
data TopTxOnlyView (l :: L.TxLevel) era where
  TopTxView
    :: { tvInsCollateral :: [TxIn]
       -- ^ Collateral inputs, sorted. The ledger takes them if a Plutus
       -- script fails.
       , tvTotalCollateral :: Maybe L.Coin
       -- ^ The total collateral, in lovelace. 'Nothing': not in the body.
       , tvReturnCollateral :: Maybe (TxOut era)
       -- ^ The output that gets the rest of the collateral back if a Plutus
       -- script fails. 'Nothing': not in the body.
       , tvFee :: L.Coin
       -- ^ The fee, in lovelace.
       , tvScriptValidity :: ScriptValidity
       -- ^ Whether the Plutus scripts pass. @ScriptInvalid@: they fail, so the
       -- ledger takes only the collateral.
       , tvSubTransactions :: [TxView L.SubTx era]
       -- ^ From Dijkstra on. Sub-transactions, in the body's order, each with
       -- its own witness set.
       , tvStartingAccountBalanceIntervals :: L.AccountBalanceIntervals era
       -- ^ From Dijkstra on. Like 'bvAccountBalanceIntervals', but checked
       -- once, before the whole transaction, sub-transactions included.
       }
    -> TopTxOnlyView L.TopTx era
  -- | A sub-transaction has none of these fields.
  NoTopTxFields :: TopTxOnlyView L.SubTx era

-- | A transaction's witness set, as decoded.
data WitnessesView era = WitnessesView
  { wvKeyWitnesses :: [L.WitVKey L.Witness]
  -- ^ Signatures: each key and its signature of the body, in the ledger types
  -- 'Cardano.Api.Experimental.Tx.signTx' takes.
  , wvBootstrapWitnesses :: [L.BootstrapWitness]
  -- ^ Signatures for Byron addresses.
  , wvScripts :: Map L.ScriptHash (Either UndecodedPlutusScript (AnyScript era))
  -- ^ Scripts, by hash. 'Right' when the script deserialises at the era's
  -- latest protocol version, the same check the decoder of 'AnyScript' makes.
  -- 'Left' for a Plutus script that does not: the ledger decodes a Plutus
  -- script as bytes and only checks them when it validates the transaction.
  -- Native scripts are always 'Right'.
  , wvDatums :: Map L.DataHash (L.Data era)
  -- ^ Datums, by hash: every datum in the witness set, typed like the
  -- supplemental datums of 'Cardano.Api.Experimental.Tx.BodyContent'.
  -- Telling which ones are supplemental would need the UTxO.
  , wvRedeemers :: [RedeemerView era]
  -- ^ Redeemers, sorted by purpose, then by index.
  }

-- | A redeemer, with the types a Plutus script witness uses for it when
-- building.
data RedeemerView era = RedeemerView
  { rvPurpose :: PlutusScriptPurpose
  -- ^ What the redeemer is for: the kind of body item its pointer points at
  -- (an input, a minting policy, a certificate, and so on).
  , rvIndex :: Word32
  -- ^ Which item of that kind: its position in the ledger's order, counting
  -- from 0.
  , rvItem :: Maybe (AnyWitnessable era)
  -- ^ The body item at that index, found with 'L.redeemerPointerInverse' on
  -- the body of the transaction that holds the redeemer. 'Nothing' if the
  -- body has no item at that index. The ledger would reject such a
  -- transaction, but the redeemer is still shown.
  , rvData :: HashableScriptData
  -- ^ The data the redeemer passes to the script.
  , rvExUnits :: ExecutionUnits
  -- ^ The script's budget: memory and CPU steps.
  }

-- | A Plutus script kept as bytes, not deserialised.
data UndecodedPlutusScript where
  -- | The script's Plutus version, and its bytes as the transaction has them.
  UndecodedPlutusScript
    :: IsPlutusScriptLanguage lang
    => PlutusScriptVersion lang
    -> PlutusScript lang
    -> UndecodedPlutusScript

deriving instance Show UndecodedPlutusScript

-- | Equal when the versions and the bytes are equal.
instance Eq UndecodedPlutusScript where
  UndecodedPlutusScript v1 (PlutusScriptSerialised bytes1) == UndecodedPlutusScript v2 (PlutusScriptSerialised bytes2) =
    AnyPlutusScriptVersion v1 == AnyPlutusScriptVersion v2 && bytes1 == bytes2

deriving instance Eq (RedeemerView era)

deriving instance Show (RedeemerView era)

deriving instance L.AlonzoEraTx era => Eq (WitnessesView era)

deriving instance L.AlonzoEraTx era => Show (WitnessesView era)

deriving instance (L.AlonzoEraTx era, L.ConwayEraTxBody era) => Eq (TopTxOnlyView l era)

deriving instance (L.AlonzoEraTx era, L.ConwayEraTxBody era) => Show (TopTxOnlyView l era)

deriving instance (L.AlonzoEraTx era, L.ConwayEraTxBody era) => Eq (BodyView l era)

deriving instance (L.AlonzoEraTx era, L.ConwayEraTxBody era) => Show (BodyView l era)

deriving instance (L.AlonzoEraTx era, L.ConwayEraTxBody era) => Eq (TxView l era)

deriving instance (L.AlonzoEraTx era, L.ConwayEraTxBody era) => Show (TxView l era)
