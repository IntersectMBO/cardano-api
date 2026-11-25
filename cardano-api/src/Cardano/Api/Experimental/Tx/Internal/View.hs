{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
  , viewUnsignedTx
  , viewLedgerTx
  , viewSignedSubTx
  , viewUnsignedSubTx

    -- * Body fields
  , BodyView (..)
  , TopTxOnlyView (..)

    -- * Witness set
  , WitnessesView (..)
  , RedeemerView (..)
  , UndecodedPlutusScript (..)
  )
where

import Cardano.Api.Address (StakeAddress, fromShelleyStakeAddr)
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import Cardano.Api.Experimental.AnyScript (AnyScript (..))
import Cardano.Api.Experimental.Certificate qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
  ( AnyWitnessable (..)
  , Witnessable (..)
  , toPlutusScriptPurposeIndex
  )
import Cardano.Api.Experimental.Plutus.Internal.Script (PlutusScriptInEra (..))
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness (PlutusScriptPurpose)
import Cardano.Api.Experimental.Simple.Script (SimpleScript (..))
import Cardano.Api.Experimental.Tx.Internal.BodyContent (TxOut (..))
import Cardano.Api.Experimental.Tx.Internal.SubTransaction
  ( SignedSubTx (..)
  , UnsignedSubTx (..)
  )
import Cardano.Api.Experimental.Tx.Internal.Type (UnsignedTx (..))
import Cardano.Api.Key.Internal (Hash (PaymentKeyHash), PaymentKey)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script
  ( AnyPlutusScriptVersion (..)
  , ExecutionUnits
  , IsPlutusScriptLanguage
  , PlutusScript (..)
  , PlutusScriptVersion
  , fromAlonzoExUnits
  , fromAlonzoLanguage
  )
import Cardano.Api.Plutus.Internal.ScriptData (HashableScriptData, fromAlonzoData)
import Cardano.Api.Tx.Internal.Sign (ScriptValidity, isValidToScriptValidity)
import Cardano.Api.Tx.Internal.TxIn (TxId, TxIn, fromShelleyTxId, fromShelleyTxIn)
import Cardano.Api.Tx.Internal.TxMetadata (TxMetadata (..), fromShelleyMetadata)
import Cardano.Api.Value.Internal
  ( PolicyAssets
  , PolicyId
  , fromMaryPolicyID
  , multiAssetToPolicyAssets
  )

import Cardano.Ledger.Alonzo.Plutus.Context qualified as L
  ( EraPlutusContext (..)
  , SupportedPlutusRunnable (..)
  )
import Cardano.Ledger.Alonzo.Scripts qualified as L (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as L (TxLevel (..))
import Cardano.Ledger.Dijkstra.TxBody qualified as L
import Cardano.Ledger.Keys qualified as L (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Lens.Micro ((^.))

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

-- | View a decoded top-level transaction. If it has key witnesses anyway, the
-- view keeps them.
viewUnsignedTx :: Era era -> UnsignedTx (LedgerEra era) -> TxView L.TopTx (LedgerEra era)
viewUnsignedTx era (UnsignedTx tx) = viewLedgerTx era tx

-- | View a ledger top-level transaction.
viewLedgerTx :: Era era -> L.Tx L.TopTx (LedgerEra era) -> TxView L.TopTx (LedgerEra era)
viewLedgerTx era tx =
  obtainCommonConstraints era $
    TxView
      { txViewId = fromShelleyTxId (L.txIdTx tx)
      , txViewBody = viewBody era tx (viewTopTxOnly era tx)
      , txViewWitnesses = viewWitnesses era tx
      }

-- | View a signed sub-transaction.
viewSignedSubTx :: SignedSubTx -> TxView L.SubTx (LedgerEra DijkstraEra)
viewSignedSubTx (SignedSubTx tx) = viewLedgerSubTx tx

-- | View an unsigned sub-transaction. If it has key witnesses anyway, the view
-- keeps them.
viewUnsignedSubTx :: UnsignedSubTx -> TxView L.SubTx (LedgerEra DijkstraEra)
viewUnsignedSubTx (UnsignedSubTx tx) = viewLedgerSubTx tx

viewLedgerSubTx :: L.Tx L.SubTx (LedgerEra DijkstraEra) -> TxView L.SubTx (LedgerEra DijkstraEra)
viewLedgerSubTx tx =
  TxView
    { txViewId = fromShelleyTxId (L.txIdTx tx)
    , txViewBody = viewBody DijkstraEra tx NoTopTxFields
    , txViewWitnesses = viewWitnesses DijkstraEra tx
    }

-- | The fields only a top-level body has. The lenses used here are typed
-- @'L.TxBody' 'L.TopTx'@ in the ledger.
viewTopTxOnly :: Era era -> L.Tx L.TopTx (LedgerEra era) -> TopTxOnlyView L.TopTx (LedgerEra era)
viewTopTxOnly era tx =
  obtainCommonConstraints era $
    let body = tx ^. L.bodyTxL
     in TopTxView
          { tvInsCollateral = map fromShelleyTxIn (toList (body ^. L.collateralInputsTxBodyL))
          , tvTotalCollateral = L.strictMaybeToMaybe (body ^. L.totalCollateralTxBodyL)
          , tvReturnCollateral = TxOut <$> L.strictMaybeToMaybe (body ^. L.collateralReturnTxBodyL)
          , tvFee = body ^. L.feeTxBodyL
          , tvScriptValidity = isValidToScriptValidity (tx ^. L.isPhase2ValidTxL)
          , tvSubTransactions = case era of
              ConwayEra -> []
              DijkstraEra -> map viewLedgerSubTx (toList (body ^. L.subTransactionsTxBodyL))
          , tvStartingAccountBalanceIntervals = case era of
              ConwayEra -> L.AccountBalanceIntervals mempty
              DijkstraEra -> body ^. L.startingAccountBalanceIntervalsTxBodyL
          }

-- | The fields both levels have. This function is polymorphic in the level,
-- so it can only use the ledger's level-polymorphic lenses: the compiler
-- keeps the top-level only fields out.
viewBody
  :: Era era
  -> L.Tx l (LedgerEra era)
  -> TopTxOnlyView l (LedgerEra era)
  -> BodyView l (LedgerEra era)
viewBody era tx topTxOnly =
  obtainCommonConstraints era $
    let body = tx ^. L.bodyTxL
        auxData = L.strictMaybeToMaybe (tx ^. L.auxDataTxL)
        L.ValidityInterval lowerBound upperBound = body ^. L.vldtTxBodyL
        L.Withdrawals withdrawals = body ^. L.withdrawalsTxBodyL
     in BodyView
          { bvIns = map fromShelleyTxIn (toList (body ^. L.inputsTxBodyL))
          , bvInsReference = map fromShelleyTxIn (toList (body ^. L.referenceInputsTxBodyL))
          , bvOuts = map TxOut (toList (body ^. L.outputsTxBodyL))
          , bvValidityLowerBound = L.strictMaybeToMaybe lowerBound
          , bvValidityUpperBound = L.strictMaybeToMaybe upperBound
          , bvMetadata =
              TxMetadata (fromShelleyMetadata (maybe mempty (^. L.metadataTxAuxDataL) auxData))
          , bvAuxScripts =
              maybe [] (map SimpleScript . toList . (^. L.nativeScriptsTxAuxDataL)) auxData
          , bvAuxPlutusScripts =
              maybe [] (undecodedAuxPlutusScripts . (^. L.plutusScriptsTxAuxDataL)) auxData
          , bvWithdrawals =
              [(fromShelleyStakeAddr account, coin) | (account, coin) <- Map.toList withdrawals]
          , bvCertificates = map Exp.Certificate (toList (body ^. L.certsTxBodyL))
          , bvMintValue = multiAssetToPolicyAssets (body ^. L.mintTxBodyL)
          , bvExtraKeyWits =
              [PaymentKeyHash (L.coerceKeyRole kh) | kh <- toList (body ^. L.reqSignerHashesTxBodyG)]
          , bvProposalProcedures = toList (body ^. L.proposalProceduresTxBodyL)
          , bvVotingProcedures = body ^. L.votingProceduresTxBodyL
          , bvCurrentTreasuryValue = L.strictMaybeToMaybe (body ^. L.currentTreasuryValueTxBodyL)
          , bvTreasuryDonation = body ^. L.treasuryDonationTxBodyL
          , bvScriptIntegrityHash = L.strictMaybeToMaybe (body ^. L.scriptIntegrityHashTxBodyL)
          , bvAuxDataHash = L.strictMaybeToMaybe (body ^. L.auxDataHashTxBodyL)
          , bvNetworkId = L.strictMaybeToMaybe (body ^. L.networkIdTxBodyL)
          , bvGuards = case era of
              ConwayEra -> []
              DijkstraEra -> toList (body ^. L.guardsTxBodyL)
          , bvRequiredTopLevelGuards = case era of
              ConwayEra -> mempty
              DijkstraEra -> body ^. L.requiredTopLevelGuardsL
          , bvDirectDeposits = case era of
              ConwayEra -> L.DirectDeposits mempty
              DijkstraEra -> body ^. L.directDepositsTxBodyL
          , bvAccountBalanceIntervals = case era of
              ConwayEra -> L.AccountBalanceIntervals mempty
              DijkstraEra -> body ^. L.accountBalanceIntervalsTxBodyL
          , bvTopTxOnly = topTxOnly
          }

-- | The witness set, with each redeemer pointer resolved against the body
-- of the same transaction.
viewWitnesses :: Era era -> L.Tx l (LedgerEra era) -> WitnessesView (LedgerEra era)
viewWitnesses era tx =
  obtainCommonConstraints era $
    let wits = tx ^. L.witsTxL
        body = tx ^. L.bodyTxL
        L.TxDats datums = wits ^. L.datsTxWitsL
     in WitnessesView
          { wvKeyWitnesses = toList (wits ^. L.addrTxWitsL)
          , wvBootstrapWitnesses = toList (wits ^. L.bootAddrTxWitsL)
          , wvScripts = Map.map (viewScript era) (wits ^. L.scriptTxWitsL)
          , wvDatums = datums
          , wvRedeemers =
              [ RedeemerView
                  { rvPurpose = purpose
                  , rvIndex = index
                  , rvItem = case L.redeemerPointerInverse body pointer of
                      L.SJust item -> Just (viewRedeemerItem era body item)
                      L.SNothing -> Nothing
                  , rvData = fromAlonzoData redeemerData
                  , rvExUnits = fromAlonzoExUnits exUnits
                  }
              | (pointer, (redeemerData, exUnits)) <- Map.toList (L.unRedeemers (wits ^. L.rdmrsTxWitsL))
              , let (purpose, index) = viewRedeemerPointer era pointer
              ]
          }

-- | A witness-set script. A Plutus script that does not deserialise stays as
-- bytes.
viewScript
  :: Era era -> L.Script (LedgerEra era) -> Either UndecodedPlutusScript (AnyScript (LedgerEra era))
viewScript era = case era of
  ConwayEra -> fromAlonzoScript
  DijkstraEra -> fromAlonzoScript
 where
  fromAlonzoScript
    :: forall ledgerEra
     . L.EraPlutusContext ledgerEra
    => L.AlonzoScript ledgerEra
    -> Either UndecodedPlutusScript (AnyScript ledgerEra)
  fromAlonzoScript (L.NativeScript nativeScript) = Right (AnySimpleScript (SimpleScript nativeScript))
  fromAlonzoScript (L.PlutusScript plutusScript) =
    case L.mkSupportedPlutusRunnable (L.eraProtVerHigh @ledgerEra) plutusScript of
      L.SupportedPlutusRunnable runnable
        | Plutus.isValidPlutusRunnable runnable -> Right (AnyPlutusScript (PlutusScriptInEra runnable))
        | otherwise -> Left (L.withPlutusScript plutusScript undecodedPlutus)

  undecodedPlutus :: Plutus.PlutusLanguage lang => Plutus.Plutus lang -> UndecodedPlutusScript
  undecodedPlutus plutus = undecodedPlutusScript (Plutus.plutusLanguage plutus) (Plutus.plutusBinary plutus)

-- | Plutus scripts from auxiliary data, in the order of the ledger's map.
undecodedAuxPlutusScripts
  :: Map Plutus.Language (NonEmpty Plutus.PlutusBinary) -> [UndecodedPlutusScript]
undecodedAuxPlutusScripts scripts =
  [ undecodedPlutusScript language binary
  | (language, binaries) <- Map.toList scripts
  , binary <- toList binaries
  ]

undecodedPlutusScript :: Plutus.Language -> Plutus.PlutusBinary -> UndecodedPlutusScript
undecodedPlutusScript language (Plutus.PlutusBinary bytes) =
  case fromAlonzoLanguage language of
    AnyPlutusScriptVersion version -> UndecodedPlutusScript version (PlutusScriptSerialised bytes)

-- | The body item a resolved redeemer pointer points at.
viewRedeemerItem
  :: forall era l
   . Era era
  -> L.TxBody l (LedgerEra era)
  -> L.PlutusPurpose L.AsIxItem (LedgerEra era)
  -> AnyWitnessable (LedgerEra era)
viewRedeemerItem era body purpose = case era of
  ConwayEra -> case purpose of
    L.ConwaySpending (L.AsIxItem _ txIn) -> AnyWitnessable (WitTxIn (fromShelleyTxIn txIn))
    L.ConwayMinting (L.AsIxItem _ policyId) -> mint policyId
    L.ConwayCertifying (L.AsIxItem _ cert) -> AnyWitnessable (WitTxCert cert)
    L.ConwayWithdrawing (L.AsIxItem _ account) -> withdrawal account
    L.ConwayVoting (L.AsIxItem _ voter) -> AnyWitnessable (WitVote voter)
    L.ConwayProposing (L.AsIxItem _ proposal) -> AnyWitnessable (WitProposal proposal)
  DijkstraEra -> case purpose of
    L.DijkstraSpending (L.AsIxItem _ txIn) -> AnyWitnessable (WitTxIn (fromShelleyTxIn txIn))
    L.DijkstraMinting (L.AsIxItem _ policyId) -> mint policyId
    L.DijkstraCertifying (L.AsIxItem _ cert) -> AnyWitnessable (WitTxCert cert)
    L.DijkstraWithdrawing (L.AsIxItem _ account) -> withdrawal account
    L.DijkstraVoting (L.AsIxItem _ voter) -> AnyWitnessable (WitVote voter)
    L.DijkstraProposing (L.AsIxItem _ proposal) -> AnyWitnessable (WitProposal proposal)
    L.DijkstraGuarding (L.AsIxItem _ scriptHash) -> AnyWitnessable (WitGuard (L.ScriptHashObj scriptHash))
 where
  -- The pointer was resolved against this body, so the policy and the
  -- account are there.
  mint :: L.PolicyID -> AnyWitnessable (LedgerEra era)
  mint policyId =
    obtainCommonConstraints era $
      let apiPolicyId = fromMaryPolicyID policyId
          minted = multiAssetToPolicyAssets (body ^. L.mintTxBodyL)
       in AnyWitnessable (WitMint apiPolicyId (Map.findWithDefault mempty apiPolicyId minted))
  withdrawal :: L.AccountAddress -> AnyWitnessable (LedgerEra era)
  withdrawal account =
    obtainCommonConstraints era $
      let L.Withdrawals withdrawals = body ^. L.withdrawalsTxBodyL
       in AnyWitnessable
            (WitWithdrawal (fromShelleyStakeAddr account) (Map.findWithDefault mempty account withdrawals))

-- | The purpose and index of a redeemer pointer.
viewRedeemerPointer
  :: Era era -> L.PlutusPurpose L.AsIx (LedgerEra era) -> (PlutusScriptPurpose, Word32)
viewRedeemerPointer era = case era of
  ConwayEra -> toPlutusScriptPurposeIndex ShelleyBasedEraConway
  DijkstraEra -> toPlutusScriptPurposeIndex ShelleyBasedEraDijkstra

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
