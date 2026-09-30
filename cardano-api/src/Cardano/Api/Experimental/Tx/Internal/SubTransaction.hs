{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Experimental.Tx.Internal.SubTransaction
  ( UnsignedSubTx (..)
  , SignedSubTx (..)
  , makeUnsignedSubTx
  , makeSubTxKeyWitness
  , signSubTx
  , makeSignedSubTx
  , getUnsignedSubTxId
  , getSignedSubTxId
  , setTxSignedSubTransactions

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Tx.Internal.BodyContent
  ( MakeUnsignedTxError (..)
  , SubTxBodyContent
  , TxBodyContent
  , TxOut (..)
  , collectTxBodyScriptWitnessRequirements
  , convCertificates
  , convMintValue
  , convPParamsToScriptIntegrityHash
  , convProposalProcedures
  , convReferenceInputs
  , convTxIns
  , convVotingProcedures
  , convWithdrawals
  , setTxSubTransactions
  , subTxAccountBalanceIntervals
  , subTxAuxScripts
  , subTxCertificates
  , subTxCurrentTreasuryValue
  , subTxDirectDeposits
  , subTxGuards
  , subTxIns
  , subTxInsReference
  , subTxMetadata
  , subTxMintValue
  , subTxOuts
  , subTxProposalProcedures
  , subTxProtocolParams
  , subTxRequiredTopLevelGuards
  , subTxTreasuryDonation
  , subTxValidityLowerBound
  , subTxValidityUpperBound
  , subTxVotingProcedures
  , subTxWithdrawals
  , toAuxiliaryData
  )
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
  ( TxScriptWitnessRequirements (..)
  )
import Cardano.Api.HasTypeProxy (HasTypeProxy (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Serialise.Cbor (SerialiseAsCBOR (..))
import Cardano.Api.Serialise.TextEnvelope.Internal
  ( HasTextEnvelope (..)
  , TextEnvelopeType (..)
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxIn (TxId, fromShelleyTxId)

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Core qualified as L
  ( HashAnnotated (..)
  , TxLevel (..)
  )
import Cardano.Ledger.Dijkstra.TxBody qualified as L
  ( DijkstraEraTxBody (accountBalanceIntervalsTxBodyL, requiredTopLevelGuardsL)
  )

import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Lens.Micro

-- Sub-transactions exist from the Dijkstra era onwards, and Dijkstra is the
-- only such era, so the types below are fixed to it rather than indexed by the
-- era. Once a second era has sub-transactions, adding the index back is
-- mechanical.

-- | A sub-transaction with script witnesses but no key witnesses.
newtype UnsignedSubTx = UnsignedSubTx (L.Tx L.SubTx (LedgerEra DijkstraEra))
  deriving (Eq, Show)

-- | A sub-transaction with its key witnesses, see 'setTxSignedSubTransactions'.
newtype SignedSubTx = SignedSubTx (L.Tx L.SubTx (LedgerEra DijkstraEra))
  deriving (Eq, Show)

instance HasTypeProxy UnsignedSubTx where
  data AsType UnsignedSubTx = AsUnsignedSubTx
  proxyToAsType _ = AsUnsignedSubTx

instance HasTypeProxy SignedSubTx where
  data AsType SignedSubTx = AsSignedSubTx
  proxyToAsType _ = AsSignedSubTx

instance SerialiseAsCBOR UnsignedSubTx where
  serialiseToCBOR (UnsignedSubTx tx) = Ledger.serialize' subTxProtVer tx
  deserialiseFromCBOR _ bs =
    UnsignedSubTx
      <$> Ledger.decodeFullAnnotator subTxProtVer "UnsignedSubTx" Ledger.decCBOR (LBS.fromStrict bs)

instance SerialiseAsCBOR SignedSubTx where
  serialiseToCBOR (SignedSubTx tx) = Ledger.serialize' subTxProtVer tx
  deserialiseFromCBOR _ bs =
    SignedSubTx
      <$> Ledger.decodeFullAnnotator subTxProtVer "SignedSubTx" Ledger.decCBOR (LBS.fromStrict bs)

subTxProtVer :: L.Version
subTxProtVer = L.eraProtVerHigh @(LedgerEra DijkstraEra)

-- The envelope types follow the @"Tx DijkstraEra"@ naming of top-level transactions.

instance HasTextEnvelope UnsignedSubTx where
  textEnvelopeTypes _ = pure $ TextEnvelopeType "Unwitnessed SubTx DijkstraEra"
  textEnvelopeDefaultDescr _ = "Ledger Cddl Format"

instance HasTextEnvelope SignedSubTx where
  textEnvelopeTypes _ = pure $ TextEnvelopeType "Witnessed SubTx DijkstraEra"
  textEnvelopeDefaultDescr _ = "Ledger Cddl Format"

-- | The hash of the sub-transaction body. Unchanged by signing.
getUnsignedSubTxId :: UnsignedSubTx -> TxId
getUnsignedSubTxId (UnsignedSubTx tx) = fromShelleyTxId (L.txIdTx tx)

getSignedSubTxId :: SignedSubTx -> TxId
getSignedSubTxId (SignedSubTx tx) = fromShelleyTxId (L.txIdTx tx)

-- | Embed signed sub-transactions in a top-level transaction body, replacing
-- any that are already there.
setTxSignedSubTransactions
  :: [SignedSubTx]
  -> TxBodyContent (LedgerEra DijkstraEra)
  -> TxBodyContent (LedgerEra DijkstraEra)
setTxSignedSubTransactions subTxs = setTxSubTransactions [tx | SignedSubTx tx <- subTxs]

-- | Build a sub-transaction body and its script witness set from
-- 'SubTxBodyContent'.
makeUnsignedSubTx
  :: SubTxBodyContent (LedgerEra DijkstraEra)
  -> Either MakeUnsignedTxError UnsignedSubTx
makeUnsignedSubTx st = do
  let TxScriptWitnessRequirements languages scripts datums redeemers =
        collectTxBodyScriptWitnessRequirements @_ @DijkstraEra st

  scriptIntegrityHash <-
    convPParamsToScriptIntegrityHash @DijkstraEra
      (subTxProtocolParams st)
      redeemers
      datums
      languages

  let txAuxData = toAuxiliaryData @DijkstraEra (subTxMetadata st) (subTxAuxScripts st)

      body =
        L.mkBasicTxBody
          & L.inputsTxBodyL .~ convTxIns (subTxIns st)
          & L.referenceInputsTxBodyL .~ convReferenceInputs (subTxInsReference st)
          & L.outputsTxBodyL .~ fromList [o | TxOut o <- subTxOuts st]
          & L.vldtTxBodyL . L.invalidBeforeL .~ L.maybeToStrictMaybe (subTxValidityLowerBound st)
          & L.vldtTxBodyL . L.invalidHereAfterL .~ L.maybeToStrictMaybe (subTxValidityUpperBound st)
          & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
          & L.withdrawalsTxBodyL .~ convWithdrawals (subTxWithdrawals st)
          & L.certsTxBodyL .~ convCertificates (subTxCertificates st)
          & L.mintTxBodyL .~ convMintValue (subTxMintValue st)
          & L.auxDataHashTxBodyL .~ L.maybeToStrictMaybe (L.hashTxAuxData <$> txAuxData)
          & L.proposalProceduresTxBodyL .~ convProposalProcedures (subTxProposalProcedures st)
          & L.votingProceduresTxBodyL .~ convVotingProcedures (subTxVotingProcedures st)
          & L.treasuryDonationTxBodyL .~ fromMaybe (L.Coin 0) (subTxTreasuryDonation st)
          & L.currentTreasuryValueTxBodyL .~ L.maybeToStrictMaybe (subTxCurrentTreasuryValue st)
          & L.guardsTxBodyL .~ subTxGuards st
          & L.requiredTopLevelGuardsL .~ subTxRequiredTopLevelGuards st
          & L.directDepositsTxBodyL .~ subTxDirectDeposits st
          & L.accountBalanceIntervalsTxBodyL .~ subTxAccountBalanceIntervals st

      scriptWitnesses =
        L.mkBasicTxWits
          & L.scriptTxWitsL .~ fromList [(L.hashScript sw, sw) | sw <- scripts]
          & L.datsTxWitsL .~ datums
          & L.rdmrsTxWitsL .~ redeemers

  Right $
    UnsignedSubTx $
      L.mkBasicTx body
        & L.witsTxL .~ scriptWitnesses
        & L.auxDataTxL .~ L.maybeToStrictMaybe txAuxData

-- | Sign the body of an unsigned sub-transaction with one key.
makeSubTxKeyWitness
  :: UnsignedSubTx
  -> ShelleyWitnessSigningKey
  -> L.WitVKey L.Witness
makeSubTxKeyWitness (UnsignedSubTx unsigned) wsk =
  let txhash = L.extractHash $ L.hashAnnotated (unsigned ^. L.bodyTxL)
      sk = toShelleySigningKey wsk
      vk = getShelleyKeyWitnessVerificationKey sk
   in L.WitVKey vk (makeShelleySignature txhash sk)

-- | Add key witnesses to an unsigned sub-transaction.
signSubTx
  :: [L.BootstrapWitness]
  -> [L.WitVKey L.Witness]
  -> UnsignedSubTx
  -> SignedSubTx
signSubTx bootstrapWits shelleyKeyWits (UnsignedSubTx unsigned) =
  let keyWits =
        L.mkBasicTxWits
          & L.addrTxWitsL .~ Set.fromList shelleyKeyWits
          & L.bootAddrTxWitsL .~ Set.fromList bootstrapWits
   in SignedSubTx $ unsigned & L.witsTxL %~ (keyWits <>)

-- | Build and sign a sub-transaction in one step.
makeSignedSubTx
  :: [ShelleyWitnessSigningKey]
  -> SubTxBodyContent (LedgerEra DijkstraEra)
  -> Either MakeUnsignedTxError SignedSubTx
makeSignedSubTx keys st = do
  unsigned <- makeUnsignedSubTx st
  Right $ signSubTx [] (map (makeSubTxKeyWitness unsigned) keys) unsigned
