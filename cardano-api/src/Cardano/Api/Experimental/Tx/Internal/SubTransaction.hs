{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Experimental.Tx.Internal.SubTransaction
  ( SubTx (..)
  , defaultSubTx
  , UnsignedSubTx (..)
  , SignedSubTx (..)
  , makeUnsignedSubTx
  , makeSubTxKeyWitness
  , signSubTx
  , makeSignedSubTx
  , collectSubTxScriptWitnessRequirements
  , getUnsignedSubTxId
  , getSignedSubTxId
  , setTxSignedSubTransactions

    -- * Reading sub-transactions of any era
  , unsignedSubTxFromSomeTypes
  , signedSubTxFromSomeTypes

    -- * Data family instances
  , AsType (..)

    -- * Setters
  , setSubTxIns
  , setSubTxInsReference
  , setSubTxOuts
  , setSubTxCertificates
  , setSubTxWithdrawals
  , setSubTxValidityLowerBound
  , setSubTxValidityUpperBound
  , setSubTxMintValue
  , setSubTxProtocolParams
  , setSubTxMetadata
  , setSubTxAuxScripts
  , setSubTxProposalProcedures
  , setSubTxVotingProcedures
  , setSubTxCurrentTreasuryValue
  , setSubTxTreasuryDonation
  , setSubTxSupplementalDatums
  , setSubTxGuards
  , setSubTxRequiredTopLevelGuards
  , setSubTxDirectDeposits
  , setSubTxAccountBalanceIntervals
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness (AnyWitness (..))
import Cardano.Api.Experimental.Tx.Internal.TopTx.BodyContent
  ( MakeUnsignedTxError (..)
  , TxBodyContent (..)
  , TxCertificates (..)
  , TxInsReference (..)
  , TxMintValue (..)
  , TxOut (..)
  , TxProposalProcedures (..)
  , TxVotingProcedures (..)
  , TxWithdrawals (..)
  , collectScriptWitnessRequirements
  , convCertificates
  , convMintValue
  , convPParamsToScriptIntegrityHash
  , convProposalProcedures
  , convReferenceInputs
  , convTxIns
  , convVotingProcedures
  , convWithdrawals
  , setTxSubTransactions
  , toAuxiliaryData
  )
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
  ( TxScriptWitnessRequirements (..)
  )
import Cardano.Api.HasTypeProxy (HasTypeProxy (..), asType)
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Serialise.Cbor (SerialiseAsCBOR (..))
import Cardano.Api.Serialise.TextEnvelope.Internal
  ( FromSomeType (..)
  , HasTextEnvelope (..)
  , TextEnvelopeType (..)
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxIn (TxId, TxIn, fromShelleyTxId)
import Cardano.Api.Tx.Internal.TxMetadata (TxMetadata (..))

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Core qualified as L
  ( EraIndependentTxBody
  , HashAnnotated (..)
  , TxLevel (..)
  )
import Cardano.Ledger.Dijkstra.TxBody qualified as L
  ( DijkstraEraTxBody (accountBalanceIntervalsTxBodyL, requiredTopLevelGuardsL)
  )

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.OMap.Strict qualified as LOMap
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Lens.Micro

-- | Content of a Dijkstra sub-transaction body.
--
-- Compared to the top-level 'TxBodyContent', a sub-transaction has no collateral inputs,
-- no total/return collateral, no fee, no required signer key hashes (replaced by guards)
-- and no script validity flag.
data SubTx era
  = SubTx
  { subTxIns :: [(TxIn, AnyWitness era)]
  , subTxInsReference :: TxInsReference era
  , subTxOuts :: [TxOut era]
  , subTxCertificates :: TxCertificates era
  , subTxWithdrawals :: TxWithdrawals era
  , subTxValidityLowerBound :: Maybe L.SlotNo
  , subTxValidityUpperBound :: Maybe L.SlotNo
  , subTxMintValue :: TxMintValue era
  , subTxProtocolParams :: Maybe (L.PParams era)
  , subTxMetadata :: TxMetadata
  , subTxAuxScripts :: [SimpleScript era]
  , subTxProposalProcedures :: Maybe (TxProposalProcedures era)
  , subTxVotingProcedures :: Maybe (TxVotingProcedures era)
  , subTxCurrentTreasuryValue :: Maybe L.Coin
  , subTxTreasuryDonation :: Maybe L.Coin
  , subTxSupplementalDatums :: Map L.DataHash (L.Data era)
  , -- ------------------------------------------------------------
    -- Fields above are shared with the Conway-era 'TxBodyContent'.
    -- Fields below are new in the Dijkstra era.
    -- ------------------------------------------------------------
    subTxGuards :: OSet (L.Credential L.Guard)
  , subTxRequiredTopLevelGuards :: Map (L.Credential L.Guard) (StrictMaybe (L.Data era))
  , subTxDirectDeposits :: L.DirectDeposits
  , subTxAccountBalanceIntervals :: L.AccountBalanceIntervals era
  }

defaultSubTx :: SubTx era
defaultSubTx =
  SubTx
    { subTxIns = []
    , subTxInsReference = TxInsReference mempty Set.empty
    , subTxOuts = []
    , subTxCertificates = TxCertificates OMap.empty
    , subTxWithdrawals = TxWithdrawals mempty
    , subTxValidityLowerBound = Nothing
    , subTxValidityUpperBound = Nothing
    , subTxMintValue = TxMintValue mempty
    , subTxProtocolParams = Nothing
    , subTxMetadata = TxMetadata mempty
    , subTxAuxScripts = []
    , subTxProposalProcedures = Nothing
    , subTxVotingProcedures = Nothing
    , subTxCurrentTreasuryValue = Nothing
    , subTxTreasuryDonation = Nothing
    , subTxSupplementalDatums = mempty
    , subTxGuards = OSet.empty
    , subTxRequiredTopLevelGuards = mempty
    , subTxDirectDeposits = L.DirectDeposits mempty
    , subTxAccountBalanceIntervals = L.AccountBalanceIntervals mempty
    }

-- | A sub-transaction with script witnesses but no key witnesses.
data UnsignedSubTx era
  = ( IsEra era
    , L.EraTx (LedgerEra era)
    , L.HashAnnotated (L.TxBody L.SubTx (LedgerEra era)) L.EraIndependentTxBody
    ) =>
    UnsignedSubTx (L.Tx L.SubTx (LedgerEra era))

deriving instance Eq (UnsignedSubTx era)

deriving instance Show (UnsignedSubTx era)

-- | A sub-transaction with its key witnesses, see 'setTxSignedSubTransactions'.
data SignedSubTx era
  = ( IsEra era
    , L.EraTx (LedgerEra era)
    ) =>
    SignedSubTx (L.Tx L.SubTx (LedgerEra era))

deriving instance Eq (SignedSubTx era)

deriving instance Show (SignedSubTx era)

instance HasTypeProxy era => HasTypeProxy (UnsignedSubTx era) where
  data AsType (UnsignedSubTx era) = AsUnsignedSubTx (AsType era)
  proxyToAsType _ = AsUnsignedSubTx (asType @era)

instance HasTypeProxy era => HasTypeProxy (SignedSubTx era) where
  data AsType (SignedSubTx era) = AsSignedSubTx (AsType era)
  proxyToAsType _ = AsSignedSubTx (asType @era)

-- Decoding dispatches on the era because only Dijkstra has sub-transactions.

instance (HasTypeProxy era, IsEra era) => SerialiseAsCBOR (UnsignedSubTx era) where
  serialiseToCBOR (UnsignedSubTx tx) =
    Ledger.serialize' (L.eraProtVerHigh @(LedgerEra era)) tx
  deserialiseFromCBOR _ bs =
    case useEra @era of
      ConwayEra -> Left subTxUnsupportedDecoderError
      DijkstraEra ->
        UnsignedSubTx
          <$> Ledger.decodeFullAnnotator
            (L.eraProtVerHigh @(LedgerEra era))
            "UnsignedSubTx"
            Ledger.decCBOR
            (LBS.fromStrict bs)

instance (HasTypeProxy era, IsEra era) => SerialiseAsCBOR (SignedSubTx era) where
  serialiseToCBOR (SignedSubTx tx) =
    Ledger.serialize' (L.eraProtVerHigh @(LedgerEra era)) tx
  deserialiseFromCBOR _ bs =
    case useEra @era of
      ConwayEra -> Left subTxUnsupportedDecoderError
      DijkstraEra ->
        SignedSubTx
          <$> Ledger.decodeFullAnnotator
            (L.eraProtVerHigh @(LedgerEra era))
            "SignedSubTx"
            Ledger.decCBOR
            (LBS.fromStrict bs)

subTxUnsupportedDecoderError :: Ledger.DecoderError
subTxUnsupportedDecoderError =
  Ledger.DecoderErrorCustom
    "SubTx"
    "Sub-transactions are only supported from the Dijkstra era onwards."

instance (HasTypeProxy era, IsEra era) => HasTextEnvelope (UnsignedSubTx era) where
  textEnvelopeTypes _ =
    pure $ TextEnvelopeType $ "Unwitnessed SubTx " <> subTxEnvelopeEraName (useEra @era)
  textEnvelopeDefaultDescr _ = "Ledger Cddl Format"

instance (HasTypeProxy era, IsEra era) => HasTextEnvelope (SignedSubTx era) where
  textEnvelopeTypes _ =
    pure $ TextEnvelopeType $ "Witnessed SubTx " <> subTxEnvelopeEraName (useEra @era)
  textEnvelopeDefaultDescr _ = "Ledger Cddl Format"

-- | Era suffix for envelope types, matching @"Tx ConwayEra"@.
subTxEnvelopeEraName :: Era era -> String
subTxEnvelopeEraName = \case
  ConwayEra -> "ConwayEra"
  DijkstraEra -> "DijkstraEra"

-- | One 'FromSomeType' per era, for reading an envelope of unknown era.
unsignedSubTxFromSomeTypes
  :: (forall era. Era era -> UnsignedSubTx era -> b)
  -> [FromSomeType HasTextEnvelope b]
unsignedSubTxFromSomeTypes f =
  [ obtainCommonConstraints era $ FromSomeType (AsUnsignedSubTx (eraAsType era)) (f era)
  | Some era <- [minBound .. maxBound]
  ]

-- | As 'unsignedSubTxFromSomeTypes', for signed sub-transactions.
signedSubTxFromSomeTypes
  :: (forall era. Era era -> SignedSubTx era -> b)
  -> [FromSomeType HasTextEnvelope b]
signedSubTxFromSomeTypes f =
  [ obtainCommonConstraints era $ FromSomeType (AsSignedSubTx (eraAsType era)) (f era)
  | Some era <- [minBound .. maxBound]
  ]

-- | The hash of the sub-transaction body. Unchanged by signing.
getUnsignedSubTxId :: UnsignedSubTx era -> TxId
getUnsignedSubTxId (UnsignedSubTx tx) = fromShelleyTxId (L.txIdTx tx)

getSignedSubTxId :: SignedSubTx era -> TxId
getSignedSubTxId (SignedSubTx tx) = fromShelleyTxId (L.txIdTx tx)

-- | Embed signed sub-transactions in a top-level transaction body.
setTxSignedSubTransactions
  :: [SignedSubTx era]
  -> TxBodyContent (LedgerEra era)
  -> TxBodyContent (LedgerEra era)
setTxSignedSubTransactions = \case
  [] -> \bc -> bc{txSubTransactions = LOMap.empty}
  subTxs@(SignedSubTx _ : _) -> setTxSubTransactions [tx | SignedSubTx tx <- subTxs]

-- | Build a sub-transaction body and its script witness set from 'SubTx'
-- content. Sub-transactions exist from Dijkstra onwards, so this fails in
-- Conway.
makeUnsignedSubTx
  :: forall era
   . Era era
  -> SubTx (LedgerEra era)
  -> Either MakeUnsignedTxError (UnsignedSubTx era)
makeUnsignedSubTx era st = obtainCommonConstraints era $ case era of
  ConwayEra -> Left MakeUnsignedTxSubTransactionsUnsupported
  DijkstraEra -> do
    let TxScriptWitnessRequirements languages scripts datums redeemers =
          collectSubTxScriptWitnessRequirements @era st

    scriptIntegrityHash <-
      convPParamsToScriptIntegrityHash @era
        (subTxProtocolParams st)
        redeemers
        datums
        languages

    let txAuxData = toAuxiliaryData @era (subTxMetadata st) (subTxAuxScripts st)

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

collectSubTxScriptWitnessRequirements
  :: forall era
   . IsEra era
  => SubTx (LedgerEra era)
  -> TxScriptWitnessRequirements (LedgerEra era)
collectSubTxScriptWitnessRequirements
  SubTx
    { subTxIns
    , subTxInsReference
    , subTxCertificates
    , subTxMintValue
    , subTxWithdrawals
    , subTxVotingProcedures
    , subTxProposalProcedures
    , subTxSupplementalDatums
    } =
    collectScriptWitnessRequirements @era
      subTxIns
      subTxInsReference
      subTxCertificates
      subTxMintValue
      subTxWithdrawals
      subTxVotingProcedures
      subTxProposalProcedures
      subTxSupplementalDatums

-- | Sign the body of an unsigned sub-transaction with one key.
makeSubTxKeyWitness
  :: UnsignedSubTx era
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
  -> UnsignedSubTx era
  -> SignedSubTx era
signSubTx bootstrapWits shelleyKeyWits (UnsignedSubTx unsigned) =
  let keyWits =
        L.mkBasicTxWits
          & L.addrTxWitsL .~ Set.fromList shelleyKeyWits
          & L.bootAddrTxWitsL .~ Set.fromList bootstrapWits
   in SignedSubTx $ unsigned & L.witsTxL %~ (keyWits <>)

-- | Build and sign a sub-transaction in one step.
makeSignedSubTx
  :: Era era
  -> [ShelleyWitnessSigningKey]
  -> SubTx (LedgerEra era)
  -> Either MakeUnsignedTxError (SignedSubTx era)
makeSignedSubTx era keys st = do
  unsigned <- makeUnsignedSubTx era st
  Right $ signSubTx [] (map (makeSubTxKeyWitness unsigned) keys) unsigned

-- Setters

setSubTxIns :: [(TxIn, AnyWitness era)] -> SubTx era -> SubTx era
setSubTxIns v st = st{subTxIns = v}

setSubTxInsReference :: TxInsReference era -> SubTx era -> SubTx era
setSubTxInsReference v st = st{subTxInsReference = v}

setSubTxOuts :: [TxOut era] -> SubTx era -> SubTx era
setSubTxOuts v st = st{subTxOuts = v}

setSubTxCertificates :: TxCertificates era -> SubTx era -> SubTx era
setSubTxCertificates v st = st{subTxCertificates = v}

setSubTxWithdrawals :: TxWithdrawals era -> SubTx era -> SubTx era
setSubTxWithdrawals v st = st{subTxWithdrawals = v}

setSubTxValidityLowerBound :: L.SlotNo -> SubTx era -> SubTx era
setSubTxValidityLowerBound v st = st{subTxValidityLowerBound = Just v}

setSubTxValidityUpperBound :: L.SlotNo -> SubTx era -> SubTx era
setSubTxValidityUpperBound v st = st{subTxValidityUpperBound = Just v}

setSubTxMintValue :: TxMintValue era -> SubTx era -> SubTx era
setSubTxMintValue v st = st{subTxMintValue = v}

setSubTxProtocolParams :: L.PParams era -> SubTx era -> SubTx era
setSubTxProtocolParams v st = st{subTxProtocolParams = Just v}

setSubTxMetadata :: TxMetadata -> SubTx era -> SubTx era
setSubTxMetadata v st = st{subTxMetadata = v}

setSubTxAuxScripts :: [SimpleScript era] -> SubTx era -> SubTx era
setSubTxAuxScripts v st = st{subTxAuxScripts = v}

setSubTxProposalProcedures :: TxProposalProcedures era -> SubTx era -> SubTx era
setSubTxProposalProcedures v st = st{subTxProposalProcedures = Just v}

setSubTxVotingProcedures :: TxVotingProcedures era -> SubTx era -> SubTx era
setSubTxVotingProcedures v st = st{subTxVotingProcedures = Just v}

setSubTxCurrentTreasuryValue :: L.Coin -> SubTx era -> SubTx era
setSubTxCurrentTreasuryValue v st = st{subTxCurrentTreasuryValue = Just v}

setSubTxTreasuryDonation :: L.Coin -> SubTx era -> SubTx era
setSubTxTreasuryDonation v st = st{subTxTreasuryDonation = Just v}

setSubTxSupplementalDatums :: Map L.DataHash (L.Data era) -> SubTx era -> SubTx era
setSubTxSupplementalDatums v st = st{subTxSupplementalDatums = v}

setSubTxGuards :: OSet (L.Credential L.Guard) -> SubTx era -> SubTx era
setSubTxGuards v st = st{subTxGuards = v}

setSubTxRequiredTopLevelGuards
  :: Map (L.Credential L.Guard) (StrictMaybe (L.Data era)) -> SubTx era -> SubTx era
setSubTxRequiredTopLevelGuards v st = st{subTxRequiredTopLevelGuards = v}

setSubTxDirectDeposits :: L.DirectDeposits -> SubTx era -> SubTx era
setSubTxDirectDeposits v st = st{subTxDirectDeposits = v}

setSubTxAccountBalanceIntervals :: L.AccountBalanceIntervals era -> SubTx era -> SubTx era
setSubTxAccountBalanceIntervals v st = st{subTxAccountBalanceIntervals = v}
