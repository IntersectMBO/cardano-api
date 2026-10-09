{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Experimental.Tx.Internal.BodyContent
  ( TxCertificates (..)
  , TxReturnCollateral (..)
  , TxTotalCollateral (..)
  , TxExtraKeyWitnesses (..)
  , TxInsReference (..)
  , TxMintValue (..)
  , TxOut (..)
  , TxProposalProcedures (..)
  , TxValidityLowerBound (..)
  , TxVotingProcedures (..)
  , TxWithdrawals (..)

    -- * Transaction body content at either level
  , BodyContent (TxBodyContent, SubTxBodyContent)
  , TxBodyContent
  , SubTxBodyContent
  , defaultTxBodyContent
  , defaultSubTxBodyContent

    -- ** Fields of a top-level body
  , txIns
  , txInsCollateral
  , txInsReference
  , txOuts
  , txTotalCollateral
  , txReturnCollateral
  , txFee
  , txValidityLowerBound
  , txValidityUpperBound
  , txMetadata
  , txAuxScripts
  , txExtraKeyWits
  , txProtocolParams
  , txWithdrawals
  , txCertificates
  , txMintValue
  , txScriptValidity
  , txProposalProcedures
  , txVotingProcedures
  , txCurrentTreasuryValue
  , txTreasuryDonation
  , txSupplementalDatums
  , txGuards
  , txSubTransactions
  , txRequiredTopLevelGuards
  , txDirectDeposits
  , txAccountBalanceIntervals
  , txStartingAccountBalanceIntervals

    -- ** Fields of a sub-transaction body
  , subTxIns
  , subTxInsReference
  , subTxOuts
  , subTxValidityLowerBound
  , subTxValidityUpperBound
  , subTxMetadata
  , subTxAuxScripts
  , subTxProtocolParams
  , subTxWithdrawals
  , subTxCertificates
  , subTxMintValue
  , subTxProposalProcedures
  , subTxVotingProcedures
  , subTxCurrentTreasuryValue
  , subTxTreasuryDonation
  , subTxSupplementalDatums
  , subTxGuards
  , subTxRequiredTopLevelGuards
  , subTxDirectDeposits
  , subTxAccountBalanceIntervals
  , Datum (..)
  , MakeUnsignedTxError (..)
  , extractDatumsAndHashes
  , getDatums
  , collectTxBodyScriptWitnessRequirements
  , makeUnsignedTx
  , extractAllIndexedPlutusScriptWitnesses
  , txMintValueToValue
  , mkTxCertificates
  , mkTxVotingProcedures
  , mkTxProposalProcedures

    -- * Getters and Setters

    -- ** Shared by both levels
  , modTxOuts
  , setTxAuxScripts
  , setTxCertificates
  , setTxCurrentTreasuryValue
  , setTxIns
  , setTxInsReference
  , setTxMetadata
  , setTxMintValue
  , setTxOuts
  , setTxProposalProcedures
  , setTxProtocolParams
  , setTxSupplementalDatums
  , setTxTreasuryDonation
  , setTxValidityLowerBound
  , setTxValidityUpperBound
  , setTxVotingProcedures
  , setTxWithdrawals
  , setTxGuards
  , setTxRequiredTopLevelGuards
  , setTxDirectDeposits
  , setTxAccountBalanceIntervals

    -- ** Top-level bodies only
  , setTxReturnCollateral
  , setTxTotalCollateral
  , setTxExtraKeyWits
  , setTxFee
  , setTxInsCollateral
  , setTxScriptValidity
  , setTxSubTransactions
  , setTxStartingAccountBalanceIntervals

    -- * Internal conversions
  , convTxIns
  , convReferenceInputs
  , convWithdrawals
  , convCertificates
  , convMintValue
  , convProposalProcedures
  , convVotingProcedures
  , convPParamsToScriptIntegrityHash
  , toAuxiliaryData
  , extractWitnessableTxIns
  , extractWitnessableMints
  , extractWitnessableCertificates
  , extractWitnessableWithdrawals
  , extractWitnessableVotes
  , extractWitnessableProposals
  )
where

import Cardano.Api.Address
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import Cardano.Api.Error
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Certificate qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus
  ( AnyIndexedPlutusScriptWitness (..)
  , Witnessable (..)
  , WitnessableItem (..)
  , createIndexedPlutusScriptWitnesses
  )
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
  ( AnyWitness (..)
  , anyScriptWitnessToAnyWitness
  )
import Cardano.Api.Experimental.Tx.Internal.Certificate.Compatible (getTxCertWitness)
import Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
  ( TxScriptWitnessRequirements (..)
  , getTxScriptWitnessesRequirements
  )
import Cardano.Api.Experimental.Tx.Internal.Type
import Cardano.Api.Governance.Internal.Action.VotingProcedure
  ( VotingError (..)
  , mergeVotingProcedures
  )
import Cardano.Api.Key.Internal
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Monad.Error (failEitherWith, liftMaybe)
import Cardano.Api.Plutus.Internal.Script
  ( PlutusScript (..)
  , PlutusScriptVersion (..)
  , ScriptInAnyLang (..)
  , ScriptLanguage (..)
  , fromAllegraTimelock
  , toAllegraTimelock
  )
import Cardano.Api.Plutus.Internal.Script qualified as OldScript
import Cardano.Api.Plutus.Internal.ScriptData qualified as Api
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor (serialiseToCBOR)
import Cardano.Api.Tx.Internal.Body
  ( CtxTx
  , TxIn
  , asGuard
  , toShelleyTxIn
  , toShelleyWithdrawal
  )
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal
  ( PolicyAssets
  , PolicyId
  , Value
  , fromLedgerValue
  , policyAssetsToValue
  , toMaryValue
  )

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxBody qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core qualified as L (TxLevel (..))
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Dijkstra.TxBody qualified as L
  ( DijkstraEraTxBody
      ( accountBalanceIntervalsTxBodyL
      , requiredTopLevelGuardsL
      , startingAccountBalanceIntervalsTxBodyL
      , subTransactionsTxBodyL
      )
  )
import Cardano.Ledger.Plutus.Language (PlutusBinary (..), plutusLanguage)
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Functor
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OMap.Strict qualified as LOMap
import Data.OSet.Strict (OSet)
import Data.OSet.Strict qualified as OSet
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Exts (IsList (..))
import Lens.Micro

-- | Error that can occur when constructing an unsigned transaction.
data MakeUnsignedTxError
  = -- | Plutus scripts are present in the transaction but no protocol
    -- parameters were provided. Protocol parameters are required to
    -- compute the script integrity hash (script_data_hash).
    MakeUnsignedTxMissingProtocolParams
  | -- | Fields were set on the body content that the ledger body of the
    -- target era has no slot for. The body content record is shared by all
    -- eras, so this is only detected when the body is built.
    -- The field names are those of the 'TxBodyContent' record.
    MakeUnsignedTxFieldsNotSupportedInEra (Some Era) (NonEmpty Text)
  deriving (Eq, Show)

instance Error MakeUnsignedTxError where
  prettyError MakeUnsignedTxMissingProtocolParams =
    mconcat
      [ "Transaction uses Plutus scripts but no protocol parameters were provided. "
      , "Protocol parameters are required to compute the script integrity hash "
      , "(script_data_hash) from the cost models."
      ]
  prettyError (MakeUnsignedTxFieldsNotSupportedInEra (Some era) fields) =
    mconcat
      [ "Transaction body content sets fields that do not exist in the "
      , pshow era
      , " era: "
      , pretty . Text.intercalate ", " $ NonEmpty.toList fields
      ]

makeUnsignedTx
  :: forall era
   . Era era
  -> TxBodyContent (LedgerEra era)
  -> Either MakeUnsignedTxError (UnsignedTx (LedgerEra era))
makeUnsignedTx era bc = obtainCommonConstraints era $ do
  let TxScriptWitnessRequirements languages scripts datums redeemers = collectTxBodyScriptWitnessRequirements bc

  -- cardano-api types
  let apiMintValue = txMintValue bc
      apiReferenceInputs = txInsReference bc

      -- Ledger types
      txins = convTxIns $ txIns bc
      collTxIns = convCollateralTxIns bc
      refTxIns = convReferenceInputs apiReferenceInputs
      outs = fromList [o | TxOut o <- txOuts bc]
      protocolParameters = txProtocolParams bc
      fee = txFee bc
      withdrawals = convWithdrawals $ txWithdrawals bc
      certs = convCertificates $ txCertificates bc
      retCollateral = unTxReturnCollateral <$> txReturnCollateral bc
      totCollateral = unTxTotalCollateral <$> txTotalCollateral bc
      txAuxData = toAuxiliaryData (txMetadata bc) (txAuxScripts bc)
      scriptValidity = scriptValidityToIsValid $ txScriptValidity bc

  scriptIntegrityHash <-
    convPParamsToScriptIntegrityHash
      protocolParameters
      redeemers
      datums
      languages

  let setMint = convMintValue apiMintValue
      -- Fields common to all supported eras. Era-specific fields are set in
      -- 'eraSpecificLedgerTxBody'.
      commonLedgerTxBody =
        L.mkBasicTxBody
          & L.inputsTxBodyL .~ txins
          & L.collateralInputsTxBodyL .~ collTxIns
          & L.referenceInputsTxBodyL .~ refTxIns
          & L.outputsTxBodyL .~ outs
          & L.totalCollateralTxBodyL .~ L.maybeToStrictMaybe totCollateral
          & L.collateralReturnTxBodyL .~ L.maybeToStrictMaybe retCollateral
          & L.feeTxBodyL .~ fee
          & L.vldtTxBodyL . L.invalidBeforeL .~ L.maybeToStrictMaybe (txValidityLowerBound bc)
          & L.vldtTxBodyL . L.invalidHereAfterL .~ L.maybeToStrictMaybe (txValidityUpperBound bc)
          & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
          & L.withdrawalsTxBodyL .~ withdrawals
          & L.certsTxBodyL .~ certs
          & L.mintTxBodyL .~ setMint
          & L.auxDataHashTxBodyL .~ L.maybeToStrictMaybe (Ledger.hashTxAuxData <$> txAuxData)
          & L.proposalProceduresTxBodyL .~ convProposalProcedures (txProposalProcedures bc)
          & L.votingProceduresTxBodyL .~ convVotingProcedures (txVotingProcedures bc)
          & L.treasuryDonationTxBodyL .~ fromMaybe (L.Coin 0) (txTreasuryDonation bc)
          & L.currentTreasuryValueTxBodyL .~ L.maybeToStrictMaybe (txCurrentTreasuryValue bc)

      scriptWitnesses =
        L.mkBasicTxWits
          & L.scriptTxWitsL
            .~ fromList
              [ (L.hashScript sw, sw)
              | sw <- scripts
              ]
          & L.datsTxWitsL .~ datums
          & L.rdmrsTxWitsL .~ redeemers

  eraSpecificTxBody <- eraSpecificLedgerTxBody era commonLedgerTxBody bc
  Right $
    UnsignedTx $
      L.mkBasicTx eraSpecificTxBody
        & L.witsTxL .~ scriptWitnesses
        & L.auxDataTxL .~ L.maybeToStrictMaybe (toAuxiliaryData (txMetadata bc) (txAuxScripts bc))
        & L.isPhase2ValidTxL .~ scriptValidity

convTxIns :: [(TxIn, AnyWitness era)] -> Set L.TxIn
convTxIns inputs =
  Set.fromList [toShelleyTxIn txin | (txin, _) <- inputs]

convCollateralTxIns :: TxBodyContent (LedgerEra era) -> Set L.TxIn
convCollateralTxIns b =
  fromList (map toShelleyTxIn $ txInsCollateral b)

convReferenceInputs :: TxInsReference era -> Set L.TxIn
convReferenceInputs (TxInsReference ins _) =
  fromList $ map toShelleyTxIn ins

convWithdrawals :: TxWithdrawals era -> L.Withdrawals
convWithdrawals (TxWithdrawals ws) =
  toShelleyWithdrawal ws

convMintValue :: TxMintValue era -> L.MultiAsset
convMintValue v = do
  let L.MaryValue _coin multiAsset = toMaryValue $ txMintValueToValue v
  multiAsset

convExtraKeyWitnesses
  :: TxExtraKeyWitnesses -> Set (L.KeyHash L.Guard)
convExtraKeyWitnesses (TxExtraKeyWitnesses khs) =
  fromList
    [ asGuard kh
    | PaymentKeyHash kh <- khs
    ]

convCertificates
  :: TxCertificates (LedgerEra era)
  -> Seq.StrictSeq (L.TxCert (LedgerEra era))
convCertificates (TxCertificates cs) =
  fromList . map (\(Exp.Certificate c, _) -> c) $ toList cs

convPParamsToScriptIntegrityHash
  :: forall era
   . IsEra era
  => Maybe (Ledger.PParams (LedgerEra era))
  -> L.Redeemers (LedgerEra era)
  -> L.TxDats (LedgerEra era)
  -> Set Plutus.Language
  -> Either MakeUnsignedTxError (StrictMaybe L.ScriptIntegrityHash)
convPParamsToScriptIntegrityHash mTxProtocolParams redeemers datums languages = obtainCommonConstraints (useEra @era) $ do
  -- This logic is copied from ledger, because their code is not reusable
  -- c.f. https://github.com/IntersectMBO/cardano-ledger/commit/5a975d9af507c9ee835a86d3bb77f3e2670ad228#diff-8236dfec9688f22550b91fc9a87af9915523ab9c5bd817218ecceec8ca7a789bR282
  let shouldCalculateHash =
        not $
          null (redeemers ^. L.unRedeemersL)
            && null (datums ^. L.unTxDatsL)
            && null languages
  if shouldCalculateHash
    then do
      pp <- liftMaybe MakeUnsignedTxMissingProtocolParams mTxProtocolParams
      pure $
        SJust $
          L.hashScriptIntegrity $
            L.ScriptIntegrity redeemers datums (Set.map (L.getLanguageView pp) languages)
    else pure SNothing

convProposalProcedures
  :: forall era
   . IsEra era
  => Maybe (TxProposalProcedures (LedgerEra era)) -> OSet (L.ProposalProcedure (LedgerEra era))
convProposalProcedures Nothing = OSet.empty
convProposalProcedures (Just (TxProposalProcedures proposals)) =
  obtainCommonConstraints (useEra @era) $ fromList $ fst <$> toList proposals

convVotingProcedures
  :: Maybe (TxVotingProcedures (LedgerEra era)) -> L.VotingProcedures (LedgerEra era)
convVotingProcedures (Just (TxVotingProcedures vps _)) = vps
convVotingProcedures Nothing = L.VotingProcedures mempty

-- | Auxiliary data consists of the tx metadata
-- and the auxiliary scripts, and the auxiliary script data.
toAuxiliaryData
  :: forall era
   . IsEra era
  => TxMetadata
  -> [SimpleScript (LedgerEra era)]
  -> Maybe (L.TxAuxData (LedgerEra era))
toAuxiliaryData txMData ss' =
  let ms = toShelleyMetadata $ unTxMetadata txMData
   in case useEra @era of
        ConwayEra ->
          let ss = [L.NativeScript s | SimpleScript s <- ss']
           in guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
        DijkstraEra ->
          let ss = [L.NativeScript s | SimpleScript s <- ss']
           in guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss

-- | Set the fields that differ between eras on the body built in 'makeUnsignedTx'.
eraSpecificLedgerTxBody
  :: Era era
  -> L.TxBody L.TopTx (LedgerEra era)
  -> TxBodyContent (LedgerEra era)
  -> Either MakeUnsignedTxError (L.TxBody L.TopTx (LedgerEra era))
eraSpecificLedgerTxBody era ledgerbody bc =
  case era of
    ConwayEra ->
      case NonEmpty.nonEmpty dijkstraOnlyFieldsSet of
        Just fields -> Left $ MakeUnsignedTxFieldsNotSupportedInEra (Some era) fields
        Nothing ->
          Right $
            ledgerbody
              & L.reqSignerHashesTxBodyL .~ reqSignerHashes
    DijkstraEra ->
      -- Dijkstra replaced required signer hashes with guards, so extra key
      -- witnesses become key-hash guards.
      Right $
        ledgerbody
          & L.guardsTxBodyL
            .~ (txGuards bc <> OSet.fromSet (Set.map L.KeyHashObj reqSignerHashes))
          & L.subTransactionsTxBodyL .~ txSubTransactions bc
          & L.requiredTopLevelGuardsL .~ txRequiredTopLevelGuards bc
          & L.directDepositsTxBodyL .~ txDirectDeposits bc
          & L.accountBalanceIntervalsTxBodyL .~ txAccountBalanceIntervals bc
          & L.startingAccountBalanceIntervalsTxBodyL .~ txStartingAccountBalanceIntervals bc
 where
  reqSignerHashes = convExtraKeyWitnesses (txExtraKeyWits bc)

  -- Every field the Dijkstra branch above writes must be empty in Conway.
  dijkstraOnlyFieldsSet :: [Text]
  dijkstraOnlyFieldsSet =
    [ name
    | (name, isPresent) <-
        [ ("txGuards", present (txGuards bc))
        , ("txSubTransactions", present (txSubTransactions bc))
        , ("txRequiredTopLevelGuards", present (txRequiredTopLevelGuards bc))
        , ("txDirectDeposits", present (L.unDirectDeposits (txDirectDeposits bc)))
        ,
          ( "txAccountBalanceIntervals"
          , present (L.unAccountBalanceIntervals (txAccountBalanceIntervals bc))
          )
        ,
          ( "txStartingAccountBalanceIntervals"
          , present (L.unAccountBalanceIntervals (txStartingAccountBalanceIntervals bc))
          )
        ]
    , isPresent
    ]

  present :: Foldable f => f a -> Bool
  present = not . null

data TxOut era where
  TxOut :: L.EraTxOut era => L.TxOut era -> TxOut era

instance ToJSON (TxOut L.ShelleyEra) where toJSON = txOutToJson ShelleyBasedEraShelley

instance ToJSON (TxOut L.AllegraEra) where toJSON = txOutToJson ShelleyBasedEraAllegra

instance ToJSON (TxOut L.MaryEra) where toJSON = txOutToJson ShelleyBasedEraMary

-- | Note: Unlike the legacy API's @TxOut@, this instance does not render
-- supplemental datums. At the ledger level, a supplemental datum is not
-- stored in the @TxOut@ — only its hash is. The full datum lives in the
-- transaction witness set (@TxDats@). The legacy API bundled the full
-- datum into @TxOut@ for convenience, but since this type wraps the
-- ledger's @TxOut@ directly, supplemental datums are indistinguishable
-- from hash-only datums here.
instance ToJSON (TxOut L.AlonzoEra) where toJSON = txOutToJson ShelleyBasedEraAlonzo

instance ToJSON (TxOut L.BabbageEra) where toJSON = txOutToJson ShelleyBasedEraBabbage

instance ToJSON (TxOut L.ConwayEra) where toJSON = txOutToJson ShelleyBasedEraConway

txOutToJson :: ShelleyBasedEra era -> TxOut (ShelleyLedgerEra era) -> Aeson.Value
txOutToJson sbe (TxOut o) =
  Aeson.object $
    txOutBaseJsonFields sbe o <> alonzoOnwardsFields
 where
  alonzoOnwardsFields = case sbe of
    ShelleyBasedEraShelley -> []
    ShelleyBasedEraAllegra -> []
    ShelleyBasedEraMary -> []
    ShelleyBasedEraAlonzo -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)
    ShelleyBasedEraBabbage -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)
    ShelleyBasedEraConway -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)
    ShelleyBasedEraDijkstra -> datumAndRefScriptFields (o ^. L.datumTxOutG) (o ^. L.referenceScriptTxOutG)

-- | Emit the datum, inline-datum, and reference-script JSON fields appropriate
-- for the era. Pre-Alonzo emits nothing; Alonzo emits @datumhash@ and @datum@;
-- Babbage+ additionally emits @inlineDatum@, @inlineDatumRaw@, @inlineDatumhash@
-- and @referenceScript@.
datumAndRefScriptFields
  :: L.AlonzoEraScript era
  => Maybe (L.Datum era)
  -> Maybe (Maybe (L.Script era))
  -> [Pair]
datumAndRefScriptFields mDatum mRefScript =
  datumFields <> inlineDatumFields <> refScriptFields
 where
  isBabbagePlus = isJust mRefScript

  datumFields = case mDatum of
    Nothing -> []
    Just L.NoDatum -> ["datumhash" .= Aeson.Null, "datum" .= Aeson.Null]
    Just (L.DatumHash dh) -> ["datumhash" .= dh, "datum" .= Aeson.Null]
    Just (L.Datum _) -> ["datum" .= Aeson.Null]

  inlineDatumFields = case mDatum of
    Just (L.Datum bd) ->
      let hsd = Api.fromAlonzoData (L.binaryDataToData bd)
       in [ "inlineDatumhash" .= L.hashBinaryData bd
          , "inlineDatum" .= Api.scriptDataToJsonDetailedSchema hsd
          , "inlineDatumRaw"
              .= (Aeson.String . Text.decodeUtf8 . Base16.encode . serialiseToCBOR $ hsd)
          ]
    _
      | isBabbagePlus -> ["inlineDatum" .= Aeson.Null, "inlineDatumRaw" .= Aeson.Null]
      | otherwise -> []

  refScriptFields = case mRefScript of
    Nothing -> []
    Just Nothing -> ["referenceScript" .= Aeson.Null]
    Just (Just script) -> ["referenceScript" .= ledgerScriptToScriptInAnyLang script]

-- | Render just the base fields (address and value) shared by all eras.
txOutBaseJsonFields
  :: L.EraTxOut (ShelleyLedgerEra era) => ShelleyBasedEra era -> L.TxOut (ShelleyLedgerEra era) -> [Pair]
txOutBaseJsonFields sbe o =
  [ "address" .= addrToJson (o ^. L.addrTxOutL)
  , "value" .= fromLedgerValue sbe (o ^. L.valueTxOutL)
  ]

-- | Convert a ledger 'L.Addr' to JSON using the same format as the legacy API
-- (bech32 for Shelley addresses, base58 for Byron addresses).
addrToJson :: L.Addr -> Aeson.Value
addrToJson (L.Addr nw pc scr) = toJSON (ShelleyAddress nw pc scr)
addrToJson (L.AddrBootstrap (L.BootstrapAddress addr)) = toJSON (ByronAddress addr)

-- | Convert a ledger 'Script' to a cardano-api 'ScriptInAnyLang' without
-- per-era pattern matching, using 'AlonzoEraScript' methods.
ledgerScriptToScriptInAnyLang
  :: L.AlonzoEraScript era => L.Script era -> ScriptInAnyLang
ledgerScriptToScriptInAnyLang script =
  case L.getNativeScript script of
    Just ns ->
      ScriptInAnyLang SimpleScriptLanguage (OldScript.SimpleScript (fromAllegraTimelock ns))
    Nothing ->
      case L.toPlutusScript script of
        Just ps -> L.withPlutusScript ps $ \plutus ->
          let sbs = unPlutusBinary (L.plutusBinary plutus)
           in case plutusLanguage plutus of
                Plutus.PlutusV1 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) $
                    OldScript.PlutusScript PlutusScriptV1 (PlutusScriptSerialised sbs)
                Plutus.PlutusV2 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) $
                    OldScript.PlutusScript PlutusScriptV2 (PlutusScriptSerialised sbs)
                Plutus.PlutusV3 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) $
                    OldScript.PlutusScript PlutusScriptV3 (PlutusScriptSerialised sbs)
                Plutus.PlutusV4 ->
                  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV4) $
                    OldScript.PlutusScript PlutusScriptV4 (PlutusScriptSerialised sbs)
        Nothing -> error "ledgerScriptToScriptInAnyLang: script is neither native nor Plutus"

-- | Convert a 'ScriptInAnyLang' to a ledger 'L.Script'. Reverse of 'ledgerScriptToScriptInAnyLang'.
scriptInAnyLangToLedgerScript
  :: forall era
   . ( L.AlonzoEraScript era
     , L.NativeScript era ~ Timelock era
     )
  => ScriptInAnyLang -> Parser (L.Script era)
scriptInAnyLangToLedgerScript (ScriptInAnyLang lang script) =
  case (lang, script) of
    (SimpleScriptLanguage, OldScript.SimpleScript ss) ->
      pure $ Ledger.fromNativeScript (toAllegraTimelock ss)
    (PlutusScriptLanguage PlutusScriptV1, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV1)
    (PlutusScriptLanguage PlutusScriptV2, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV2)
    (PlutusScriptLanguage PlutusScriptV3, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV3)
    (PlutusScriptLanguage PlutusScriptV4, OldScript.PlutusScript _ (PlutusScriptSerialised sbs)) ->
      L.fromPlutusScript
        <$> L.mkPlutusScript (Plutus.Plutus (PlutusBinary sbs) :: Plutus.Plutus 'Plutus.PlutusV4)

deriving instance (Show (TxOut era))

deriving instance (Eq (TxOut era))

-- | Pre-Alonzo eras have no datums or reference scripts, so parsing
instance FromJSON (TxOut L.ShelleyEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraShelley)

instance FromJSON (TxOut L.AllegraEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraAllegra)

instance FromJSON (TxOut L.MaryEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraMary)

instance FromJSON (TxOut L.AlonzoEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraAlonzo)

instance FromJSON (TxOut L.BabbageEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraBabbage)

instance FromJSON (TxOut L.ConwayEra) where
  parseJSON = Aeson.withObject "TxOut" (txOutParseJson ShelleyBasedEraConway)

txOutParseJson
  :: ShelleyBasedEra era -> Aeson.Object -> Parser (TxOut (ShelleyLedgerEra era))
txOutParseJson sbe o = do
  addr <- addrFromJson =<< o .: "address"
  apiVal <- parseJSON =<< o .: "value"
  let mv = toMaryValue apiVal
  case sbe of
    ShelleyBasedEraShelley -> do
      let L.MaryValue _ ma = mv
      unless (ma == mempty) $
        fail "txOutParseJson: ada-only era output cannot carry a multi-asset value"
      pure . TxOut $ L.mkBasicTxOut addr (L.coin mv)
    ShelleyBasedEraAllegra -> do
      let L.MaryValue _ ma = mv
      unless (ma == mempty) $
        fail "txOutParseJson: ada-only era output cannot carry a multi-asset value"
      pure . TxOut $ L.mkBasicTxOut addr (L.coin mv)
    ShelleyBasedEraMary -> pure . TxOut $ L.mkBasicTxOut addr mv
    ShelleyBasedEraAlonzo -> do
      let base = L.mkBasicTxOut addr mv
      mDatumHash <- o .:? "datumhash"
      pure . TxOut $ case mDatumHash of
        Nothing -> base
        Just dh -> base & L.dataHashTxOutL .~ SJust dh
    ShelleyBasedEraBabbage ->
      babbageOnwardsTxOutParseJson (L.mkBasicTxOut addr mv) o
    ShelleyBasedEraConway ->
      babbageOnwardsTxOutParseJson (L.mkBasicTxOut addr mv) o
    ShelleyBasedEraDijkstra -> error "TODO Dijkstra: txOutParseJson: era not supported"

-- | Parse a ledger 'L.Addr' from JSON. Reverse of 'addrToJson'.
addrFromJson :: Aeson.Value -> Parser L.Addr
addrFromJson = Aeson.withText "Address" $ \txt ->
  case deserialiseAddress AsAddressAny txt of
    Nothing -> fail $ "addrFromJson: invalid address: " <> show txt
    Just addrAny -> pure $ case addrAny of
      AddressByron (ByronAddress addr) -> L.AddrBootstrap (L.BootstrapAddress addr)
      AddressShelley (ShelleyAddress nw pc scr) -> L.Addr nw pc scr

-- | Parse a Babbage+ TxOut with datum and reference script support.
babbageOnwardsTxOutParseJson
  :: forall era
   . ( L.BabbageEraTxOut era
     , L.NativeScript era ~ Timelock era
     )
  => L.TxOut era -> Aeson.Object -> Parser (TxOut era)
babbageOnwardsTxOutParseJson baseTxOut o = do
  -- Parse datum fields
  mDatumHash <- o .:? "datumhash"
  mInlineDatumRaw <- o .:? "inlineDatumRaw"
  mInlineDatumHash <- o .:? "inlineDatumhash"
  -- Parse reference script
  mRefScript <- o .:? "referenceScript"
  -- Determine datum
  datum <- case mInlineDatumRaw of
    Just rawHex -> do
      expectedHash <-
        maybe
          (fail "babbageOnwardsTxOutParseJson: inlineDatumRaw present without inlineDatumhash")
          pure
          mInlineDatumHash
      rawBytes <-
        failEitherWith
          (("babbageOnwardsTxOutParseJson: failed to hex-decode inlineDatumRaw: " <>) . show)
          $ Base16.decode (Text.encodeUtf8 rawHex)
      binaryData <-
        failEitherWith
          ("babbageOnwardsTxOutParseJson: failed to CBOR-decode inlineDatumRaw: " <>)
          $ L.makeBinaryData (SBS.toShort rawBytes)
      when (L.hashBinaryData binaryData /= expectedHash) $
        fail $
          mconcat
            [ "babbageOnwardsTxOutParseJson: inline datum hash mismatch: "
            , "expected "
            , show expectedHash
            , ", got "
            , show (L.hashBinaryData binaryData)
            ]
      pure $ L.Datum binaryData
    Nothing -> do
      when (isJust mInlineDatumHash) $
        fail "babbageOnwardsTxOutParseJson: inlineDatumhash present without inlineDatumRaw"
      pure $ maybe L.NoDatum L.DatumHash mDatumHash
  -- Determine reference script
  refScript <- L.maybeToStrictMaybe <$> forM mRefScript scriptInAnyLangToLedgerScript
  -- Construct TxOut
  pure . TxOut $
    baseTxOut
      & L.datumTxOutL .~ datum
      & L.referenceScriptTxOutL .~ refScript

data Datum ctx era where
  TxOutDatumHash
    :: L.DataHash
    -> Datum ctx era
  TxOutSupplementalDatum
    :: L.DataHash
    -> L.Data era
    -> Datum CtxTx era
  TxOutDatumInline
    :: L.DataHash
    -> L.Data era
    -> Datum ctx era

deriving instance (Show (Datum ctx era))

deriving instance (Eq (Datum ctx era))

extractDatumsAndHashes :: Datum ctx era -> Maybe (L.DataHash, L.Data era)
extractDatumsAndHashes TxOutDatumHash{} = Nothing
extractDatumsAndHashes (TxOutSupplementalDatum h d) = Just (h, d)
extractDatumsAndHashes (TxOutDatumInline h d) = Just (h, d)

data TxInsReference era = TxInsReference [TxIn] (Set (Datum CtxTx era))

newtype TxTotalCollateral = TxTotalCollateral {unTxTotalCollateral :: L.Coin}

newtype TxReturnCollateral era = TxReturnCollateral {unTxReturnCollateral :: L.TxOut era}

newtype TxValidityLowerBound = TxValidityLowerBound L.SlotNo

newtype TxExtraKeyWitnesses = TxExtraKeyWitnesses [Hash PaymentKey]

newtype TxWithdrawals era = TxWithdrawals {unTxWithdrawals :: [(StakeAddress, L.Coin, AnyWitness era)]}
  deriving (Eq, Show)

newtype TxCertificates era
  = TxCertificates
  {unTxCertificates :: OMap (Exp.Certificate era) (Maybe (AnyWitness era))}
  deriving (Show, Eq)

-- | Create 'TxCertificates'. Note that 'Certificate era' will be deduplicated. Certificates that
-- require a witness will be stored with 'Just' the caller-supplied witness; those that do not (e.g.
-- deposit-less stake registration in Conway) will be stored with 'Nothing'.
--
-- Note that, when building a transaction in Conway era, a witness is not required for staking credential
-- registration, but this is only the case during the transitional period of Conway era and only for staking
-- credential registration certificates without a deposit. Future eras will require a witness for
-- registration certificates, because the one without a deposit will be removed.
mkTxCertificates
  :: forall era
   . Era era
  -> [(Exp.Certificate (LedgerEra era), AnyWitness (LedgerEra era))]
  -> TxCertificates (LedgerEra era)
mkTxCertificates era certs = TxCertificates . OMap.fromList $ map getStakeCred certs
 where
  getStakeCred
    :: (Exp.Certificate (LedgerEra era), AnyWitness (LedgerEra era))
    -> ( Exp.Certificate (LedgerEra era)
       , Maybe (AnyWitness (LedgerEra era))
       )
  getStakeCred (c@(Exp.Certificate cert), wit) =
    (c, wit <$ getTxCertWitness (convert era) (obtainCommonConstraints era cert))

newtype TxMintValue era
  = TxMintValue
  { unTxMintValue
      :: Map
           PolicyId
           ( PolicyAssets
           , AnyScriptWitness era
           )
  }
  deriving (Eq, Show)

-- | Convert 'TxMintValue' to a more handy 'Value'.
txMintValueToValue :: TxMintValue era -> Value
txMintValueToValue (TxMintValue policiesWithAssets) =
  mconcat
    [ policyAssetsToValue policyId assets
    | (policyId, (assets, _witness)) <- toList policiesWithAssets
    ]

newtype TxProposalProcedures era
  = TxProposalProcedures
      ( OMap
          (L.ProposalProcedure era)
          (AnyWitness era)
      )
  deriving (Show, Eq)

-- | A smart constructor for 'TxProposalProcedures'. It makes sure that the value produced is consistent - the
-- witnessed proposals are also present in the first constructor parameter.
mkTxProposalProcedures
  :: forall era
   . IsEra era
  => [(L.ProposalProcedure (LedgerEra era), AnyWitness (LedgerEra era))]
  -> TxProposalProcedures (LedgerEra era)
mkTxProposalProcedures proposals = do
  TxProposalProcedures $
    obtainCommonConstraints (useEra @era) $
      OMap.fromList proposals

data TxVotingProcedures era
  = TxVotingProcedures
      (L.VotingProcedures era)
      (Map L.Voter (AnyWitness era))
  deriving (Eq, Show)

-- | Create voting procedures from map of voting procedures and optional witnesses.
-- Validates the function argument, to make sure the list of votes is legal.
-- See 'mergeVotingProcedures' for validation rules.
mkTxVotingProcedures
  :: forall era
   . [(L.VotingProcedures era, AnyWitness era)]
  -> Either (VotingError era) (TxVotingProcedures era)
mkTxVotingProcedures votingProcedures = do
  procedure <-
    foldM f (L.VotingProcedures Map.empty) votingProcedures
  votingScriptWitnessMap <-
    foldM
      (\acc next -> Map.union acc <$> uncurry votingScriptWitnessSingleton next)
      Map.empty
      votingProcedures
  pure $ TxVotingProcedures procedure votingScriptWitnessMap
 where
  f
    :: L.VotingProcedures era
    -> (L.VotingProcedures era, AnyWitness era)
    -> Either (VotingError era) (L.VotingProcedures era)
  f acc (procedure, _witness) = mergeVotingProcedures acc procedure

  votingScriptWitnessSingleton
    :: L.VotingProcedures era
    -> AnyWitness era
    -> Either (VotingError era) (Map L.Voter (AnyWitness era))
  votingScriptWitnessSingleton lVotingProcedures scriptWitness =
    case fst <$> Map.lookupMin (L.unVotingProcedures lVotingProcedures) of
      Nothing -> Left $ VotingScriptWitnessWithoutVoter lVotingProcedures
      Just voter -> Right $ Map.singleton voter scriptWitness

-- | Content of a transaction body at either transaction level.
--
-- The transaction level is the ledger's 'L.TxLevel' kind. A 'L.TopTx' body is
-- an ordinary transaction, the thing that is submitted to the chain. A
-- 'L.SubTx' body is a Dijkstra sub-transaction: a transaction built and signed
-- on its own, then embedded whole in a top-level transaction's
-- 'txSubTransactions'. Both levels share most of their fields; a
-- sub-transaction has no fee, collateral, script validity flag, required
-- signers or sub-transactions of its own, since the top level carries those.
--
-- The fields shared by both levels live directly in the record. The fields that
-- exist only in a top-level body live in 'TopTxOnlyFields'. Use the
-- 'TxBodyContent' pattern for a top-level body and the 'SubTxBodyContent'
-- pattern for a sub-transaction body: each presents a flat record, so the
-- nesting is never visible outside this module. A function typed
-- @BodyContent l era@ works on bodies of either level.
data BodyContent (l :: L.TxLevel) era
  = BodyContent
  { bcIns :: [(TxIn, AnyWitness era)]
  , bcInsReference :: TxInsReference era
  , bcOuts :: [TxOut era]
  , bcValidityLowerBound :: Maybe L.SlotNo
  , bcValidityUpperBound :: Maybe L.SlotNo
  , bcMetadata :: TxMetadata
  , bcAuxScripts :: [SimpleScript era]
  , bcProtocolParams :: Maybe (L.PParams era)
  , bcWithdrawals :: TxWithdrawals era
  , bcCertificates :: TxCertificates era
  , bcMintValue :: TxMintValue era
  , bcProposalProcedures :: Maybe (TxProposalProcedures era)
  , bcVotingProcedures :: Maybe (TxVotingProcedures era)
  , bcCurrentTreasuryValue :: Maybe L.Coin
  -- ^ Current treasury value
  , bcTreasuryDonation :: Maybe L.Coin
  -- ^ Treasury donation to perform
  , bcSupplementalDatums :: Map L.DataHash (L.Data era)
  -- ^ Supplemental datums are datums whose hashes correspond to output datum hashes.
  -- They are included in the transaction witness set for communication purposes only.
  -- ------------------------------------------------------------
  -- Fields below are new in the Dijkstra era.
  -- ------------------------------------------------------------
  , bcGuards :: OSet (L.Credential L.Guard)
  , bcRequiredTopLevelGuards :: Map (L.Credential L.Guard) (StrictMaybe (L.Data era))
  , bcDirectDeposits :: L.DirectDeposits
  , bcAccountBalanceIntervals :: L.AccountBalanceIntervals era
  , bcTopTxOnlyFields :: TopTxOnlyFields l era
  }

-- | The fields that exist only in a top-level transaction body. Matching a
-- constructor tells you the level of the enclosing 'BodyContent'.
data TopTxOnlyFields (l :: L.TxLevel) era where
  TopTxFields
    :: { tlInsCollateral :: [TxIn]
       , tlTotalCollateral :: Maybe TxTotalCollateral
       , tlReturnCollateral :: Maybe (TxReturnCollateral era)
       , tlFee :: L.Coin
       , tlExtraKeyWits :: TxExtraKeyWitnesses
       , tlScriptValidity :: ScriptValidity
       , tlSubTransactions :: LOMap.OMap L.TxId (L.Tx L.SubTx era)
       , tlStartingAccountBalanceIntervals :: L.AccountBalanceIntervals era
       }
    -> TopTxOnlyFields L.TopTx era
  -- | These fields do not exist in a sub-transaction body.
  AbsentInSubTx :: TopTxOnlyFields L.SubTx era

-- | Content of a top-level transaction body. See the 'TxBodyContent' pattern.
type TxBodyContent = BodyContent L.TopTx

-- | Content of a Dijkstra sub-transaction body. See the 'SubTxBodyContent' pattern.
--
-- Compared to a top-level 'TxBodyContent', a sub-transaction body has no
-- collateral inputs, no total or return collateral, no fee, no extra key
-- witnesses (guards replace them), no script validity flag, no
-- sub-transactions of its own and no starting account balance intervals.
type SubTxBodyContent = BodyContent L.SubTx

-- | A top-level transaction body as one flat record.
pattern TxBodyContent
  :: [(TxIn, AnyWitness era)]
  -> [TxIn]
  -> TxInsReference era
  -> [TxOut era]
  -> Maybe TxTotalCollateral
  -> Maybe (TxReturnCollateral era)
  -> L.Coin
  -> Maybe L.SlotNo
  -> Maybe L.SlotNo
  -> TxMetadata
  -> [SimpleScript era]
  -> TxExtraKeyWitnesses
  -> Maybe (L.PParams era)
  -> TxWithdrawals era
  -> TxCertificates era
  -> TxMintValue era
  -> ScriptValidity
  -> Maybe (TxProposalProcedures era)
  -> Maybe (TxVotingProcedures era)
  -> Maybe L.Coin
  -> Maybe L.Coin
  -> Map L.DataHash (L.Data era)
  -> OSet (L.Credential L.Guard)
  -> LOMap.OMap L.TxId (L.Tx L.SubTx era)
  -> Map (L.Credential L.Guard) (StrictMaybe (L.Data era))
  -> L.DirectDeposits
  -> L.AccountBalanceIntervals era
  -> L.AccountBalanceIntervals era
  -> TxBodyContent era
pattern TxBodyContent
  { txIns
  , txInsCollateral
  , txInsReference
  , txOuts
  , txTotalCollateral
  , txReturnCollateral
  , txFee
  , txValidityLowerBound
  , txValidityUpperBound
  , txMetadata
  , txAuxScripts
  , txExtraKeyWits
  , txProtocolParams
  , txWithdrawals
  , txCertificates
  , txMintValue
  , txScriptValidity
  , txProposalProcedures
  , txVotingProcedures
  , txCurrentTreasuryValue
  , txTreasuryDonation
  , txSupplementalDatums
  , txGuards
  , txSubTransactions
  , txRequiredTopLevelGuards
  , txDirectDeposits
  , txAccountBalanceIntervals
  , txStartingAccountBalanceIntervals
  } =
  BodyContent
    { bcIns = txIns
    , bcInsReference = txInsReference
    , bcOuts = txOuts
    , bcValidityLowerBound = txValidityLowerBound
    , bcValidityUpperBound = txValidityUpperBound
    , bcMetadata = txMetadata
    , bcAuxScripts = txAuxScripts
    , bcProtocolParams = txProtocolParams
    , bcWithdrawals = txWithdrawals
    , bcCertificates = txCertificates
    , bcMintValue = txMintValue
    , bcProposalProcedures = txProposalProcedures
    , bcVotingProcedures = txVotingProcedures
    , bcCurrentTreasuryValue = txCurrentTreasuryValue
    , bcTreasuryDonation = txTreasuryDonation
    , bcSupplementalDatums = txSupplementalDatums
    , bcGuards = txGuards
    , bcRequiredTopLevelGuards = txRequiredTopLevelGuards
    , bcDirectDeposits = txDirectDeposits
    , bcAccountBalanceIntervals = txAccountBalanceIntervals
    , bcTopTxOnlyFields =
      TopTxFields
        { tlInsCollateral = txInsCollateral
        , tlTotalCollateral = txTotalCollateral
        , tlReturnCollateral = txReturnCollateral
        , tlFee = txFee
        , tlExtraKeyWits = txExtraKeyWits
        , tlScriptValidity = txScriptValidity
        , tlSubTransactions = txSubTransactions
        , tlStartingAccountBalanceIntervals = txStartingAccountBalanceIntervals
        }
    }

{-# COMPLETE TxBodyContent #-}

-- | A sub-transaction body as one flat record.
pattern SubTxBodyContent
  :: [(TxIn, AnyWitness era)]
  -> TxInsReference era
  -> [TxOut era]
  -> Maybe L.SlotNo
  -> Maybe L.SlotNo
  -> TxMetadata
  -> [SimpleScript era]
  -> Maybe (L.PParams era)
  -> TxWithdrawals era
  -> TxCertificates era
  -> TxMintValue era
  -> Maybe (TxProposalProcedures era)
  -> Maybe (TxVotingProcedures era)
  -> Maybe L.Coin
  -> Maybe L.Coin
  -> Map L.DataHash (L.Data era)
  -> OSet (L.Credential L.Guard)
  -> Map (L.Credential L.Guard) (StrictMaybe (L.Data era))
  -> L.DirectDeposits
  -> L.AccountBalanceIntervals era
  -> SubTxBodyContent era
pattern SubTxBodyContent
  { subTxIns
  , subTxInsReference
  , subTxOuts
  , subTxValidityLowerBound
  , subTxValidityUpperBound
  , subTxMetadata
  , subTxAuxScripts
  , subTxProtocolParams
  , subTxWithdrawals
  , subTxCertificates
  , subTxMintValue
  , subTxProposalProcedures
  , subTxVotingProcedures
  , subTxCurrentTreasuryValue
  , subTxTreasuryDonation
  , subTxSupplementalDatums
  , subTxGuards
  , subTxRequiredTopLevelGuards
  , subTxDirectDeposits
  , subTxAccountBalanceIntervals
  } =
  BodyContent
    { bcIns = subTxIns
    , bcInsReference = subTxInsReference
    , bcOuts = subTxOuts
    , bcValidityLowerBound = subTxValidityLowerBound
    , bcValidityUpperBound = subTxValidityUpperBound
    , bcMetadata = subTxMetadata
    , bcAuxScripts = subTxAuxScripts
    , bcProtocolParams = subTxProtocolParams
    , bcWithdrawals = subTxWithdrawals
    , bcCertificates = subTxCertificates
    , bcMintValue = subTxMintValue
    , bcProposalProcedures = subTxProposalProcedures
    , bcVotingProcedures = subTxVotingProcedures
    , bcCurrentTreasuryValue = subTxCurrentTreasuryValue
    , bcTreasuryDonation = subTxTreasuryDonation
    , bcSupplementalDatums = subTxSupplementalDatums
    , bcGuards = subTxGuards
    , bcRequiredTopLevelGuards = subTxRequiredTopLevelGuards
    , bcDirectDeposits = subTxDirectDeposits
    , bcAccountBalanceIntervals = subTxAccountBalanceIntervals
    , bcTopTxOnlyFields = AbsentInSubTx
    }

{-# COMPLETE SubTxBodyContent #-}

defaultTxBodyContent :: TxBodyContent era
defaultTxBodyContent =
  defaultBodyContent
    TopTxFields
      { tlInsCollateral = []
      , tlTotalCollateral = Nothing
      , tlReturnCollateral = Nothing
      , tlFee = 0
      , tlExtraKeyWits = TxExtraKeyWitnesses []
      , tlScriptValidity = ScriptValid
      , tlSubTransactions = LOMap.empty
      , tlStartingAccountBalanceIntervals = L.AccountBalanceIntervals mempty
      }

defaultSubTxBodyContent :: SubTxBodyContent era
defaultSubTxBodyContent = defaultBodyContent AbsentInSubTx

-- | Empty shared fields around the given top-level-only fields.
defaultBodyContent :: TopTxOnlyFields l era -> BodyContent l era
defaultBodyContent topTxOnlyFields =
  BodyContent
    { bcIns = []
    , bcInsReference = TxInsReference mempty Set.empty
    , bcOuts = []
    , bcValidityLowerBound = Nothing
    , bcValidityUpperBound = Nothing
    , bcMetadata = TxMetadata mempty
    , bcAuxScripts = []
    , bcProtocolParams = Nothing
    , bcWithdrawals = TxWithdrawals mempty
    , bcCertificates = TxCertificates OMap.empty
    , bcMintValue = TxMintValue mempty
    , bcProposalProcedures = Nothing
    , bcVotingProcedures = Nothing
    , bcCurrentTreasuryValue = Nothing
    , bcTreasuryDonation = Nothing
    , bcSupplementalDatums = mempty
    , bcGuards = OSet.empty
    , bcRequiredTopLevelGuards = mempty
    , bcDirectDeposits = L.DirectDeposits mempty
    , bcAccountBalanceIntervals = L.AccountBalanceIntervals mempty
    , bcTopTxOnlyFields = topTxOnlyFields
    }

extractAllIndexedPlutusScriptWitnesses
  :: forall era
   . Era era
  -> TxBodyContent (LedgerEra era)
  -> Either
       CBOR.DecoderError
       [AnyIndexedPlutusScriptWitness (LedgerEra era)]
extractAllIndexedPlutusScriptWitnesses era b = obtainCommonConstraints era $ do
  let txInWits = extractWitnessableTxIns $ txIns b
      certWits = extractWitnessableCertificates $ txCertificates b
      mintWits = [(wit, anyScriptWitnessToAnyWitness sw) | (wit, sw) <- extractWitnessableMints $ txMintValue b]
      withdrawalWits = extractWitnessableWithdrawals $ txWithdrawals b
      proposalScriptWits = extractWitnessableProposals $ txProposalProcedures b
      voteWits = extractWitnessableVotes $ txVotingProcedures b

  let indexedScriptTxInWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses txInWits
      indexedCertScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses certWits
      indexedMintScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses mintWits
      indexedWithdrawalScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses withdrawalWits
      indexedProposalScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses proposalScriptWits
      indexedVoteScriptWits = obtainCommonConstraints era $ createIndexedPlutusScriptWitnesses voteWits
  return $
    mconcat
      [ indexedScriptTxInWits
      , indexedMintScriptWits
      , indexedCertScriptWits
      , indexedWithdrawalScriptWits
      , indexedProposalScriptWits
      , indexedVoteScriptWits
      ]

extractWitnessableTxIns
  :: forall era
   . IsEra era
  => [(TxIn, AnyWitness (LedgerEra era))]
  -> [(Witnessable TxInItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableTxIns tIns =
  obtainCommonConstraints (useEra @era) $
    List.nub [(WitTxIn txin, wit) | (txin, wit) <- tIns]

-- | Wrap every certificate as a 'Witnessable', paired with its witness.
--
-- An unwitnessed certificate still occupies a redeemer index slot: the
-- ledger indexes the 'Certifying' purpose by position in the full
-- certificate sequence, not just the witnessed subset, so the result below
-- keeps one entry per certificate in insertion order.
--
-- In the Conway era only, a certificate may legitimately have no witness
-- (deposit-less stake registration), so a missing witness defaults to
-- 'AnyKeyWitnessPlaceholder'. From Dijkstra onwards 'mkTxCertificates'
-- guarantees every entry has a 'Just' witness, so the placeholder is dead
-- code for those eras.
extractWitnessableCertificates
  :: forall era
   . IsEra era
  => TxCertificates (LedgerEra era)
  -> [(Witnessable CertItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableCertificates (TxCertificates certs) =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ (WitTxCert cert, wit)
      | (Exp.Certificate cert, mWit) <- toList certs
      , let wit = fromMaybe AnyKeyWitnessPlaceholder mWit
      ]

extractWitnessableMints
  :: forall era
   . IsEra era
  => TxMintValue (LedgerEra era)
  -> [(Witnessable MintItem (LedgerEra era), AnyScriptWitness (LedgerEra era))]
extractWitnessableMints mVal =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ (WitMint policyId policyAssets, wit)
      | (policyId, (policyAssets, wit)) <- getMints mVal
      ]
 where
  getMints (TxMintValue txms) = toList txms

extractWitnessableWithdrawals
  :: forall era
   . IsEra era
  => TxWithdrawals (LedgerEra era)
  -> [(Witnessable WithdrawalItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableWithdrawals txWithDrawals =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ (WitWithdrawal addr withAmt, wit)
      | (addr, withAmt, wit) <- getWithdrawals txWithDrawals
      ]
 where
  getWithdrawals (TxWithdrawals txws) = txws

extractWitnessableVotes
  :: forall era
   . IsEra era
  => Maybe (TxVotingProcedures (LedgerEra era))
  -> [(Witnessable VoterItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableVotes Nothing = []
extractWitnessableVotes (Just txVoteProc) =
  obtainCommonConstraints (useEra @era) $
    List.nub
      [ (WitVote vote, wit)
      | (vote, wit) <- getVotes txVoteProc
      ]
 where
  -- Uses a total 'Map.findWithDefault' (placeholder witness on a miss),
  -- not a lookup that skips missing voters. A skipped voter would shrink
  -- this list and shift every later voter's redeemer index.
  --
  -- 'mkTxVotingProcedures' builds 'scriptWitnessedVotes' in lockstep with
  -- 'allVotingProcedures', assuming exactly one voter per merged
  -- 'L.VotingProcedures' value, so a miss should not normally happen.
  getVotes
    :: TxVotingProcedures (LedgerEra era)
    -> [(L.Voter, AnyWitness (LedgerEra era))]
  getVotes (TxVotingProcedures allVotingProcedures scriptWitnessedVotes) =
    [ (voter, wit)
    | (voter, _) <- toList $ L.unVotingProcedures allVotingProcedures
    , let wit = Map.findWithDefault AnyKeyWitnessPlaceholder voter scriptWitnessedVotes
    ]

extractWitnessableProposals
  :: forall era
   . IsEra era
  => Maybe
       (TxProposalProcedures (LedgerEra era))
  -> [(Witnessable ProposalItem (LedgerEra era), AnyWitness (LedgerEra era))]
extractWitnessableProposals Nothing = []
extractWitnessableProposals (Just txPropProcedures) =
  List.nub
    [ (obtainCommonConstraints (useEra @era) (WitProposal prop), wit)
    | (prop, wit) <-
        getProposals txPropProcedures
    ]
 where
  getProposals
    :: TxProposalProcedures (LedgerEra era)
    -> [(L.ProposalProcedure (LedgerEra era), AnyWitness (LedgerEra era))]
  getProposals (TxProposalProcedures txps) =
    obtainCommonConstraints (useEra @era) (toList txps)

-- | Collect the script witness requirements of a body at either level. Every
-- field involved is shared between top-level and sub-transaction bodies.
collectTxBodyScriptWitnessRequirements
  :: forall l era
   . IsEra era
  => BodyContent l (LedgerEra era)
  -> TxScriptWitnessRequirements (LedgerEra era)
collectTxBodyScriptWitnessRequirements
  BodyContent
    { bcIns
    , bcInsReference
    , bcCertificates
    , bcMintValue
    , bcWithdrawals
    , bcVotingProcedures
    , bcProposalProcedures
    , bcSupplementalDatums
    } = obtainCommonConstraints (useEra @era) $ do
    let supplementaldatums =
          TxScriptWitnessRequirements
            mempty
            mempty
            (getDatums bcInsReference bcSupplementalDatums)
            mempty

    let txInWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableTxIns bcIns
        txWithdrawalWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableWithdrawals bcWithdrawals
        txCertWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableCertificates bcCertificates
        txMintWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            [(wit, anyScriptWitnessToAnyWitness sw) | (wit, sw) <- extractWitnessableMints bcMintValue]
        txVotingWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableVotes bcVotingProcedures
        txProposalWits =
          obtainMonoidConstraint (useEra @era) getTxScriptWitnessesRequirements $
            extractWitnessableProposals bcProposalProcedures

    obtainMonoidConstraint (useEra @era) $
      mconcat
        [ supplementaldatums
        , txInWits
        , txWithdrawalWits
        , txCertWits
        , txMintWits
        , txVotingWits
        , txProposalWits
        ]

obtainMonoidConstraint
  :: Era era
  -> (Monoid (TxScriptWitnessRequirements (LedgerEra era)) => a)
  -> a
obtainMonoidConstraint eon = case eon of
  ConwayEra -> id
  DijkstraEra -> id

-- | Collect datums for the transaction witness set ('TxDats'):
-- 1. supplemental datums provided explicitly
-- 2. datums from reference inputs
--
-- Supplemental datums are datums whose hashes correspond to datum hashes
-- in transaction outputs. They are included for communication purposes only
-- (the Alonzo ledger spec uses subset equality for these).
--
-- Note that this function does not check whose datum hashes are present in the reference inputs. This means if there
-- are redundant datums in 'TxInsReference', a submission of such transaction will fail.
getDatums
  :: forall era
   . IsEra era
  => TxInsReference (LedgerEra era)
  -- ^ reference inputs
  -> Map L.DataHash (L.Data (LedgerEra era))
  -- ^ supplemental datums
  -> L.TxDats (LedgerEra era)
getDatums txInsRef supplementalDats = do
  let TxInsReference _ datumSet = txInsRef
      refInDatums = mapMaybe extractDatumsAndHashes $ Set.toList datumSet
  obtainCommonConstraints (useEra @era) $
    L.TxDats $
      fromList refInDatums <> supplementalDats

-- Getters and Setters

-- Shared by both levels. These work on a 'TxBodyContent' and on a
-- 'SubTxBodyContent' alike.

setTxAuxScripts :: [SimpleScript era] -> BodyContent l era -> BodyContent l era
setTxAuxScripts v bc = bc{bcAuxScripts = v}

setTxIns :: [(TxIn, AnyWitness era)] -> BodyContent l era -> BodyContent l era
setTxIns v bc = bc{bcIns = v}

setTxInsReference :: TxInsReference era -> BodyContent l era -> BodyContent l era
setTxInsReference v bc = bc{bcInsReference = v}

setTxProtocolParams :: L.PParams era -> BodyContent l era -> BodyContent l era
setTxProtocolParams v bc = bc{bcProtocolParams = Just v}

setTxValidityLowerBound :: L.SlotNo -> BodyContent l era -> BodyContent l era
setTxValidityLowerBound v bc = bc{bcValidityLowerBound = Just v}

setTxValidityUpperBound :: L.SlotNo -> BodyContent l era -> BodyContent l era
setTxValidityUpperBound v bc = bc{bcValidityUpperBound = Just v}

setTxMetadata :: TxMetadata -> BodyContent l era -> BodyContent l era
setTxMetadata v bc = bc{bcMetadata = v}

setTxOuts :: [TxOut era] -> BodyContent l era -> BodyContent l era
setTxOuts v bc = bc{bcOuts = v}

modTxOuts :: ([TxOut era] -> [TxOut era]) -> BodyContent l era -> BodyContent l era
modTxOuts f bc = bc{bcOuts = f (bcOuts bc)}

setTxMintValue :: TxMintValue era -> BodyContent l era -> BodyContent l era
setTxMintValue v bc = bc{bcMintValue = v}

setTxCertificates :: TxCertificates era -> BodyContent l era -> BodyContent l era
setTxCertificates v bc = bc{bcCertificates = v}

setTxWithdrawals :: TxWithdrawals era -> BodyContent l era -> BodyContent l era
setTxWithdrawals v bc = bc{bcWithdrawals = v}

setTxVotingProcedures :: TxVotingProcedures era -> BodyContent l era -> BodyContent l era
setTxVotingProcedures v bc = bc{bcVotingProcedures = Just v}

setTxProposalProcedures :: TxProposalProcedures era -> BodyContent l era -> BodyContent l era
setTxProposalProcedures v bc = bc{bcProposalProcedures = Just v}

setTxCurrentTreasuryValue :: L.Coin -> BodyContent l era -> BodyContent l era
setTxCurrentTreasuryValue v bc = bc{bcCurrentTreasuryValue = Just v}

setTxTreasuryDonation :: L.Coin -> BodyContent l era -> BodyContent l era
setTxTreasuryDonation v bc = bc{bcTreasuryDonation = Just v}

setTxSupplementalDatums :: Map L.DataHash (L.Data era) -> BodyContent l era -> BodyContent l era
setTxSupplementalDatums v bc = bc{bcSupplementalDatums = v}

setTxGuards :: OSet (L.Credential L.Guard) -> BodyContent l era -> BodyContent l era
setTxGuards v bc = bc{bcGuards = v}

setTxRequiredTopLevelGuards
  :: Map (L.Credential L.Guard) (StrictMaybe (L.Data era)) -> BodyContent l era -> BodyContent l era
setTxRequiredTopLevelGuards v bc = bc{bcRequiredTopLevelGuards = v}

setTxDirectDeposits :: L.DirectDeposits -> BodyContent l era -> BodyContent l era
setTxDirectDeposits v bc = bc{bcDirectDeposits = v}

setTxAccountBalanceIntervals
  :: L.AccountBalanceIntervals era -> BodyContent l era -> BodyContent l era
setTxAccountBalanceIntervals v bc = bc{bcAccountBalanceIntervals = v}

-- Top-level bodies only. A sub-transaction body has none of these fields.

setTxExtraKeyWits :: TxExtraKeyWitnesses -> TxBodyContent era -> TxBodyContent era
setTxExtraKeyWits v txBodyContent = txBodyContent{txExtraKeyWits = v}

setTxInsCollateral :: [TxIn] -> TxBodyContent era -> TxBodyContent era
setTxInsCollateral v txBodyContent = txBodyContent{txInsCollateral = v}

setTxReturnCollateral :: TxReturnCollateral era -> TxBodyContent era -> TxBodyContent era
setTxReturnCollateral v txBodyContent = txBodyContent{txReturnCollateral = Just v}

setTxTotalCollateral :: TxTotalCollateral -> TxBodyContent era -> TxBodyContent era
setTxTotalCollateral v txBodyContent = txBodyContent{txTotalCollateral = Just v}

setTxFee :: L.Coin -> TxBodyContent era -> TxBodyContent era
setTxFee v txBodyContent = txBodyContent{txFee = v}

setTxScriptValidity :: ScriptValidity -> TxBodyContent era -> TxBodyContent era
setTxScriptValidity v txBodyContent = txBodyContent{txScriptValidity = v}

-- | Sub-transactions are keyed by their transaction id, which is derived from
-- each sub-transaction here so callers never compute it by hand.
setTxSubTransactions :: L.EraTx era => [L.Tx L.SubTx era] -> TxBodyContent era -> TxBodyContent era
setTxSubTransactions v txBodyContent = txBodyContent{txSubTransactions = LOMap.fromFoldable v}

setTxStartingAccountBalanceIntervals
  :: L.AccountBalanceIntervals era -> TxBodyContent era -> TxBodyContent era
setTxStartingAccountBalanceIntervals v txBodyContent =
  txBodyContent{txStartingAccountBalanceIntervals = v}
