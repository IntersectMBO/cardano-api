{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Tx
  ( txToUtxoRpcTx
  , anyEraTxConstraints
  , txInToUtxoRpcTxInput
  , metadatumToUtxoRpcMetadatum
  , txAuxDataScripts
  )
where

import Cardano.Api.Address
import Cardano.Api.Block
import Cardano.Api.Era
import Cardano.Api.Experimental
  ( PlutusScriptPurpose (..)
  , toPlutusScriptPurposeIndex
  )
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Certificate (txCertToUtxoRpcCertificate)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Governance (proposalProcedureToUtxoRpcProposal)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.PlutusData (scriptDataToUtxoRpcPlutusData)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Script (ledgerScriptToUtxoRpcScript)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.TxEval (mkProtoRedeemer)
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.TxOutput
  ( policyAssetsToUtxoRpcMultiassets
  , txOutToUtxoRpcTxOutput
  )

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Keys.Bootstrap qualified as L

import RIO hiding (toList)

import Data.Map.Strict qualified as M
import Data.ProtoLens (defMessage)
import GHC.IsList
import Network.GRPC.Spec

-- | Convert a ledger transaction to the UTxO RPC 'UtxoRpc.Tx' message.
-- Populates hash, fee, successful, inputs, outputs, reference inputs, validity,
-- mint, withdrawals, collateral, certificates, witnesses, auxiliary data and
-- governance proposals, with spending, withdrawal and certificate redeemers
-- wired to their respective entries. Era-gated fields are read through the
-- any-era getters, whose 'Nothing' maps to the proto default; the
-- 'ShelleyBasedEra' witness is recovered from 'IsShelleyBasedEra' and brings
-- the any-era reading classes into scope.
txToUtxoRpcTx
  :: forall era
   . IsShelleyBasedEra era
  => L.Tx L.TopTx (ShelleyLedgerEra era)
  -> Proto UtxoRpc.Tx
txToUtxoRpcTx ledgerTx = anyEraTxConstraints sbe $ do
  let convertTxOut = txOutToUtxoRpcTxOutput sbe . fromShelleyTxOut sbe
      body = ledgerTx ^. L.bodyTxL
      wits = ledgerTx ^. L.witsTxL
      -- reference inputs exist from Babbage onwards
      referenceInputs = maybe [] toList $ body ^. L.referenceInputsTxBodyG
      -- Shelley's plain TTL arrives as an interval without a lower bound
      validity = do
        let L.ValidityInterval lowerBound upperBound = body ^. L.vldtTxBodyG
        defMessage
          & U5c.start .~ L.strictMaybe 0 unSlotNo lowerBound
          & U5c.ttl .~ L.strictMaybe 0 unSlotNo upperBound
      -- minting exists from Mary onwards
      mint = maybe mempty multiAssetToPolicyAssets $ body ^. L.mintTxBodyG
      -- the spending redeemer at index ix belongs to the input at position
      -- ix in the sorted input set
      inputs :: [Proto UtxoRpc.TxInput]
      inputs =
        zip [0 ..] (toList (body ^. L.inputsTxBodyL)) <&> \(ix, txIn) ->
          txInToUtxoRpcTxInput (fromShelleyTxIn txIn)
            & U5c.maybe'redeemer .~ M.lookup (SpendingScript, ix) redeemersByIndex
      -- the withdrawal redeemer at index ix belongs to the withdrawal at
      -- position ix in the sorted withdrawal map
      withdrawals :: [Proto UtxoRpc.Withdrawal]
      withdrawals =
        zip [0 ..] (M.toList (L.unWithdrawals (body ^. L.withdrawalsTxBodyL)))
          <&> \(ix, (rewardAccount, coin)) ->
            defMessage
              & U5c.rewardAccount .~ serialiseToRawBytes (fromShelleyStakeAddr rewardAccount)
              & U5c.coin .~ inject coin
              & U5c.maybe'redeemer
                .~ M.lookup (WithdrawingScript, ix) redeemersByIndex
      -- collateral inputs exist from Alonzo onwards
      collateralInputs = maybe [] toList $ body ^. L.collateralInputsTxBodyG
      -- collateral return and total collateral exist from Babbage onwards;
      -- the outer 'Maybe' is era support, the inner one field presence
      collateralReturn :: Maybe (L.TxOut (ShelleyLedgerEra era))
      collateralReturn = join $ body ^. L.collateralReturnTxBodyG
      totalCollateral :: Maybe L.Coin
      totalCollateral = join $ body ^. L.totalCollateralTxBodyG
      collateral
        | null collateralInputs
        , Nothing <- collateralReturn
        , Nothing <- totalCollateral =
            Nothing
        | otherwise =
            Just $
              defMessage
                & U5c.collateral .~ map (txInToUtxoRpcTxInput . fromShelleyTxIn) collateralInputs
                & U5c.maybe'collateralReturn .~ fmap convertTxOut collateralReturn
                & U5c.maybe'totalCollateral .~ fmap inject totalCollateral
      -- a transaction marked invalid during phase-2 validation consumes its
      -- collateral instead of producing its outputs; before Alonzo every
      -- transaction is valid
      L.IsValid isValid = fromMaybe (L.IsValid True) $ ledgerTx ^. L.isValidTxG
      vkeyWitnesses :: [Proto UtxoRpc.VKeyWitness]
      vkeyWitnesses =
        toList (wits ^. L.addrTxWitsL) <&> \(L.WitVKey (L.VKey vkey) (DSIGN.SignedDSIGN signature)) ->
          defMessage
            & U5c.vkey .~ DSIGN.rawSerialiseVerKeyDSIGN vkey
            & U5c.signature .~ DSIGN.rawSerialiseSigDSIGN signature
      bootstrapWitnesses :: [Proto UtxoRpc.BootstrapWitness]
      bootstrapWitnesses =
        toList (wits ^. L.bootAddrTxWitsL) <&> \bootstrapWitness -> do
          let L.VKey bootstrapKey = L.bwKey bootstrapWitness
              DSIGN.SignedDSIGN bootstrapSignature = L.bwSignature bootstrapWitness
          defMessage
            & U5c.vkey .~ DSIGN.rawSerialiseVerKeyDSIGN bootstrapKey
            & U5c.signature .~ DSIGN.rawSerialiseSigDSIGN bootstrapSignature
            & U5c.chainCode .~ L.unChainCode (L.bwChainCode bootstrapWitness)
            & U5c.attributes .~ L.bwAttributes bootstrapWitness
      scriptWitnesses :: [Proto UtxoRpc.Script]
      scriptWitnesses =
        M.elems (wits ^. L.scriptTxWitsL) <&> ledgerScriptToUtxoRpcScript sbe
      -- plutus datums exist from Alonzo onwards
      plutusDatums :: [Proto UtxoRpc.PlutusData]
      plutusDatums =
        maybe [] (M.elems . L.unTxDats) (wits ^. L.datsTxWitsG) <&> \datum ->
          scriptDataToUtxoRpcPlutusData . getScriptData $ fromAlonzoData datum
      -- redeemers exist from Alonzo onwards; they are keyed by plutus
      -- script purpose and index so that spending, withdrawal and
      -- certificate redeemers can be attached to their inputs, withdrawals
      -- and certificates. Before Alonzo the redeemer getter yields nothing,
      -- so the classifier is never consulted there
      redeemersByIndex :: Map (PlutusScriptPurpose, Word32) (Proto UtxoRpc.Redeemer)
      redeemersByIndex =
        fromList
          [ ( scriptPurposeIndex
            , mkProtoRedeemer
                scriptPurposeIndex
                (fromAlonzoExUnits exUnits)
                (Just (getScriptData (fromAlonzoData datum), L.originalBytes datum))
            )
          | (purpose, (datum, exUnits)) <-
              maybe [] (M.toList . L.unRedeemers) $ wits ^. L.rdmrsTxWitsG
          , let scriptPurposeIndex = toPlutusScriptPurposeIndex sbe purpose
          ]
      witnessSet :: Proto UtxoRpc.WitnessSet
      witnessSet =
        defMessage
          & U5c.vkeywitness .~ vkeyWitnesses
          & U5c.script .~ scriptWitnesses
          & U5c.plutusDatums .~ plutusDatums
          & U5c.redeemers .~ M.elems redeemersByIndex
          & U5c.bootstrapWitnesses .~ bootstrapWitnesses
      -- the certificate redeemer at index ix belongs to the certificate at
      -- position ix in the certificate list
      certificates :: [Proto UtxoRpc.Certificate]
      certificates =
        zip [0 ..] (toList (body ^. L.certsTxBodyL)) <&> \(ix, cert) ->
          txCertToUtxoRpcCertificate sbe cert
            & U5c.maybe'redeemer
              .~ M.lookup (CertifyingScript, ix) redeemersByIndex
      -- auxiliary data is only set when the transaction carries any; the
      -- metadata map is uniformly accessible, the auxiliary scripts vary
      -- per era family
      auxiliary :: Maybe (Proto UtxoRpc.AuxData)
      auxiliary =
        L.strictMaybeToMaybe (ledgerTx ^. L.auxDataTxL) <&> \auxData ->
          defMessage
            & U5c.metadata
              .~ ( M.toList (auxData ^. L.metadataTxAuxDataL) <&> \(metadataLabel, metadatum) ->
                     defMessage
                       & U5c.label .~ metadataLabel
                       & U5c.value .~ metadatumToUtxoRpcMetadatum metadatum
                 )
            & U5c.scripts
              .~ (txAuxDataScripts sbe auxData <&> ledgerScriptToUtxoRpcScript sbe)
      -- governance proposals exist from Conway onwards; converting them
      -- needs 'L.ConwayEraPParams' for the parameter update rendering,
      -- which resolves at the concrete eras
      proposals :: [Proto UtxoRpc.GovernanceActionProposal]
      proposals = case sbe of
        ShelleyBasedEraShelley -> []
        ShelleyBasedEraAllegra -> []
        ShelleyBasedEraMary -> []
        ShelleyBasedEraAlonzo -> []
        ShelleyBasedEraBabbage -> []
        ShelleyBasedEraConway -> conwayOnwardsProposals
        ShelleyBasedEraDijkstra -> conwayOnwardsProposals
       where
        conwayOnwardsProposals
          :: L.ConwayEraPParams (ShelleyLedgerEra era)
          => [Proto UtxoRpc.GovernanceActionProposal]
        conwayOnwardsProposals =
          maybe [] (map proposalProcedureToUtxoRpcProposal . toList) $
            body ^. L.proposalProceduresTxBodyG
  defMessage
    & U5c.hash .~ serialiseToRawBytes (fromShelleyTxId (L.txIdTx ledgerTx))
    & U5c.inputs .~ inputs
    & U5c.outputs .~ map convertTxOut (toList (body ^. L.outputsTxBodyL))
    & U5c.referenceInputs .~ map (txInToUtxoRpcTxInput . fromShelleyTxIn) referenceInputs
    & U5c.certificates .~ certificates
    & U5c.validity .~ validity
    & U5c.mint .~ policyAssetsToUtxoRpcMultiassets mint
    & U5c.withdrawals .~ withdrawals
    & U5c.maybe'collateral .~ collateral
    & U5c.witnesses .~ witnessSet
    & U5c.fee .~ inject (body ^. L.feeTxBodyL)
    & U5c.successful .~ isValid
    & U5c.maybe'auxiliary .~ auxiliary
    & U5c.proposals .~ proposals
 where
  sbe = shelleyBasedEra @era

-- | Bring the constraints of 'txToUtxoRpcTx' into scope for a Shelley-based
-- era: the uniform any-era transaction reading classes of cardano-ledger-api,
-- whose era-gated getters return 'Nothing' where the era predates a field,
-- and 'IsShelleyBasedEra' to recover the witness. Unlike the eon constraint
-- bundles this dispatch is total, including Dijkstra.
anyEraTxConstraints
  :: ShelleyBasedEra era
  -> ((IsShelleyBasedEra era, L.AnyEraTx (ShelleyLedgerEra era)) => a)
  -> a
anyEraTxConstraints sbe k = case sbe of
  ShelleyBasedEraShelley -> k
  ShelleyBasedEraAllegra -> k
  ShelleyBasedEraMary -> k
  ShelleyBasedEraAlonzo -> k
  ShelleyBasedEraBabbage -> k
  ShelleyBasedEraConway -> k
  ShelleyBasedEraDijkstra -> k

-- | Convert a 'TxIn' to the UTxO RPC 'UtxoRpc.TxInput' message.
-- Only the transaction hash and output index are populated:
-- resolving @as_output@ requires a UTxO lookup, and the caller attaches the
-- spending @redeemer@ where one exists.
txInToUtxoRpcTxInput :: TxIn -> Proto UtxoRpc.TxInput
txInToUtxoRpcTxInput (TxIn txId' (TxIx txIx)) =
  defMessage
    & U5c.txHash .~ serialiseToRawBytes txId'
    & U5c.outputIndex .~ fromIntegral txIx

-- | Convert a ledger metadatum to the UTxO RPC 'UtxoRpc.Metadatum' message.
-- The UTxO RPC encoding only supports signed 64-bit metadata integers, while
-- the ledger rules allow any integer with an 8-byte magnitude, i.e. within
-- @[-(2^64 - 1), 2^64 - 1]@. Values outside of the 'Int64' range are
-- therefore clamped to 'minBound' and 'maxBound' respectively. Consumers
-- which need full fidelity should decode the @native_bytes@ CBOR of the
-- transaction instead.
metadatumToUtxoRpcMetadatum :: L.Metadatum -> Proto UtxoRpc.Metadatum
metadatumToUtxoRpcMetadatum = \case
  L.I int -> do
    let clamped =
          fromInteger . max (toInteger (minBound @Int64)) $
            min (toInteger (maxBound @Int64)) int
    defMessage & U5c.int .~ clamped
  L.B bytes -> defMessage & U5c.bytes .~ bytes
  L.S text -> defMessage & U5c.text .~ text
  L.List elements ->
    defMessage & U5c.array . U5c.items .~ map metadatumToUtxoRpcMetadatum elements
  L.Map keyValuePairs ->
    defMessage
      & U5c.map . U5c.pairs
        .~ ( keyValuePairs <&> \(key, value) ->
               defMessage
                 & U5c.key .~ metadatumToUtxoRpcMetadatum key
                 & U5c.value .~ metadatumToUtxoRpcMetadatum value
           )

-- | Extract the auxiliary scripts of a transaction as ledger 'L.Script'
-- values. Shelley auxiliary data carries no scripts, Allegra and Mary carry
-- timelock scripts, and Alonzo onwards additionally carry Plutus scripts. The
-- dispatch matches on the concrete eras because the auxiliary data and
-- script types of each era family only line up at concrete eras.
txAuxDataScripts
  :: ShelleyBasedEra era
  -> L.TxAuxData (ShelleyLedgerEra era)
  -> [L.Script (ShelleyLedgerEra era)]
txAuxDataScripts sbe auxData =
  case sbe of
    ShelleyBasedEraShelley -> []
    ShelleyBasedEraAllegra -> toList $ auxData ^. L.nativeScriptsTxAuxDataL
    ShelleyBasedEraMary -> toList $ auxData ^. L.nativeScriptsTxAuxDataL
    ShelleyBasedEraAlonzo -> toList $ L.getAlonzoTxAuxDataScripts auxData
    ShelleyBasedEraBabbage -> toList $ L.getAlonzoTxAuxDataScripts auxData
    ShelleyBasedEraConway -> toList $ L.getAlonzoTxAuxDataScripts auxData
    ShelleyBasedEraDijkstra -> toList $ L.getAlonzoTxAuxDataScripts auxData
