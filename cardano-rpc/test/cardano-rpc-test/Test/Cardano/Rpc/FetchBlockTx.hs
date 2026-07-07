{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.FetchBlockTx where

import Cardano.Api (SlotNo (..))
import Cardano.Api.Address (toShelleyStakeAddr, toShelleyStakeCredential)
import Cardano.Api.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus (ScriptData (..), toAlonzoData, unsafeHashableScriptData)
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value (multiAssetToPolicyAssets)
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L

import RIO hiding (toList)

import Data.Map.Strict qualified as M
import Data.ProtoLens (decodeMessage, encodeMessage)
import GHC.IsList (fromList, toList)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (Proto (..))

import Test.Gen.Cardano.Api.Typed (genStakeAddress, genStakeCredential, genTx)

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as H

-- | One-way property for 'txToUtxoRpcTx' at Conway: there is no inverse
-- conversion, so the proto message and the ledger transaction are projected
-- onto comparable facts which must agree.
hprop_tx_to_utxorpc_tx_conway :: Property
hprop_tx_to_utxorpc_tx_conway = H.property $ do
  let sbe = ShelleyBasedEraConway
  tx@(ShelleyTx _ ledgerTx) <- forAll $ genTx sbe
  let protoTx = txToUtxoRpcTx sbe tx
      body = ledgerTx ^. L.bodyTxL
      witnesses = ledgerTx ^. L.witsTxL

  H.note_ "Transaction hash"
  protoTx ^. U5c.hash === serialiseToRawBytes (fromShelleyTxId (L.txIdTx ledgerTx))

  H.note_ "Fee"
  protoFee <- utxoRpcBigIntToInteger . Proto $ protoTx ^. U5c.fee
  protoFee === L.unCoin (body ^. L.feeTxBodyL)

  H.note_ "Inputs preserve the order and content of the sorted ledger input set"
  let projectTxIn :: TxIn -> (ByteString, Word32)
      projectTxIn (TxIn transactionId (TxIx txIx)) =
        (serialiseToRawBytes transactionId, fromIntegral txIx)
      protoInputRefs :: [(ByteString, Word32)]
      protoInputRefs =
        protoTx ^. U5c.inputs <&> \input -> (input ^. U5c.txHash, input ^. U5c.outputIndex)
  protoInputRefs === map (projectTxIn . fromShelleyTxIn) (toList (body ^. L.inputsTxBodyL))

  H.note_ "Outputs roundtrip back to the ledger outputs"
  let ledgerOutputs = toList (body ^. L.outputsTxBodyL)
      protoOutputs :: [U5c.TxOutput]
      protoOutputs = protoTx ^. U5c.outputs
  length protoOutputs === length ledgerOutputs
  forM_ (zip protoOutputs ledgerOutputs) $ \(protoOutput, ledgerOutput) -> do
    roundTripped <- utxoRpcTxOutputToTxOut @ConwayEra $ Proto protoOutput
    roundTripped === fromShelleyTxOut sbe ledgerOutput

  H.note_ "Certificates route to the expected oneof arms"
  let ledgerCertificates = toList (body ^. L.certsTxBodyL)
      protoCertificates :: [U5c.Certificate]
      protoCertificates = protoTx ^. U5c.certificates
  length protoCertificates === length ledgerCertificates
  forM_ (zip ledgerCertificates protoCertificates) $ \(ledgerCertificate, protoCertificate) -> do
    H.annotateShow ledgerCertificate
    H.assertWith protoCertificate $ isJust . (^. U5c.maybe'certificate)
    let (armName, armIsSet) = expectedCertificateArm ledgerCertificate
    H.annotate $ "Expected certificate arm: " <> armName
    H.assertWith protoCertificate armIsSet

  H.note_ "Counts of the remaining collections"
  length (protoTx ^. U5c.withdrawals) === M.size (L.unWithdrawals (body ^. L.withdrawalsTxBodyL))
  length (protoTx ^. U5c.referenceInputs) === length (toList (body ^. L.referenceInputsTxBodyL))
  length (protoTx ^. U5c.mint) === M.size (multiAssetToPolicyAssets (body ^. L.mintTxBodyL))
  length (protoTx ^. U5c.proposals) === length (toList (body ^. L.proposalProceduresTxBodyL))

  H.note_ "Witness set counts"
  let protoWitnessSet :: U5c.WitnessSet
      protoWitnessSet = protoTx ^. U5c.witnesses
  length (protoWitnessSet ^. U5c.vkeywitness) === length (toList (witnesses ^. L.addrTxWitsL))
  length (protoWitnessSet ^. U5c.bootstrapWitnesses)
    === length (toList (witnesses ^. L.bootAddrTxWitsL))
  length (protoWitnessSet ^. U5c.script) === M.size (witnesses ^. L.scriptTxWitsL)
  length (protoWitnessSet ^. U5c.plutusDatums) === M.size (L.unTxDats (witnesses ^. L.datsTxWitsL))
  length (protoWitnessSet ^. U5c.redeemers) === M.size (L.unRedeemers (witnesses ^. L.rdmrsTxWitsL))

  H.note_ "Phase-2 validation flag"
  let L.IsValid isValid = ledgerTx ^. L.isValidTxL
  protoTx ^. U5c.successful === isValid

  H.note_ "Validity interval"
  let L.ValidityInterval lowerBound upperBound = body ^. L.vldtTxBodyL
  protoTx ^. U5c.validity . U5c.start === L.strictMaybe 0 unSlotNo lowerBound
  protoTx ^. U5c.validity . U5c.ttl === L.strictMaybe 0 unSlotNo upperBound

  H.note_ "Collateral presence and collateral input count"
  let collateralInputs = toList (body ^. L.collateralInputsTxBodyL)
      protoCollateral :: Maybe U5c.Collateral
      protoCollateral = protoTx ^. U5c.maybe'collateral
      hasCollateralReturn = isJust . L.strictMaybeToMaybe $ body ^. L.collateralReturnTxBodyL
      hasTotalCollateral = isJust . L.strictMaybeToMaybe $ body ^. L.totalCollateralTxBodyL
  isJust protoCollateral
    === (not (null collateralInputs) || hasCollateralReturn || hasTotalCollateral)
  length (maybe [] (^. U5c.collateral) protoCollateral) === length collateralInputs

  H.note_ "Auxiliary data presence"
  isJust (protoTx ^. U5c.maybe'auxiliary)
    === isJust (L.strictMaybeToMaybe (ledgerTx ^. L.auxDataTxL))

  H.note_ "Redeemer wiring"
  let redeemerIndexes :: [ScriptWitnessIndex]
      redeemerIndexes =
        map (toScriptIndex AlonzoEraOnwardsConway . fst)
          . M.toList
          . L.unRedeemers
          $ witnesses ^. L.rdmrsTxWitsL
  checkRedeemerWiring
    U5c.REDEEMER_PURPOSE_SPEND
    [ix | ScriptWitnessIndexTxIn ix <- redeemerIndexes]
    (protoTx ^. U5c.inputs)
    (^. U5c.maybe'redeemer)
  checkRedeemerWiring
    U5c.REDEEMER_PURPOSE_REWARD
    [ix | ScriptWitnessIndexWithdrawal ix <- redeemerIndexes]
    (protoTx ^. U5c.withdrawals)
    (^. U5c.maybe'redeemer)
  checkRedeemerWiring
    U5c.REDEEMER_PURPOSE_CERT
    [ix | ScriptWitnessIndexCertificate ix <- redeemerIndexes]
    protoCertificates
    (^. U5c.maybe'redeemer)

-- | Totality of 'txToUtxoRpcTx' across the Shelley-based eras: the proto
-- message roundtrips at the protobuf wire level, which also forces every
-- field, so a partial pattern or bottom in any era branch fails the property.
--
-- Note: 'Bounded' 'AnyShelleyBasedEra' caps at Conway, so Dijkstra is not
-- covered here.
hprop_tx_to_utxorpc_tx_all_eras :: Property
hprop_tx_to_utxorpc_tx_all_eras = H.property $ do
  AnyShelleyBasedEra (sbe :: ShelleyBasedEra era) <-
    forAll $ H.element ([minBound .. maxBound] :: [AnyShelleyBasedEra])
  tx <- forAll $ genTx sbe
  let protoTx = txToUtxoRpcTx sbe tx

  H.note_ "Wire-level protobuf roundtrip, forcing the full message"
  decodeMessage (encodeMessage protoTx) === Right protoTx

  H.note_ "Era-gated fields stay empty in eras which do not support them"
  let supportsMint =
        isJust (forShelleyBasedEraMaybeEon sbe :: Maybe (MaryEraOnwards era))
      supportsPlutus =
        isJust (forShelleyBasedEraMaybeEon sbe :: Maybe (AlonzoEraOnwards era))
      supportsReferenceInputs =
        isJust (forShelleyBasedEraMaybeEon sbe :: Maybe (BabbageEraOnwards era))
      supportsProposals =
        isJust (forShelleyBasedEraMaybeEon sbe :: Maybe (ConwayEraOnwards era))
  unless supportsMint $
    protoTx ^. U5c.mint === []
  unless supportsPlutus $ do
    protoTx ^. U5c.maybe'collateral === Nothing
    protoTx ^. U5c.witnesses . U5c.plutusDatums === []
    protoTx ^. U5c.witnesses . U5c.redeemers === []
    H.assertWith protoTx (^. U5c.successful)
  unless supportsReferenceInputs $
    protoTx ^. U5c.referenceInputs === []
  unless supportsProposals $
    protoTx ^. U5c.proposals === []

-- | Positive coverage for the redeemer wiring of 'txToUtxoRpcTx' at Conway.
--
-- 'genTx' only produces key-witnessed inputs, unwitnessed certificates and
-- always-empty withdrawals, so generated transactions never carry redeemers
-- and the wiring check in 'hprop_tx_to_utxorpc_tx_conway' only ever exercises
-- its negative branch. To cover the positive branch, known withdrawals,
-- certificates and redeemers are injected into the generated transaction
-- through the ledger lenses, and the wiring is asserted entry by entry: a
-- transposed purpose, index or payload in the implementation fails here.
hprop_tx_to_utxorpc_tx_redeemer_wiring :: Property
hprop_tx_to_utxorpc_tx_redeemer_wiring = H.property $ do
  let sbe = ShelleyBasedEraConway
  ShelleyTx _ ledgerTx <- forAll $ genTx sbe
  accountAddress <- forAll $ toShelleyStakeAddr <$> genStakeAddress
  stakeCredential <- forAll $ toShelleyStakeCredential <$> genStakeCredential
  let mkRedeemerDatum :: Integer -> L.Data (ShelleyLedgerEra ConwayEra)
      mkRedeemerDatum = toAlonzoData . unsafeHashableScriptData . ScriptDataNumber
      registrationCertificate :: L.TxCert (ShelleyLedgerEra ConwayEra)
      registrationCertificate = L.ConwayTxCertDeleg $ L.ConwayRegCert stakeCredential L.SNothing
      -- the generator always produces at least one input, so spending index 0
      -- is populated; the injected withdrawal and certificate populate the
      -- rewarding and certifying indexes 0
      modifiedLedgerTx =
        ledgerTx
          & L.bodyTxL . L.withdrawalsTxBodyL
            .~ L.Withdrawals (M.singleton accountAddress (L.Coin 1000000))
          & L.bodyTxL . L.certsTxBodyL .~ fromList [registrationCertificate]
          & L.witsTxL . L.rdmrsTxWitsL
            .~ L.Redeemers
              ( fromList
                  [ (L.ConwaySpending (L.AsIx 0), (mkRedeemerDatum 42, L.ExUnits 1 2))
                  , (L.ConwayRewarding (L.AsIx 0), (mkRedeemerDatum 43, L.ExUnits 3 4))
                  , (L.ConwayCertifying (L.AsIx 0), (mkRedeemerDatum 44, L.ExUnits 5 6))
                  ]
              )
      protoTx = txToUtxoRpcTx sbe (ShelleyTx sbe modifiedLedgerTx)

  H.note_ "The witness set carries exactly the three injected redeemers"
  length (protoTx ^. U5c.witnesses . U5c.redeemers) === 3

  H.note_ "The spending redeemer is wired to input 0 and to no other input"
  let protoInputs :: [U5c.TxInput]
      protoInputs = protoTx ^. U5c.inputs
  H.assertWith protoInputs $ not . null
  checkRedeemerWiring U5c.REDEEMER_PURPOSE_SPEND [0] protoInputs (^. U5c.maybe'redeemer)
  assertInjectedRedeemer 42 (L.ExUnits 1 2) protoInputs (^. U5c.maybe'redeemer)

  H.note_ "The rewarding redeemer is wired to the injected withdrawal"
  let protoWithdrawals :: [U5c.Withdrawal]
      protoWithdrawals = protoTx ^. U5c.withdrawals
  length protoWithdrawals === 1
  checkRedeemerWiring U5c.REDEEMER_PURPOSE_REWARD [0] protoWithdrawals (^. U5c.maybe'redeemer)
  assertInjectedRedeemer 43 (L.ExUnits 3 4) protoWithdrawals (^. U5c.maybe'redeemer)

  H.note_ "The certifying redeemer is wired to the injected certificate"
  let protoCertificates :: [U5c.Certificate]
      protoCertificates = protoTx ^. U5c.certificates
  length protoCertificates === 1
  checkRedeemerWiring U5c.REDEEMER_PURPOSE_CERT [0] protoCertificates (^. U5c.maybe'redeemer)
  assertInjectedRedeemer 44 (L.ExUnits 5 6) protoCertificates (^. U5c.maybe'redeemer)

-- | Assert that the proto entries at list positions carrying a redeemer of
-- the given purpose have that redeemer wired in, and that all other entries
-- have no redeemer set.
checkRedeemerWiring
  :: HasCallStack
  => U5c.RedeemerPurpose
  -- ^ Expected redeemer purpose of the wired entries
  -> [Word32]
  -- ^ Positions which must carry a redeemer
  -> [msg]
  -- ^ Proto messages in transaction order
  -> (msg -> Maybe U5c.Redeemer)
  -- ^ Read the redeemer field of the message
  -> PropertyT IO ()
checkRedeemerWiring expectedPurpose expectedIndexes protoMessages getRedeemer =
  withFrozenCallStack $
    forM_ (zip [0 ..] protoMessages) $ \(position, protoMessage) ->
      if position `elem` expectedIndexes
        then do
          redeemer <- H.nothingFail $ getRedeemer protoMessage
          redeemer ^. U5c.purpose === expectedPurpose
          redeemer ^. U5c.index === position
        else getRedeemer protoMessage === Nothing

-- | Assert that the redeemer wired to the first proto entry carries the
-- injected payload and execution units.
assertInjectedRedeemer
  :: HasCallStack
  => Integer
  -- ^ Expected datum number, as injected via 'ScriptDataNumber'
  -> L.ExUnits
  -- ^ Expected execution units, as injected into the redeemer map
  -> [msg]
  -- ^ Proto messages in transaction order
  -> (msg -> Maybe U5c.Redeemer)
  -- ^ Read the redeemer field of the message
  -> PropertyT IO ()
assertInjectedRedeemer expectedDatum expectedExUnits protoMessages getRedeemer =
  withFrozenCallStack $ do
    protoMessage <- H.nothingFail $ listToMaybe protoMessages
    redeemer <- H.nothingFail $ getRedeemer protoMessage
    redeemer ^. U5c.payload . U5c.bigInt . U5c.int === fromIntegral expectedDatum
    redeemer ^. U5c.exUnits . U5c.memory === fromIntegral (L.exUnitsMem expectedExUnits)
    redeemer ^. U5c.exUnits . U5c.steps === fromIntegral (L.exUnitsSteps expectedExUnits)

-- | The name and presence check of the proto oneof arm to which
-- 'txCertToUtxoRpcCertificate' must route a Conway ledger certificate.
expectedCertificateArm
  :: L.TxCert (ShelleyLedgerEra ConwayEra)
  -> (String, U5c.Certificate -> Bool)
expectedCertificateArm = \case
  L.ConwayTxCertDeleg delegCert ->
    case delegCert of
      L.ConwayRegCert _ L.SNothing ->
        ("stakeRegistration", isJust . (^. U5c.maybe'stakeRegistration))
      L.ConwayRegCert _ (L.SJust _) ->
        ("regCert", isJust . (^. U5c.maybe'regCert))
      L.ConwayUnRegCert _ L.SNothing ->
        ("stakeDeregistration", isJust . (^. U5c.maybe'stakeDeregistration))
      L.ConwayUnRegCert _ (L.SJust _) ->
        ("unregCert", isJust . (^. U5c.maybe'unregCert))
      L.ConwayDelegCert _ (L.DelegStake _) ->
        ("stakeDelegation", isJust . (^. U5c.maybe'stakeDelegation))
      L.ConwayDelegCert _ (L.DelegVote _) ->
        ("voteDelegCert", isJust . (^. U5c.maybe'voteDelegCert))
      L.ConwayDelegCert _ L.DelegStakeVote{} ->
        ("stakeVoteDelegCert", isJust . (^. U5c.maybe'stakeVoteDelegCert))
      L.ConwayRegDelegCert _ (L.DelegStake _) _ ->
        ("stakeRegDelegCert", isJust . (^. U5c.maybe'stakeRegDelegCert))
      L.ConwayRegDelegCert _ (L.DelegVote _) _ ->
        ("voteRegDelegCert", isJust . (^. U5c.maybe'voteRegDelegCert))
      L.ConwayRegDelegCert _ L.DelegStakeVote{} _ ->
        ("stakeVoteRegDelegCert", isJust . (^. U5c.maybe'stakeVoteRegDelegCert))
  L.ConwayTxCertPool poolCert ->
    case poolCert of
      L.RegPool{} ->
        ("poolRegistration", isJust . (^. U5c.maybe'poolRegistration))
      L.RetirePool{} ->
        ("poolRetirement", isJust . (^. U5c.maybe'poolRetirement))
  L.ConwayTxCertGov govCert ->
    case govCert of
      L.ConwayRegDRep{} ->
        ("regDrepCert", isJust . (^. U5c.maybe'regDrepCert))
      L.ConwayUnRegDRep{} ->
        ("unregDrepCert", isJust . (^. U5c.maybe'unregDrepCert))
      L.ConwayUpdateDRep{} ->
        ("updateDrepCert", isJust . (^. U5c.maybe'updateDrepCert))
      L.ConwayAuthCommitteeHotKey{} ->
        ("authCommitteeHotCert", isJust . (^. U5c.maybe'authCommitteeHotCert))
      L.ConwayResignCommitteeColdKey{} ->
        ("resignCommitteeColdCert", isJust . (^. U5c.maybe'resignCommitteeColdCert))
