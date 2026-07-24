{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Rpc.FetchBlockTx where

import Cardano.Api (SlotNo (..))
import Cardano.Api.Address
  ( serialiseAddress
  , toShelleyAddr
  , toShelleyStakeAddr
  , toShelleyStakeCredential
  )
import Cardano.Api.Era
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus (ScriptData (..), toAlonzoData, unsafeHashableScriptData)
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value (multiAssetToPolicyAssets)
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Certificate (txCertToUtxoRpcCertificate)

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Hashes qualified as L (hashAnnotated)

import RIO hiding (toList)

import Data.Map.Strict qualified as M
import Data.ProtoLens (decodeMessage, encodeMessage)
import Data.Text.Encoding qualified as T
import GHC.IsList (fromList, toList)
import GHC.Stack (withFrozenCallStack)
import Network.GRPC.Spec (Proto (..))

import Test.Gen.Cardano.Api.Typed
  ( genAddressInEra
  , genStakeAddress
  , genStakeCredential
  , genTx
  , genTxIn
  )

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | The latest stable era. The properties which run in a single concrete era
-- use it, so a new stable era only needs updating here; the era-specific
-- constructors in those properties then fail to compile until adjusted.
latestStableEra :: Exp.Era ConwayEra
latestStableEra = Exp.useEra

-- | One-way projection property for 'txToUtxoRpcTx': there is no inverse
-- conversion, so the proto message and the ledger transaction are projected
-- onto comparable facts which must agree. Era-uniform facts are checked for
-- every era; era-gated facts are checked where the era supports them, with
-- the eon-gated ledger lenses as the oracle so the checks stay independent
-- of the any-era getters the conversion reads.
--
-- 40 samples: enough diversity to reach all certificate oneof arms
-- (coupon-collector over the arms of the expected-arm functions).
txToUtxoRpcTxProjections :: forall era. ShelleyBasedEra era -> Property
txToUtxoRpcTxProjections sbe = H.withTests 40 . H.property $ anyEraTxConstraints sbe $ do
  ShelleyTx _ ledgerTx <- forAll $ genTx sbe
  let protoTx = txToUtxoRpcTx ledgerTx
      body = ledgerTx ^. L.bodyTxL
      witnesses = ledgerTx ^. L.witsTxL
      protoCertificates :: [Proto U5c.Certificate]
      protoCertificates = protoTx ^. U5c.certificates

  H.note_ "Transaction hash"
  protoTx ^. U5c.hash === serialiseToRawBytes (fromShelleyTxId (L.txIdTx ledgerTx))

  H.note_ "Fee"
  protoFee <- utxoRpcBigIntToInteger $ protoTx ^. U5c.fee
  protoFee === L.unCoin (body ^. L.feeTxBodyL)

  H.note_ "Inputs preserve the order and content of the sorted ledger input set"
  let projectTxIn :: TxIn -> (ByteString, Word32)
      projectTxIn (TxIn transactionId (TxIx txIx)) =
        (serialiseToRawBytes transactionId, fromIntegral txIx)
      protoInputRefs :: [(ByteString, Word32)]
      protoInputRefs =
        protoTx ^. U5c.inputs <&> \input -> (input ^. U5c.txHash, input ^. U5c.outputIndex)
  protoInputRefs === map (projectTxIn . fromShelleyTxIn) (toList (body ^. L.inputsTxBodyL))

  H.note_ "Outputs preserve position, address and coin"
  let ledgerOutputs = toList (body ^. L.outputsTxBodyL)
      protoOutputs :: [Proto U5c.TxOutput]
      protoOutputs = protoTx ^. U5c.outputs
      expectedAddress ledgerOutput =
        case fromShelleyTxOut sbe ledgerOutput of
          TxOut addressInEra _ _ _ ->
            shelleyBasedEraConstraints sbe $ T.encodeUtf8 (serialiseAddress addressInEra)
  map (\o -> (o ^. U5c.address, o ^. U5c.coin)) protoOutputs
    === map
      (\o -> (expectedAddress o, inject $ o ^. L.coinTxOutL))
      ledgerOutputs

  H.note_ "Certificates route to the expected oneof arms"
  let checkCertificateArms
        :: Show cert
        => (cert -> (String, Proto U5c.Certificate -> Bool))
        -> [cert]
        -> PropertyT IO ()
      checkCertificateArms expectedArm ledgerCertificates = do
        length protoCertificates === length ledgerCertificates
        forM_ (zip ledgerCertificates protoCertificates) $ \(ledgerCertificate, protoCertificate) -> do
          H.annotateShow ledgerCertificate
          H.assertWith protoCertificate $ isJust . (^. U5c.maybe'certificate)
          let (armName, armIsSet) = expectedArm ledgerCertificate
          H.annotate $ "Expected certificate arm: " <> armName
          H.assertWith protoCertificate armIsSet
      -- the signature pins the GADT case's result type, which GHC 9.6/9.10
      -- cannot unify inside the era-constrained arms (MonoLocalBinds)
      checkCertificates :: PropertyT IO ()
      checkCertificates = case sbe of
        ShelleyBasedEraShelley ->
          checkCertificateArms expectedShelleyCertificateArm . toList $ body ^. L.certsTxBodyL
        ShelleyBasedEraAllegra ->
          checkCertificateArms expectedShelleyCertificateArm . toList $ body ^. L.certsTxBodyL
        ShelleyBasedEraMary ->
          checkCertificateArms expectedShelleyCertificateArm . toList $ body ^. L.certsTxBodyL
        ShelleyBasedEraAlonzo ->
          checkCertificateArms expectedShelleyCertificateArm . toList $ body ^. L.certsTxBodyL
        ShelleyBasedEraBabbage ->
          checkCertificateArms expectedShelleyCertificateArm . toList $ body ^. L.certsTxBodyL
        ShelleyBasedEraConway ->
          checkCertificateArms expectedConwayCertificateArm . toList $ body ^. L.certsTxBodyL
        ShelleyBasedEraDijkstra -> pure ()
  checkCertificates

  H.note_ "Withdrawal count"
  length (protoTx ^. U5c.withdrawals) === M.size (L.unWithdrawals (body ^. L.withdrawalsTxBodyL))

  H.note_ "Witness set counts"
  let protoWitnessSet :: Proto U5c.WitnessSet
      protoWitnessSet = protoTx ^. U5c.witnesses
  length (protoWitnessSet ^. U5c.vkeywitness) === length (toList (witnesses ^. L.addrTxWitsL))
  length (protoWitnessSet ^. U5c.bootstrapWitnesses)
    === length (toList (witnesses ^. L.bootAddrTxWitsL))
  length (protoWitnessSet ^. U5c.script) === M.size (witnesses ^. L.scriptTxWitsL)

  H.note_ "Auxiliary data presence"
  isJust (protoTx ^. U5c.maybe'auxiliary)
    === isJust (L.strictMaybeToMaybe (ledgerTx ^. L.auxDataTxL))

  -- era-gated projections: the checks need the era's ledger classes, which
  -- resolve at the concrete eras of the dispatch below
  let validityInterval :: L.AllegraEraTxBody (ShelleyLedgerEra era) => PropertyT IO ()
      validityInterval = do
        H.note_ "Validity interval"
        let L.ValidityInterval lowerBound upperBound = body ^. L.vldtTxBodyL
        protoTx ^. U5c.validity . U5c.start === L.strictMaybe 0 unSlotNo lowerBound
        protoTx ^. U5c.validity . U5c.ttl === L.strictMaybe 0 unSlotNo upperBound
      alwaysSuccessful :: PropertyT IO ()
      alwaysSuccessful = do
        H.note_ "Transactions before Alonzo are always successful"
        H.assertWith protoTx (^. U5c.successful)
      mintCount :: L.MaryEraTxBody (ShelleyLedgerEra era) => PropertyT IO ()
      mintCount = do
        H.note_ "Mint count"
        length (protoTx ^. U5c.mint) === M.size (multiAssetToPolicyAssets (body ^. L.mintTxBodyL))
      referenceInputsCount :: L.BabbageEraTxBody (ShelleyLedgerEra era) => PropertyT IO ()
      referenceInputsCount = do
        H.note_ "Reference input count"
        length (protoTx ^. U5c.referenceInputs) === length (toList (body ^. L.referenceInputsTxBodyL))
      alonzoOnwardsChecks
        :: L.AlonzoEraTx (ShelleyLedgerEra era)
        => AlonzoEraOnwards era
        -> (Bool, Bool)
        -> PropertyT IO ()
      alonzoOnwardsChecks aeo (hasCollateralReturn, hasTotalCollateral) = do
        H.note_ "Phase-2 validation flag"
        let L.IsValid isValid = ledgerTx ^. L.isValidTxL
        protoTx ^. U5c.successful === isValid

        H.note_ "Plutus datum and redeemer counts"
        length (protoWitnessSet ^. U5c.plutusDatums) === M.size (L.unTxDats (witnesses ^. L.datsTxWitsL))
        length (protoWitnessSet ^. U5c.redeemers) === M.size (L.unRedeemers (witnesses ^. L.rdmrsTxWitsL))

        H.note_ "Collateral presence and collateral input count"
        let collateralInputs = toList (body ^. L.collateralInputsTxBodyL)
            protoCollateral :: Maybe (Proto U5c.Collateral)
            protoCollateral = protoTx ^. U5c.maybe'collateral
        isJust protoCollateral
          === (not (null collateralInputs) || hasCollateralReturn || hasTotalCollateral)
        length (maybe [] (^. U5c.collateral) protoCollateral) === length collateralInputs

        H.note_ "Redeemer wiring"
        let redeemerIndexes :: [ScriptWitnessIndex]
            redeemerIndexes =
              map (toScriptIndex aeo . fst)
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
  case sbe of
    ShelleyBasedEraShelley -> do
      H.note_ "Shelley's plain TTL arrives as an interval without a lower bound"
      protoTx ^. U5c.validity . U5c.start === 0
      protoTx ^. U5c.validity . U5c.ttl === body ^. L.ttlTxBodyL . to unSlotNo
      alwaysSuccessful
    ShelleyBasedEraAllegra -> do
      validityInterval
      alwaysSuccessful
    ShelleyBasedEraMary -> do
      validityInterval
      mintCount
      alwaysSuccessful
    ShelleyBasedEraAlonzo -> do
      validityInterval
      mintCount
      alonzoOnwardsChecks AlonzoEraOnwardsAlonzo (False, False)
    ShelleyBasedEraBabbage -> do
      validityInterval
      mintCount
      referenceInputsCount
      alonzoOnwardsChecks
        AlonzoEraOnwardsBabbage
        ( isJust . L.strictMaybeToMaybe $ body ^. L.collateralReturnTxBodyL
        , isJust . L.strictMaybeToMaybe $ body ^. L.totalCollateralTxBodyL
        )
    ShelleyBasedEraConway -> do
      validityInterval
      mintCount
      referenceInputsCount
      H.note_ "Proposal count"
      length (protoTx ^. U5c.proposals) === length (toList (body ^. L.proposalProceduresTxBodyL))
      alonzoOnwardsChecks
        AlonzoEraOnwardsConway
        ( isJust . L.strictMaybeToMaybe $ body ^. L.collateralReturnTxBodyL
        , isJust . L.strictMaybeToMaybe $ body ^. L.totalCollateralTxBodyL
        )
      H.note_ "Outputs roundtrip back to the ledger outputs"
      forM_ (zip protoOutputs ledgerOutputs) $ \(protoOutput, ledgerOutput) -> do
        roundTripped <- utxoRpcTxOutputToTxOut @ConwayEra protoOutput
        roundTripped === fromShelleyTxOut sbe ledgerOutput
    ShelleyBasedEraDijkstra -> pure ()

-- | One projection property per Shelley-based era, from the 'Bounded'
-- enumeration of 'AnyShelleyBasedEra'.
test_tx_to_utxorpc_tx_projections :: [TestTree]
test_tx_to_utxorpc_tx_projections =
  [ testPropertyNamed (show sbe) (fromString (show sbe)) $ txToUtxoRpcTxProjections sbe
  | AnyShelleyBasedEra sbe <- [minBound .. maxBound]
  ]

-- | Positive content coverage for the optional era-gated fields at Conway.
--
-- 'genTx' rarely populates auxiliary data, collateral and proposals, so known
-- values are injected into the generated transaction through the ledger
-- lenses and their proto content is asserted exactly.
--
-- Conway only: the conversion of each of these fields is era-uniform (one
-- getter and one mapping each), so a single content check suffices, and
-- Conway is the only generated era where all of them exist together
-- (collateral needs Babbage, proposals need Conway).
--
-- 10 samples: the injected structure under test is deterministic, 'genTx'
-- only supplies the base transaction.
hprop_tx_to_utxorpc_tx_injected_optional_fields :: Property
hprop_tx_to_utxorpc_tx_injected_optional_fields = H.withTests 10 . H.property $ do
  let sbe = convert latestStableEra
      -- shared with the assertions below, so generation and assertion cannot drift
      expectedAnchorUrl :: Text
      expectedAnchorUrl = "https://example.com/anchor"
      expectedAnchorData :: ByteString
      expectedAnchorData = "anchor data"
  ShelleyTx _ ledgerTx <- forAll $ genTx sbe
  collateralInput <- forAll genTxIn
  returnAddress <- forAll $ genAddressInEra sbe
  stakeAddress <- forAll genStakeAddress
  anchorUrl <- H.nothingFail $ L.textToUrl 64 expectedAnchorUrl
  let anchor = L.Anchor anchorUrl (L.hashAnnotated (L.AnchorData expectedAnchorData))
      returnCoin = 3000000
      totalCollateralCoin = 5000000
      proposalDeposit = 1000000
      metadataValue = L.I 42
      auxData =
        L.mkAlonzoTxAuxData (M.singleton 7 metadataValue) []
      returnTxOut =
        L.mkBasicTxOut (toShelleyAddr returnAddress) (L.inject (L.Coin returnCoin))
      proposal =
        L.ProposalProcedure
          { L.pProcDeposit = L.Coin proposalDeposit
          , L.pProcReturnAddr = toShelleyStakeAddr stakeAddress
          , L.pProcGovAction = L.InfoAction
          , L.pProcAnchor = anchor
          }
      modifiedLedgerTx =
        ledgerTx
          & L.auxDataTxL .~ L.SJust auxData
          & L.bodyTxL . L.collateralInputsTxBodyL .~ fromList [toShelleyTxIn collateralInput]
          & L.bodyTxL . L.collateralReturnTxBodyL .~ L.SJust returnTxOut
          & L.bodyTxL . L.totalCollateralTxBodyL .~ L.SJust (L.Coin totalCollateralCoin)
          & L.bodyTxL . L.proposalProceduresTxBodyL .~ fromList [proposal]
      protoTx = txToUtxoRpcTx modifiedLedgerTx

  H.note_ "The auxiliary data carries the injected metadata and the native script"
  auxiliary <- H.nothingFail $ protoTx ^. U5c.maybe'auxiliary
  map (\m -> (m ^. U5c.label, m ^. U5c.value)) (auxiliary ^. U5c.metadata)
    === [(7, metadatumToUtxoRpcMetadatum metadataValue)]

  H.note_ "The collateral message carries the injected inputs and totals"
  collateral <- H.nothingFail $ protoTx ^. U5c.maybe'collateral
  let TxIn collateralTxId (TxIx collateralIx) = collateralInput
  map (\i -> (i ^. U5c.txHash, i ^. U5c.outputIndex)) (collateral ^. U5c.collateral)
    === [(serialiseToRawBytes collateralTxId, fromIntegral collateralIx)]
  totalCollateral <- H.nothingFail $ collateral ^. U5c.maybe'totalCollateral
  totalCollateral === inject (L.Coin totalCollateralCoin)
  collateralReturn <- H.nothingFail $ collateral ^. U5c.maybe'collateralReturn
  collateralReturn ^. U5c.address
    === shelleyBasedEraConstraints sbe (T.encodeUtf8 (serialiseAddress returnAddress))
  collateralReturn ^. U5c.coin === inject (L.Coin returnCoin)

  H.note_ "The proposal carries the injected deposit, return account, anchor and action"
  [protoProposal] <- H.noteShow $ protoTx ^. U5c.proposals
  protoProposal ^. U5c.deposit === inject (L.Coin proposalDeposit)
  protoProposal ^. U5c.rewardAccount === serialiseToRawBytes stakeAddress
  protoProposal ^. U5c.anchor . U5c.url === expectedAnchorUrl
  protoProposal ^. U5c.anchor . U5c.contentHash
    === L.hashToBytes (L.extractHash (L.hashAnnotated (L.AnchorData expectedAnchorData)))
  H.assertWith (protoProposal ^. U5c.govAction) $ isJust . (^. U5c.maybe'infoAction)

-- | Totality of 'txToUtxoRpcTx' at one era: the proto message roundtrips at
-- the protobuf wire level, which also forces every field, so a partial
-- pattern or bottom in the era's branch fails the property.
--
-- One test per era ('test_tx_to_utxorpc_tx_totality') instead of one
-- property choosing eras at random: the per-era properties are independent,
-- so tasty runs them concurrently and each era gets a fixed sample count.
--
-- Note: Dijkstra is not covered until its upstream wiring lands.
txToUtxoRpcTxTotality :: forall era. Typeable era => ShelleyBasedEra era -> Property
txToUtxoRpcTxTotality sbe = H.withTests 20 . H.property $ do
  ShelleyTx _ ledgerTx <- forAll $ genTx sbe
  -- the signature avoids depending on type inference from later uses,
  -- which GHC 9.6/9.10 reject under MonoLocalBinds
  let protoTx :: Proto U5c.Tx
      protoTx = anyEraTxConstraints sbe $ txToUtxoRpcTx ledgerTx

  H.note_ "Wire-level protobuf roundtrip, forcing the full message"
  decodeMessage (encodeMessage protoTx) === Right protoTx

  H.note_ "Era-gated fields stay empty in eras which do not support them"
  let supportsMint = supportsFrom sbe ShelleyBasedEraMary
      supportsPlutus = supportsFrom sbe ShelleyBasedEraAlonzo
      supportsReferenceInputs = supportsFrom sbe ShelleyBasedEraBabbage
      supportsProposals = supportsFrom sbe ShelleyBasedEraConway
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

-- | One totality test per Shelley-based era, from the 'Bounded' enumeration
-- of 'AnyShelleyBasedEra'.
test_tx_to_utxorpc_tx_totality :: [TestTree]
test_tx_to_utxorpc_tx_totality =
  [ testPropertyNamed (show sbe) (fromString (show sbe)) $
      txToUtxoRpcTxTotality sbe
  | AnyShelleyBasedEra sbe <- [minBound .. maxBound]
  ]

-- | Positive coverage for the redeemer wiring of 'txToUtxoRpcTx' at Conway.
--
-- 'genTx' only produces key-witnessed inputs, unwitnessed certificates and
-- always-empty withdrawals, so generated transactions never carry redeemers
-- and the wiring check in 'hprop_tx_to_utxorpc_tx_conway' only ever exercises
-- its negative branch. To cover the positive branch, known withdrawals,
-- certificates and redeemers are injected into the generated transaction
-- through the ledger lenses, and the wiring is asserted entry by entry: a
-- transposed purpose, index or payload in the implementation fails here.
--
-- 10 samples: the wiring structure under test is injected deterministically,
-- so 'genTx' only supplies the base transaction and sample diversity adds
-- nothing to the property's coverage.
hprop_tx_to_utxorpc_tx_redeemer_wiring :: Property
hprop_tx_to_utxorpc_tx_redeemer_wiring = H.withTests 10 . H.property $ do
  let sbe = convert latestStableEra
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
      protoTx = txToUtxoRpcTx modifiedLedgerTx

  H.note_ "The witness set carries exactly the three injected redeemers"
  length (protoTx ^. U5c.witnesses . U5c.redeemers) === 3

  H.note_ "The spending redeemer is wired to input 0 and to no other input"
  let protoInputs :: [Proto U5c.TxInput]
      protoInputs = protoTx ^. U5c.inputs
  H.assertWith protoInputs $ not . null
  checkRedeemerWiring U5c.REDEEMER_PURPOSE_SPEND [0] protoInputs (^. U5c.maybe'redeemer)
  assertInjectedRedeemer 42 (L.ExUnits 1 2) protoInputs (^. U5c.maybe'redeemer)

  H.note_ "The rewarding redeemer is wired to the injected withdrawal"
  let protoWithdrawals :: [Proto U5c.Withdrawal]
      protoWithdrawals = protoTx ^. U5c.withdrawals
  length protoWithdrawals === 1
  checkRedeemerWiring U5c.REDEEMER_PURPOSE_REWARD [0] protoWithdrawals (^. U5c.maybe'redeemer)
  assertInjectedRedeemer 43 (L.ExUnits 3 4) protoWithdrawals (^. U5c.maybe'redeemer)

  H.note_ "The certifying redeemer is wired to the injected certificate"
  let protoCertificates :: [Proto U5c.Certificate]
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
  -> (msg -> Maybe (Proto U5c.Redeemer))
  -- ^ Read the redeemer field of the message
  -> PropertyT IO ()
checkRedeemerWiring expectedPurpose expectedIndexes protoMessages getRedeemer =
  withFrozenCallStack $
    forM_ (zip [0 ..] protoMessages) $ \(position, protoMessage) ->
      if position `elem` expectedIndexes
        then do
          redeemer <- H.nothingFail $ getRedeemer protoMessage
          redeemer ^. U5c.purpose === Proto expectedPurpose
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
  -> (msg -> Maybe (Proto U5c.Redeemer))
  -- ^ Read the redeemer field of the message
  -> PropertyT IO ()
assertInjectedRedeemer expectedDatum expectedExUnits protoMessages getRedeemer =
  withFrozenCallStack $ do
    protoMessage <- H.nothingFail $ listToMaybe protoMessages
    redeemer <- H.nothingFail $ getRedeemer protoMessage
    redeemer ^. U5c.payload . U5c.bigInt . U5c.int === fromIntegral expectedDatum
    redeemer ^. U5c.exUnits . U5c.memory === fromIntegral (L.exUnitsMem expectedExUnits)
    redeemer ^. U5c.exUnits . U5c.steps === fromIntegral (L.exUnitsSteps expectedExUnits)

-- | Positive coverage for the deposit-less arms of 'txCertToUtxoRpcCertificate'
-- at Conway.
--
-- 'genTx' only produces Conway delegation certificates through
-- 'L.ConwayRegDelegCert', so 'txToUtxoRpcTxProjections' never sees a
-- 'L.ConwayRegCert' or 'L.ConwayUnRegCert' without an explicit 'L.SJust'
-- deposit/refund. The 'L.SNothing' branches, which route to the legacy
-- 'stakeRegistration'/'stakeDeregistration' oneof arms instead of
-- 'regCert'/'unregCert', are therefore never exercised by any other test.
-- Constructing the certificates directly and calling
-- 'txCertToUtxoRpcCertificate' closes that gap.
--
-- 10 samples: the credential does not affect arm routing, so 'genStakeCredential'
-- only supplies incidental diversity.
hprop_conway_deposit_less_certs_route_to_legacy_arms :: Property
hprop_conway_deposit_less_certs_route_to_legacy_arms = H.withTests 10 . H.property $ do
  stakeCredential <- forAll $ toShelleyStakeCredential <$> genStakeCredential
  let sbe = ShelleyBasedEraConway
      registrationCertificate = L.ConwayTxCertDeleg $ L.ConwayRegCert stakeCredential L.SNothing
      deregistrationCertificate = L.ConwayTxCertDeleg $ L.ConwayUnRegCert stakeCredential L.SNothing
      protoRegistration = txCertToUtxoRpcCertificate sbe registrationCertificate
      protoDeregistration = txCertToUtxoRpcCertificate sbe deregistrationCertificate

  H.note_ "A deposit-less registration certificate routes to the legacy stakeRegistration arm"
  let (registrationArmName, registrationArmIsSet) = expectedConwayCertificateArm registrationCertificate
  H.annotate $ "Expected certificate arm: " <> registrationArmName
  H.assertWith protoRegistration registrationArmIsSet
  H.assertWith protoRegistration $ isNothing . (^. U5c.maybe'regCert)

  H.note_ "A deposit-less deregistration certificate routes to the legacy stakeDeregistration arm"
  let (deregistrationArmName, deregistrationArmIsSet) = expectedConwayCertificateArm deregistrationCertificate
  H.annotate $ "Expected certificate arm: " <> deregistrationArmName
  H.assertWith protoDeregistration deregistrationArmIsSet
  H.assertWith protoDeregistration $ isNothing . (^. U5c.maybe'unregCert)

-- | The name and presence check of the proto oneof arm to which
-- 'txCertToUtxoRpcCertificate' must route a Shelley era family ledger
-- certificate.
expectedShelleyCertificateArm
  :: L.ShelleyTxCert era'
  -> (String, Proto U5c.Certificate -> Bool)
expectedShelleyCertificateArm = \case
  L.ShelleyTxCertDelegCert delegCert ->
    case delegCert of
      L.ShelleyRegCert _ ->
        ("stakeRegistration", isJust . (^. U5c.maybe'stakeRegistration))
      L.ShelleyUnRegCert _ ->
        ("stakeDeregistration", isJust . (^. U5c.maybe'stakeDeregistration))
      L.ShelleyDelegCert _ _ ->
        ("stakeDelegation", isJust . (^. U5c.maybe'stakeDelegation))
  L.ShelleyTxCertPool poolCert ->
    case poolCert of
      L.RegPool{} ->
        ("poolRegistration", isJust . (^. U5c.maybe'poolRegistration))
      L.RetirePool{} ->
        ("poolRetirement", isJust . (^. U5c.maybe'poolRetirement))
  L.ShelleyTxCertGenesisDeleg{} ->
    ("genesisKeyDelegation", isJust . (^. U5c.maybe'genesisKeyDelegation))
  L.ShelleyTxCertMir{} ->
    ("mirCert", isJust . (^. U5c.maybe'mirCert))

-- | The name and presence check of the proto oneof arm to which
-- 'txCertToUtxoRpcCertificate' must route a Conway ledger certificate.
expectedConwayCertificateArm
  :: L.TxCert (ShelleyLedgerEra ConwayEra)
  -> (String, Proto U5c.Certificate -> Bool)
expectedConwayCertificateArm = \case
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

-- | Whether an era is at least the given first supported era.
supportsFrom
  :: (Typeable era, Typeable firstSupportedEra)
  => ShelleyBasedEra era
  -> ShelleyBasedEra firstSupportedEra
  -> Bool
supportsFrom sbe firstSupportedEra =
  fromEnum (AnyShelleyBasedEra sbe) >= fromEnum (AnyShelleyBasedEra firstSupportedEra)
