{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the read-only view of decoded transactions.
module Test.Cardano.Api.Experimental.View
  ( tests
  )
where

import Cardano.Api (AsType (AsPaymentKey))
import Cardano.Api qualified as Api
import Cardano.Api.Address (fromShelleyStakeAddr)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness (AnyScriptWitness (AnyScriptWitnessSimple))
import Cardano.Api.Experimental.Plutus qualified as Exp (plutusScriptInEraToScript)
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Key (Hash (PaymentKeyHash))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
  ( PlutusScript (PlutusScriptSerialised)
  , PlutusScriptVersion (PlutusScriptV3, PlutusScriptV4)
  , ScriptData (ScriptDataNumber)
  , fromAlonzoData
  , fromAlonzoExUnits
  , toAlonzoData
  , unsafeHashableScriptData
  )
import Cardano.Api.Tx (fromShelleyTxIn)
import Cardano.Api.Value (fromMaryPolicyID, multiAssetToPolicyAssets)

import Cardano.Ledger.Alonzo.Tx qualified as Alonzo (ScriptIntegrity (..), hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxAuxData qualified as L (mkAlonzoTxAuxData)
import Cardano.Ledger.Api qualified as UnexportedLedger
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Dijkstra.Scripts qualified as Dijkstra (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxBody qualified as Dijkstra
  ( DijkstraEraTxBody (subTransactionsTxBodyL)
  )
import Cardano.Ledger.Keys qualified as L (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad (when)
import Data.ByteString.Short qualified as SBS
import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as OSet
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import GHC.Exts (fromList)
import Lens.Micro

import Test.Gen.Cardano.Api.Experimental (genSignedSubTx, genSimpleScriptInEra)
import Test.Gen.Cardano.Api.Metadata (genTxMetadata)
import Test.Gen.Cardano.Api.Typed
  ( genPlutusScriptInEra
  , genShelleyWitnessSigningKey
  , genStakeAddress
  , genTxIn
  , genVerificationKeyHash
  )

import Hedgehog (Gen, Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as Q
import Hedgehog.Range qualified as Range
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental.View"
-- ```
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental.View"
    [ testProperty "Conway: a built body views back" $ prop_view_built_body Exp.ConwayEra
    , testProperty "Dijkstra: a built body views back" $ prop_view_built_body Exp.DijkstraEra
    , testProperty "Dijkstra: the Dijkstra-only fields view back" prop_view_dijkstra_fields
    , testProperty "Conway: redeemers point at the items they witness" $
        prop_view_redeemer_purposes Exp.ConwayEra
    , testProperty "Dijkstra: redeemers point at the items they witness" $
        prop_view_redeemer_purposes Exp.DijkstraEra
    , testProperty
        "Dijkstra: guard redeemers count key guards but point only at script guards"
        prop_view_guard_redeemer
    , testProperty
        "Witness scripts that deserialise and one that does not"
        prop_view_witness_scripts
    , testProperty
        "Conway: a script that does not deserialise survives writing and reading the transaction"
        prop_view_decoded_bad_plutus_conway
    , testProperty
        "Dijkstra: a script that does not deserialise survives writing and reading the transaction"
        prop_view_decoded_bad_plutus_dijkstra
    , testProperty "Auxiliary Plutus scripts stay as bytes" prop_view_aux_plutus_scripts
    , testProperty "AnyWitnessable equality" prop_any_witnessable_equality
    , testProperty "Sub-transactions keep their order and witnesses" prop_view_sub_transactions
    , testProperty
        "Both sub-transaction entry points view the transaction they wrap"
        prop_view_sub_transaction_entry_points
    , testProperty
        "Dijkstra: a sub-transaction's redeemers, scripts and datums are its own"
        prop_view_sub_transaction_witnesses
    , testProperty
        "Signed and unsigned views differ only in key witnesses"
        prop_view_signed_vs_unsigned
    ]

-- | Build a body with 'Exp.makeUnsignedTx', view it, and get the fields back.
prop_view_built_body
  :: ( Arbitrary (L.TxCert (Exp.LedgerEra era))
     , Arbitrary (L.NativeScript (Exp.LedgerEra era))
     , Arbitrary (L.ProposalProcedure (Exp.LedgerEra era))
     , Arbitrary (L.VotingProcedures (Exp.LedgerEra era))
     )
  => Exp.Era era
  -> Property
prop_view_built_body era = H.withTests 20 . H.property $ Exp.obtainCommonConstraints era $ do
  ins <- H.forAll $ Gen.list (Range.linear 1 4) genTxIn
  refIns <- H.forAll $ Gen.list (Range.linear 0 3) genTxIn
  outs <- H.forAll $ Gen.list (Range.linear 0 2) (Gen.small (genTxOut era))
  fee <- H.forAll Q.arbitrary
  lowerBound <- H.forAll Q.arbitrary
  upperBound <- H.forAll Q.arbitrary
  donation <- H.forAll Q.arbitrary
  treasury <- H.forAll Q.arbitrary
  collateralIns <- H.forAll $ Gen.list (Range.linear 0 3) genTxIn
  totalCollateral <- H.forAll Q.arbitrary
  Exp.TxOut returnCollateral <- H.forAll $ Gen.small (genTxOut era)
  metadata <- H.forAll genTxMetadata
  auxScripts <- H.forAll $ Gen.list (Range.linear 0 3) (Exp.SimpleScript <$> Q.arbitrary)
  withdrawals <- H.forAll $ Gen.map (Range.linear 0 3) ((,) <$> genStakeAddress <*> Q.arbitrary)
  certs <- H.forAll . fmap List.nub . Gen.list (Range.linear 0 2) $ Gen.small Q.arbitrary
  mint <- H.forAll Q.arbitrary
  mintScriptIn <- H.forAll genTxIn
  requiredSigners <- H.forAll $ Gen.list (Range.linear 0 3) (genVerificationKeyHash AsPaymentKey)
  proposals <- H.forAll . fmap List.nub . Gen.list (Range.linear 0 2) $ Gen.small Q.arbitrary
  votes <- H.forAll $ Gen.small Q.arbitrary
  datums <- H.forAll $ Gen.list (Range.linear 0 3) Q.arbitrary
  let pparams = L.emptyPParams
  -- 'Exp.mkTxVotingProcedures' needs at least one voter.
  setVotes <-
    if Map.null (L.unVotingProcedures votes)
      then pure id
      else
        Exp.setTxVotingProcedures
          <$> H.evalEither (Exp.mkTxVotingProcedures [(votes, Exp.AnyKeyWitnessPlaceholder)])
  let mintValue = multiAssetToPolicyAssets mint
      mintWitness = AnyScriptWitnessSimple (Exp.SReferenceScript mintScriptIn)
      supplementalDatums = fromList [(UnexportedLedger.hashData datum, datum) | datum <- datums]
      content =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder) | txIn <- ins]
          & Exp.setTxInsReference (Exp.TxInsReference refIns Set.empty)
          & Exp.setTxOuts outs
          & Exp.setTxFee fee
          & Exp.setTxValidityLowerBound lowerBound
          & Exp.setTxValidityUpperBound upperBound
          & Exp.setTxTreasuryDonation donation
          & Exp.setTxCurrentTreasuryValue treasury
          & Exp.setTxInsCollateral collateralIns
          & Exp.setTxTotalCollateral (Exp.TxTotalCollateral totalCollateral)
          & Exp.setTxReturnCollateral (Exp.TxReturnCollateral returnCollateral)
          & Exp.setTxMetadata metadata
          & Exp.setTxAuxScripts auxScripts
          & Exp.setTxWithdrawals
            ( Exp.TxWithdrawals
                [(address, coin, Exp.AnyKeyWitnessPlaceholder) | (address, coin) <- Map.toList withdrawals]
            )
          & Exp.setTxCertificates
            (Exp.mkTxCertificates era [(Exp.Certificate cert, Exp.AnyKeyWitnessPlaceholder) | cert <- certs])
          & Exp.setTxMintValue
            ( Exp.TxMintValue
                (fromList [(policyId, (assets, mintWitness)) | (policyId, assets) <- Map.toList mintValue])
            )
          & Exp.setTxExtraKeyWits (Exp.TxExtraKeyWitnesses requiredSigners)
          & Exp.setTxProposalProcedures
            (Exp.mkTxProposalProcedures [(proposal, Exp.AnyKeyWitnessPlaceholder) | proposal <- proposals])
          & setVotes
          & Exp.setTxSupplementalDatums supplementalDatums
          & Exp.setTxProtocolParams pparams
  unsigned@(Exp.UnsignedTx tx) <- H.evalEither $ Exp.makeUnsignedTx era content
  let viewed = Exp.viewUnsignedTx era unsigned
      body = Exp.tvBody viewed
      top = Exp.bvTopTxOnlyFields body
  -- The ledger keeps inputs and reference inputs in sets.
  Set.fromList (Exp.bvIns body) H.=== Set.fromList ins
  Set.fromList (Exp.bvInsReference body) H.=== Set.fromList refIns
  Exp.bvOuts body H.=== outs
  Exp.ttvFee top H.=== fee
  Exp.bvValidityLowerBound body H.=== Just lowerBound
  Exp.bvValidityUpperBound body H.=== Just upperBound
  Exp.bvTreasuryDonation body H.=== donation
  Exp.bvCurrentTreasuryValue body H.=== Just treasury
  Exp.ttvSubTransactions top H.=== []
  Set.fromList (Exp.ttvInsCollateral top) H.=== Set.fromList collateralIns
  Exp.ttvTotalCollateral top H.=== Just (Exp.TxTotalCollateral totalCollateral)
  Exp.ttvReturnCollateral top H.=== Just (Exp.TxOut returnCollateral)
  Exp.bvMetadata body H.=== metadata
  Exp.bvAuxScripts body H.=== auxScripts
  fromList (Exp.bvWithdrawals body) H.=== withdrawals
  Exp.bvCertificates body H.=== map Exp.Certificate certs
  Exp.bvMintValue body H.=== mintValue
  -- The ledger keeps required signers in a set. From Dijkstra on, they are key
  -- guards. The signature lets GHC before 9.12 check the match on the era.
  let requiredSignersCheck :: H.PropertyT IO ()
      requiredSignersCheck = case era of
        Exp.ConwayEra -> do
          let Exp.TxExtraKeyWitnesses keys = Exp.ttvExtraKeyWits top
          Set.fromList keys H.=== Set.fromList requiredSigners
        Exp.DijkstraEra -> do
          Exp.ttvExtraKeyWits top H.=== Exp.TxExtraKeyWitnesses []
          Set.fromList [L.coerceKeyRole kh | L.KeyHashObj kh <- Exp.bvGuards body]
            H.=== Set.fromList [kh | PaymentKeyHash kh <- requiredSigners]
  requiredSignersCheck
  Exp.bvProposalProcedures body H.=== proposals
  Exp.bvVotingProcedures body H.=== votes
  Exp.twvDatums (Exp.tvWitnesses viewed) H.=== supplementalDatums
  Exp.bvAuxDataHash body H.=== (L.hashTxAuxData <$> L.strictMaybeToMaybe (tx ^. L.auxDataTxL))
  -- Without Plutus scripts, the script integrity hash covers only the datums.
  Exp.bvScriptIntegrityHash body
    H.=== if null datums
      then Nothing
      else
        Just
          ( Alonzo.hashScriptIntegrity
              (Alonzo.ScriptIntegrity (L.Redeemers Map.empty) (L.TxDats supplementalDatums) Set.empty)
          )

genTxOut :: Exp.Era era -> Gen (Exp.TxOut (Exp.LedgerEra era))
genTxOut = \case
  Exp.ConwayEra -> Exp.TxOut <$> Q.arbitrary
  Exp.DijkstraEra -> Exp.TxOut <$> Q.arbitrary

prop_view_dijkstra_fields :: Property
prop_view_dijkstra_fields = H.property $ do
  guards :: OSet.OSet (L.Credential L.Guard) <- H.forAll Q.arbitrary
  requiredGuards <- H.forAll Q.arbitrary
  deposits <- H.forAll Q.arbitrary
  intervals <- H.forAll Q.arbitrary
  startingIntervals <- H.forAll Q.arbitrary
  txGuards <- H.evalEither $ Exp.mkTxGuards [(credential, Nothing) | credential <- toList guards]
  let content =
        Exp.defaultTxBodyContent
          & Exp.setTxGuards txGuards
          & Exp.setTxRequiredTopLevelGuards requiredGuards
          & Exp.setTxDirectDeposits deposits
          & Exp.setTxAccountBalanceIntervals intervals
          & Exp.setTxStartingAccountBalanceIntervals startingIntervals
  unsigned <- H.evalEither $ Exp.makeUnsignedTx Exp.DijkstraEra content
  let body = Exp.tvBody (Exp.viewUnsignedTx Exp.DijkstraEra unsigned)
  Exp.bvGuards body H.=== toList guards
  Exp.bvRequiredTopLevelGuards body H.=== requiredGuards
  Exp.bvDirectDeposits body H.=== deposits
  Exp.bvAccountBalanceIntervals body H.=== intervals
  Exp.ttvStartingAccountBalanceIntervals (Exp.bvTopTxOnlyFields body) H.=== startingIntervals

-- | One redeemer for each purpose but guarding, each with its own data and
-- budget. Two inputs and two minting policies give indices above 0, and one
-- redeemer points past the last input. The view lists the redeemers in the
-- ledger's order: by purpose, then by index.
prop_view_redeemer_purposes
  :: forall era
   . ( Arbitrary (L.TxCert (Exp.LedgerEra era))
     , Arbitrary (L.ProposalProcedure (Exp.LedgerEra era))
     , Arbitrary (L.VotingProcedure (Exp.LedgerEra era))
     , UnexportedLedger.ConwayEraScript (Exp.LedgerEra era)
     )
  => Exp.Era era
  -> Property
prop_view_redeemer_purposes era = H.property $ Exp.obtainCommonConstraints era $ do
  txIns <- H.forAll $ Gen.set (Range.singleton 2) Q.arbitrary
  policyIds <- H.forAll $ Gen.set (Range.singleton 2) Q.arbitrary
  assetName <- H.forAll Q.arbitrary
  cert <- H.forAll Q.arbitrary
  account <- H.forAll Q.arbitrary
  amount <- H.forAll Q.arbitrary
  voter <- H.forAll Q.arbitrary
  votes <- H.forAll Q.arbitrary
  proposal <- H.forAll Q.arbitrary
  let mint =
        L.MultiAsset (fromList [(policyId, Map.singleton assetName 1) | policyId <- toList policyIds])
      body =
        L.mkBasicTxBody
          & L.inputsTxBodyL .~ txIns
          & L.mintTxBodyL .~ mint
          & L.certsTxBodyL .~ StrictSeq.singleton cert
          & L.withdrawalsTxBodyL .~ L.Withdrawals (Map.singleton account amount)
          & L.votingProceduresTxBodyL .~ L.VotingProcedures (Map.singleton voter votes)
          & L.proposalProceduresTxBodyL .~ OSet.singleton proposal
      pointers :: [L.PlutusPurpose L.AsIx (Exp.LedgerEra era)]
      pointers = case era of
        Exp.ConwayEra ->
          [ L.ConwaySpending (L.AsIx 0)
          , L.ConwaySpending (L.AsIx 1)
          , L.ConwaySpending (L.AsIx 7)
          , L.ConwayMinting (L.AsIx 0)
          , L.ConwayMinting (L.AsIx 1)
          , L.ConwayCertifying (L.AsIx 0)
          , L.ConwayWithdrawing (L.AsIx 0)
          , L.ConwayVoting (L.AsIx 0)
          , L.ConwayProposing (L.AsIx 0)
          ]
        Exp.DijkstraEra ->
          [ Dijkstra.DijkstraSpending (L.AsIx 0)
          , Dijkstra.DijkstraSpending (L.AsIx 1)
          , Dijkstra.DijkstraSpending (L.AsIx 7)
          , Dijkstra.DijkstraMinting (L.AsIx 0)
          , Dijkstra.DijkstraMinting (L.AsIx 1)
          , Dijkstra.DijkstraCertifying (L.AsIx 0)
          , Dijkstra.DijkstraWithdrawing (L.AsIx 0)
          , Dijkstra.DijkstraVoting (L.AsIx 0)
          , Dijkstra.DijkstraProposing (L.AsIx 0)
          ]
      -- The redeemer of the i-th pointer passes the number i, with a budget of i.
      redeemers :: [(L.Data (Exp.LedgerEra era), L.ExUnits)]
      redeemers =
        [ ( toAlonzoData (unsafeHashableScriptData (ScriptDataNumber i))
          , L.ExUnits (fromInteger i) (fromInteger i)
          )
        | i <- [0 .. toInteger (length pointers) - 1]
        ]
      tx =
        L.mkBasicTx body
          & L.witsTxL . L.rdmrsTxWitsL .~ L.Redeemers (fromList (zip pointers redeemers))
      viewed = Exp.twvRedeemers (Exp.tvWitnesses (Exp.viewUnsignedTx era (Exp.UnsignedTx tx)))
      spends index = Just (Exp.AnyWitnessable (Exp.WitTxIn (fromShelleyTxIn (Set.elemAt index txIns))))
      mints index =
        let apiPolicyId = fromMaryPolicyID (Set.elemAt index policyIds)
         in Just
              (Exp.AnyWitnessable (Exp.WitMint apiPolicyId (multiAssetToPolicyAssets mint Map.! apiPolicyId)))
      items =
        [ (Exp.SpendingScript, 0, spends 0)
        , (Exp.SpendingScript, 1, spends 1)
        , (Exp.SpendingScript, 7, Nothing)
        , (Exp.MintingScript, 0, mints 0)
        , (Exp.MintingScript, 1, mints 1)
        , (Exp.CertifyingScript, 0, Just (Exp.AnyWitnessable (Exp.WitTxCert cert)))
        ,
          ( Exp.WithdrawingScript
          , 0
          , Just (Exp.AnyWitnessable (Exp.WitWithdrawal (fromShelleyStakeAddr account) amount))
          )
        , (Exp.VotingScript, 0, Just (Exp.AnyWitnessable (Exp.WitVote voter)))
        , (Exp.ProposingScript, 0, Just (Exp.AnyWitnessable (Exp.WitProposal proposal)))
        ]
  [(Exp.trvPurpose r, Exp.trvIndex r, Exp.trvItem r, Exp.trvData r, Exp.trvExUnits r) | r <- viewed]
    H.=== [ (purpose, index, item, fromAlonzoData redeemerData, fromAlonzoExUnits exUnits)
          | ((purpose, index, item), (redeemerData, exUnits)) <- zip items redeemers
          ]

-- | Guard redeemer indices count key guards and script guards alike, but only
-- a script guard takes a redeemer: one that points at a key guard finds
-- nothing.
prop_view_guard_redeemer :: Property
prop_view_guard_redeemer = H.property $ do
  keyHash <- H.forAll Q.arbitrary
  scriptHash <- H.forAll Q.arbitrary
  redeemer <- H.forAll Q.arbitrary
  let body =
        L.mkBasicTxBody
          & UnexportedLedger.guardsTxBodyL
            .~ OSet.fromList [L.KeyHashObj keyHash, L.ScriptHashObj scriptHash]
      pointers :: [L.PlutusPurpose L.AsIx (Exp.LedgerEra Exp.DijkstraEra)]
      pointers = [Dijkstra.DijkstraGuarding (L.AsIx 0), Dijkstra.DijkstraGuarding (L.AsIx 1)]
      tx =
        L.mkBasicTx body
          & L.witsTxL . L.rdmrsTxWitsL .~ L.Redeemers (fromList [(pointer, redeemer) | pointer <- pointers])
      viewed = Exp.twvRedeemers (Exp.tvWitnesses (Exp.viewUnsignedTx Exp.DijkstraEra (Exp.UnsignedTx tx)))
  [(Exp.trvPurpose r, Exp.trvIndex r, Exp.trvItem r) | r <- viewed]
    H.=== [ (Exp.GuardingScript, 0, Nothing)
          , (Exp.GuardingScript, 1, Just (Exp.AnyWitnessable (Exp.WitGuard (L.ScriptHashObj scriptHash))))
          ]

prop_view_witness_scripts :: Property
prop_view_witness_scripts = H.property $ do
  plutusScriptInEra <- H.forAll genPlutusScriptInEra
  Exp.SimpleScript native <- H.forAll genSimpleScriptInEra
  let nativeScript :: L.Script (Exp.LedgerEra Exp.ConwayEra)
      nativeScript = L.fromNativeScript native
      plutusScript = Exp.plutusScriptInEraToScript plutusScriptInEra
      -- These bytes are not a Plutus program.
      badBytes = SBS.pack [0x01, 0x02, 0x03]
  badPlutusScript <-
    H.evalMaybe $ L.mkPlutusScript (Plutus.Plutus @Plutus.PlutusV3 (Plutus.PlutusBinary badBytes))
  let badScript = L.fromPlutusScript badPlutusScript
      scripts = [nativeScript, plutusScript, badScript]
      tx =
        L.mkBasicTx L.mkBasicTxBody
          & L.witsTxL . L.scriptTxWitsL .~ fromList [(L.hashScript script, script) | script <- scripts]
      viewed = Exp.twvScripts (Exp.tvWitnesses (Exp.viewUnsignedTx Exp.ConwayEra (Exp.UnsignedTx tx)))
  Map.keysSet viewed H.=== fromList (map L.hashScript scripts)
  case Map.lookup (L.hashScript nativeScript) viewed of
    Just (Exp.DecodedScript (Exp.AnySimpleScript _)) -> H.success
    other -> H.annotateShow other >> H.failure
  case Map.lookup (L.hashScript plutusScript) viewed of
    Just (Exp.DecodedScript (Exp.AnyPlutusScript _)) -> H.success
    other -> H.annotateShow other >> H.failure
  Map.lookup (L.hashScript badScript) viewed
    H.=== Just
      (Exp.UndecodedScript (Exp.UndecodedPlutusScript PlutusScriptV3 (PlutusScriptSerialised badBytes)))

-- | Write a transaction with a Plutus script whose bytes do not deserialise,
-- read it back, and view it: the script keeps its language and its bytes.
prop_view_decoded_bad_plutus_conway :: Property
prop_view_decoded_bad_plutus_conway = H.propertyOnce $ do
  let badBytes = SBS.pack [0x01, 0x02, 0x03]
  badPlutusScript <-
    H.evalMaybe $ L.mkPlutusScript (Plutus.Plutus @Plutus.PlutusV3 (Plutus.PlutusBinary badBytes))
  let badScript :: L.Script (Exp.LedgerEra Exp.ConwayEra)
      badScript = L.fromPlutusScript badPlutusScript
      tx =
        L.mkBasicTx L.mkBasicTxBody
          & L.witsTxL . L.scriptTxWitsL .~ Map.singleton (L.hashScript badScript) badScript
      bytes = Api.serialiseToRawBytes (Exp.SignedTx tx :: Exp.SignedTx Exp.ConwayEra)
  decoded <-
    H.evalEither $ Api.deserialiseFromRawBytes (Api.asType @(Exp.SignedTx Exp.ConwayEra)) bytes
  Map.elems (Exp.twvScripts (Exp.tvWitnesses (Exp.viewSignedTx Exp.ConwayEra decoded)))
    H.=== [Exp.UndecodedScript (Exp.UndecodedPlutusScript PlutusScriptV3 (PlutusScriptSerialised badBytes))]

-- | Like 'prop_view_decoded_bad_plutus_conway', with Plutus V4 in Dijkstra.
prop_view_decoded_bad_plutus_dijkstra :: Property
prop_view_decoded_bad_plutus_dijkstra = H.propertyOnce $ do
  let badBytes = SBS.pack [0x01, 0x02, 0x03]
  badPlutusScript <-
    H.evalMaybe $ L.mkPlutusScript (Plutus.Plutus @Plutus.PlutusV4 (Plutus.PlutusBinary badBytes))
  let badScript :: L.Script (Exp.LedgerEra Exp.DijkstraEra)
      badScript = L.fromPlutusScript badPlutusScript
      tx =
        L.mkBasicTx L.mkBasicTxBody
          & L.witsTxL . L.scriptTxWitsL .~ Map.singleton (L.hashScript badScript) badScript
      bytes = Api.serialiseToRawBytes (Exp.SignedTx tx :: Exp.SignedTx Exp.DijkstraEra)
  decoded <-
    H.evalEither $ Api.deserialiseFromRawBytes (Api.asType @(Exp.SignedTx Exp.DijkstraEra)) bytes
  Map.elems (Exp.twvScripts (Exp.tvWitnesses (Exp.viewSignedTx Exp.DijkstraEra decoded)))
    H.=== [Exp.UndecodedScript (Exp.UndecodedPlutusScript PlutusScriptV4 (PlutusScriptSerialised badBytes))]

prop_view_aux_plutus_scripts :: Property
prop_view_aux_plutus_scripts = H.propertyOnce $ do
  -- Auxiliary scripts are not deserialised, so any bytes do.
  let bytes = SBS.pack [0x01, 0x02, 0x03]
  plutusScript <-
    H.evalMaybe $ L.mkPlutusScript (Plutus.Plutus @Plutus.PlutusV3 (Plutus.PlutusBinary bytes))
  let auxData :: L.TxAuxData (Exp.LedgerEra Exp.ConwayEra)
      auxData = L.mkAlonzoTxAuxData mempty [L.fromPlutusScript plutusScript]
      tx = L.mkBasicTx L.mkBasicTxBody & L.auxDataTxL .~ L.SJust auxData
      body = Exp.tvBody (Exp.viewUnsignedTx Exp.ConwayEra (Exp.UnsignedTx tx))
  Exp.bvAuxPlutusScripts body
    H.=== [Exp.UndecodedPlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes)]
  length (Exp.bvAuxScripts body) H.=== 0

prop_any_witnessable_equality :: Property
prop_any_witnessable_equality = H.property $ do
  txIn <- H.forAll genTxIn
  scriptHash <- H.forAll Q.arbitrary
  let spend, vote, guard :: Exp.AnyWitnessable (Exp.LedgerEra Exp.DijkstraEra)
      spend = Exp.AnyWitnessable (Exp.WitTxIn txIn)
      vote = Exp.AnyWitnessable (Exp.WitVote (L.DRepVoter (L.ScriptHashObj scriptHash)))
      guard = Exp.AnyWitnessable (Exp.WitGuard (L.ScriptHashObj scriptHash))
  spend H.=== spend
  vote H.=== vote
  -- Different kinds of item are not equal, even when they hold the same hash.
  vote H./== guard
  spend H./== guard

prop_view_sub_transactions :: Property
prop_view_sub_transactions = H.withTests 20 . H.property $ do
  subTxs <- H.forAll $ Gen.list (Range.linear 0 3) genSignedSubTx
  let ids = map Exp.getSignedSubTxId subTxs
  -- The ledger keys sub-transactions by their IDs, so equal ones would merge.
  when (Set.size (fromList ids) /= length ids) H.discard
  unsigned <-
    H.evalEither
      $ Exp.makeUnsignedTx Exp.DijkstraEra
      $ Exp.defaultTxBodyContent & Exp.setTxSignedSubTransactions subTxs
  let viewed =
        Exp.ttvSubTransactions
          (Exp.bvTopTxOnlyFields (Exp.tvBody (Exp.viewUnsignedTx Exp.DijkstraEra unsigned)))
  map Exp.tvId viewed H.=== ids
  map (Exp.twvKeyWitnesses . Exp.tvWitnesses) viewed
    H.=== [toList (tx ^. L.witsTxL . L.addrTxWitsL) | Exp.SignedSubTx tx <- subTxs]

prop_view_sub_transaction_entry_points :: Property
prop_view_sub_transaction_entry_points = H.withTests 20 . H.property $ do
  signedSubTx@(Exp.SignedSubTx tx) <- H.forAll genSignedSubTx
  let viewed = Exp.viewSignedSubTx signedSubTx
  Exp.tvId viewed H.=== Exp.getSignedSubTxId signedSubTx
  Exp.viewUnsignedSubTx (Exp.UnsignedSubTx tx) H.=== viewed

-- | The top level and its sub-transaction each have one input, and their own
-- redeemer for it, script and datum. Each redeemer must point at the input of
-- its own body, and each view must show only its own witnesses.
prop_view_sub_transaction_witnesses :: Property
prop_view_sub_transaction_witnesses = H.property $ do
  topIn <- H.forAll Q.arbitrary
  subIn <- H.forAll Q.arbitrary
  topRedeemer <- H.forAll Q.arbitrary
  subRedeemer <- H.forAll Q.arbitrary
  topDatum <- H.forAll Q.arbitrary
  subDatum <- H.forAll Q.arbitrary
  topNative :: L.NativeScript (Exp.LedgerEra Exp.DijkstraEra) <- H.forAll Q.arbitrary
  subNative :: L.NativeScript (Exp.LedgerEra Exp.DijkstraEra) <- H.forAll Q.arbitrary
  let topScript = L.fromNativeScript topNative
      subScript = L.fromNativeScript subNative
  when
    ( topIn == subIn
        || topRedeemer == subRedeemer
        || topDatum == subDatum
        || L.hashScript topScript == L.hashScript subScript
    )
    H.discard
  let pointer :: L.PlutusPurpose L.AsIx (Exp.LedgerEra Exp.DijkstraEra)
      pointer = Dijkstra.DijkstraSpending (L.AsIx 0)
      datums datum = Map.singleton (UnexportedLedger.hashData datum) datum
      subTx =
        L.mkBasicTx (L.mkBasicTxBody & L.inputsTxBodyL .~ Set.singleton subIn)
          & L.witsTxL . L.rdmrsTxWitsL .~ L.Redeemers (Map.singleton pointer subRedeemer)
          & L.witsTxL . L.scriptTxWitsL .~ Map.singleton (L.hashScript subScript) subScript
          & L.witsTxL . L.datsTxWitsL .~ L.TxDats (datums subDatum)
      tx =
        L.mkBasicTx
          ( L.mkBasicTxBody
              & L.inputsTxBodyL .~ Set.singleton topIn
              & Dijkstra.subTransactionsTxBodyL .~ OMap.singleton subTx
          )
          & L.witsTxL . L.rdmrsTxWitsL .~ L.Redeemers (Map.singleton pointer topRedeemer)
          & L.witsTxL . L.scriptTxWitsL .~ Map.singleton (L.hashScript topScript) topScript
          & L.witsTxL . L.datsTxWitsL .~ L.TxDats (datums topDatum)
      viewed = Exp.viewUnsignedTx Exp.DijkstraEra (Exp.UnsignedTx tx)
      topWitnesses = Exp.tvWitnesses viewed
      subWitnesses =
        map Exp.tvWitnesses (Exp.ttvSubTransactions (Exp.bvTopTxOnlyFields (Exp.tvBody viewed)))
      redeemerViews witnesses =
        [(Exp.trvItem r, Exp.trvData r, Exp.trvExUnits r) | r <- Exp.twvRedeemers witnesses]
      spends txIn (redeemerData, exUnits) =
        ( Just (Exp.AnyWitnessable (Exp.WitTxIn (fromShelleyTxIn txIn)))
        , fromAlonzoData redeemerData
        , fromAlonzoExUnits exUnits
        )
  redeemerViews topWitnesses H.=== [spends topIn topRedeemer]
  map redeemerViews subWitnesses H.=== [[spends subIn subRedeemer]]
  Map.keys (Exp.twvScripts topWitnesses) H.=== [L.hashScript topScript]
  map (Map.keys . Exp.twvScripts) subWitnesses H.=== [[L.hashScript subScript]]
  Exp.twvDatums topWitnesses H.=== datums topDatum
  map Exp.twvDatums subWitnesses H.=== [datums subDatum]

prop_view_signed_vs_unsigned :: Property
prop_view_signed_vs_unsigned = H.property $ do
  txIn <- H.forAll genTxIn
  sk <- H.forAllWith (const "<ShelleyWitnessSigningKey>") genShelleyWitnessSigningKey
  unsigned <-
    H.evalEither
      $ Exp.makeUnsignedTx Exp.ConwayEra
      $ Exp.defaultTxBodyContent & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
  let witness = Exp.makeKeyWitness Exp.ConwayEra unsigned sk
      signed = Exp.signTx Exp.ConwayEra [] [witness] unsigned
      signedView = Exp.viewSignedTx Exp.ConwayEra signed
      unsignedView = Exp.viewUnsignedTx Exp.ConwayEra unsigned
  Exp.tvId signedView H.=== Exp.tvId unsignedView
  Exp.tvBody signedView H.=== Exp.tvBody unsignedView
  Exp.twvKeyWitnesses (Exp.tvWitnesses signedView) H.=== [witness]
  Exp.twvKeyWitnesses (Exp.tvWitnesses unsignedView) H.=== []
  (Exp.tvWitnesses signedView){Exp.twvKeyWitnesses = []} H.=== Exp.tvWitnesses unsignedView
