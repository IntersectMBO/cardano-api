{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Checks that the redeemer pointer index the API assigns to a
-- plutus-witnessed item agrees with the ledger's own resolution of that
-- index.
--
-- Each property builds the real ledger container for its category (a
-- 'Set', 'StrictSeq', 'OSet' or 'Map', via
-- 'Cardano.Ledger.Alonzo.TxBody.Indexable') and compares against it
-- directly.
--
-- Unlike 'prop_extractAllIndexedPlutusScriptWitnesses' in
-- "Test.Cardano.Api.Transaction.Body.Plutus.Scripts", which just counts
-- extracted witnesses, these go through the real
-- 'Cardano.Api.Experimental.Tx.mkTxCertificates' /
-- 'mkTxProposalProcedures' / 'mkTxVotingProcedures' and matching
-- @extractWitnessable*@ / @extractWitnessableCertificates@ functions.
--
-- 'prop_oldApiCertRedeemerIndexMatchesLedgerIndexable',
-- 'prop_oldApiProposalRedeemerIndexMatchesLedgerIndexable' and
-- 'prop_oldApiVoteRedeemerIndexMatchesLedgerIndexable' instead drive the
-- deprecated old API path, via 'Cardano.Api.Tx.mkTxCertificates' /
-- 'mkTxProposalProcedures' / 'mkTxVotingProcedures' and matching
-- @extractWitnessable*@ / @extractWitnessableCertificates@ functions from
-- "Cardano.Api.Tx.Internal.Body".
--
-- 'prop_createTransactionBody_redeemer_pointers_match_ledger' checks the
-- same fix end-to-end: it builds a real 'Cardano.Api.TxBody' via the
-- deprecated 'Cardano.Api.createTransactionBody' and asks the ledger's
-- own 'Cardano.Ledger.Alonzo.TxBody.redeemerPointer' where each
-- plutus-witnessed certificate landed, rather than comparing against a
-- hand-built oracle.
module Test.Cardano.Api.Transaction.Body.Plutus.RedeemerIndex
  ( tests
  )
where

import Cardano.Api (TxIn)
import Cardano.Api qualified as Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus hiding (AnyPlutusScript (..))
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus qualified as Script

import Cardano.Ledger.Alonzo.Scripts (AsItem (..))
import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as L

import Prelude

import Data.Foldable (for_)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.OSet.Strict qualified as OSet
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Word (Word32)

import Test.Gen.Cardano.Api.Typed
  ( genAddressInEra
  , genPolicyAssets
  , genPolicyId
  , genProposal
  , genStakeAddress
  , genStakeCredential
  , genTxIn
  )

import Test.Cardano.Api.Orphans ()

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Transaction.Body.Plutus.RedeemerIndex"
    [ testProperty
        "Input redeemer index matches ledger Indexable oracle"
        prop_txInRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Certificate redeemer index matches ledger Indexable oracle"
        prop_certRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Old API certificate redeemer index matches ledger Indexable oracle"
        prop_oldApiCertRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "createTransactionBody redeemer pointers match the ledger's own resolution"
        prop_createTransactionBody_redeemer_pointers_match_ledger
    , testProperty
        "Proposal redeemer index matches ledger Indexable oracle"
        prop_proposalRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Old API proposal redeemer index matches ledger Indexable oracle"
        prop_oldApiProposalRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Withdrawal redeemer index matches ledger Indexable oracle"
        prop_withdrawalRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Vote redeemer index matches ledger Indexable oracle"
        prop_voteRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Old API vote redeemer index matches ledger Indexable oracle"
        prop_oldApiVoteRedeemerIndexMatchesLedgerIndexable
    , testProperty
        "Mint redeemer index matches ledger Indexable oracle"
        prop_mintRedeemerIndexMatchesLedgerIndexable
    ]

-- ---------------------------------------------------------------------------
-- Inputs: oracle container is a 'Set' of ledger 'L.TxIn', Ord-ranked.
-- ---------------------------------------------------------------------------

prop_txInRedeemerIndexMatchesLedgerIndexable :: Property
prop_txInRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  txIns <-
    take n
      <$> forAll (Gen.filter ((>= n) . length) $ List.nub <$> Gen.list (Range.singleton (n + 3)) genTxIn)
  referenceTxIn <- forAll genTxIn
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  pairs <- forAll $ Gen.shuffle (zip txIns flags)

  cover 20 "at least one witnessed input" $ any snd pairs
  cover 20 "at least one unwitnessed input" $ not (all snd pairs)

  let toWit witnessed = if witnessed then sharedSpendingWitness referenceTxIn else Exp.AnyKeyWitnessPlaceholder
      apiInputs = [(txIn, toWit witnessed) | (txIn, witnessed) <- pairs]
      oracle = Set.fromList $ map (Api.toShelleyTxIn . fst) pairs
      extracted = Exp.extractWitnessableTxIns @ConwayEra apiInputs
      indexed = createIndexedPlutusScriptWitnesses extracted

  length indexed === length (filter snd pairs)
  assertRedeemerMapSize extracted (length indexed)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitTxIn txIn -> do
        idx <- H.nothingFail $ asSpendingIndex purpose
        indexOf (AsItem (Api.toShelleyTxIn txIn)) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitTxIn always produces a ConwaySpending purpose"
        failure :: PropertyT IO ()

-- ---------------------------------------------------------------------------
-- Certificates: oracle container is a 'StrictSeq' of ledger 'L.TxCert', positional
-- (insertion order), matching 'certsTxBodyL'.
-- ---------------------------------------------------------------------------

-- | Certs are laid out as an unwitnessed prefix followed by a witnessed
-- suffix. Dropping unwitnessed certs in 'extractWitnessableCertificates'
-- would then deterministically shift the witnessed certs' indices, not
-- just occasionally.
prop_certRedeemerIndexMatchesLedgerIndexable :: Property
prop_certRedeemerIndexMatchesLedgerIndexable = property $ do
  unwitnessedCount <- forAll $ Gen.int (Range.constant 0 4)
  witnessedCount <- forAll $ Gen.int (Range.constant 1 4)
  let total = unwitnessedCount + witnessedCount
  creds <-
    take total
      <$> forAll
        ( Gen.filter ((>= total) . length) $
            List.nub <$> Gen.list (Range.singleton (total + 3)) genStakeCredential
        )
  shuffledCreds <- forAll $ Gen.shuffle creds
  referenceTxIn <- forAll genTxIn

  let (unwitnessedCreds, witnessedCreds) = List.splitAt unwitnessedCount shuffledCreds
      -- A plain stake registration without a deposit never carries a
      -- witness ('getTxCertWitness' returns 'Nothing' for it): the
      -- "unwitnessed" half.
      mkUnwitnessedCert cred = L.ConwayTxCertDeleg $ L.ConwayRegCert (Api.toShelleyStakeCredential cred) SNothing
      -- A stake delegation always carries a (possibly placeholder)
      -- witness: the "witnessed" half.
      mkWitnessedCert cred =
        L.ConwayTxCertDeleg $
          L.ConwayDelegCert (Api.toShelleyStakeCredential cred) (L.DelegVote L.DRepAlwaysAbstain)
      unwitnessedCerts = map mkUnwitnessedCert unwitnessedCreds
      witnessedCerts = map mkWitnessedCert witnessedCreds
      orderedCerts = unwitnessedCerts ++ witnessedCerts

  cover 20 "at least two witnessed certs" $ length witnessedCerts >= 2
  cover 20 "at least two unwitnessed certs" $ length unwitnessedCerts >= 2

  let apiCerts =
        [(Certificate cert, Exp.AnyKeyWitnessPlaceholder) | cert <- unwitnessedCerts]
          ++ [(Certificate cert, sharedCertifyingWitness referenceTxIn) | cert <- witnessedCerts]
      txCertificates = Exp.mkTxCertificates ConwayEra apiCerts
      extracted = Exp.extractWitnessableCertificates @ConwayEra txCertificates
      indexed = createIndexedPlutusScriptWitnesses extracted
      oracle = StrictSeq.fromList orderedCerts

  length indexed === length witnessedCerts
  assertRedeemerMapSize extracted (length indexed)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitTxCert cert -> do
        idx <- H.nothingFail $ asCertifyingIndex purpose
        indexOf (AsItem cert) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitTxCert always produces a ConwayCertifying purpose"
        failure :: PropertyT IO ()

-- | Regression test for the deprecated old API path. Drives
-- 'Cardano.Api.Tx.mkTxCertificates' and 'extractWitnessableCertificates'
-- directly, rather than their experimental counterparts used by
-- 'prop_certRedeemerIndexMatchesLedgerIndexable' above. Certs are laid out
-- the same way, an unwitnessed prefix followed by a witnessed suffix, so
-- dropping the unwitnessed prefix shifts every witnessed cert's index.
prop_oldApiCertRedeemerIndexMatchesLedgerIndexable :: Property
prop_oldApiCertRedeemerIndexMatchesLedgerIndexable = property $ do
  unwitnessedCount <- forAll $ Gen.int (Range.constant 0 4)
  witnessedCount <- forAll $ Gen.int (Range.constant 1 4)
  let total = unwitnessedCount + witnessedCount
  creds <-
    take total
      <$> forAll
        ( Gen.filter ((>= total) . length) $
            List.nub <$> Gen.list (Range.singleton (total + 3)) genStakeCredential
        )
  shuffledCreds <- forAll $ Gen.shuffle creds
  referenceTxIn <- forAll genTxIn

  let (unwitnessedCreds, witnessedCreds) = List.splitAt unwitnessedCount shuffledCreds
      -- Same rule as 'prop_certRedeemerIndexMatchesLedgerIndexable': a plain
      -- stake registration without a deposit never carries a witness.
      mkUnwitnessedCert cred = L.ConwayTxCertDeleg $ L.ConwayRegCert (Api.toShelleyStakeCredential cred) SNothing
      mkWitnessedCert cred =
        L.ConwayTxCertDeleg $
          L.ConwayDelegCert (Api.toShelleyStakeCredential cred) (L.DelegVote L.DRepAlwaysAbstain)
      unwitnessedCerts = map mkUnwitnessedCert unwitnessedCreds
      witnessedCerts = map mkWitnessedCert witnessedCreds
      orderedCerts = unwitnessedCerts ++ witnessedCerts

  cover 20 "at least two witnessed certs" $ length witnessedCerts >= 2
  cover 20 "at least two unwitnessed certs" $ length unwitnessedCerts >= 2

  let apiCerts =
        [(Certificate cert, Nothing) | cert <- unwitnessedCerts]
          ++ [(Certificate cert, Just (oldApiStakeWitness referenceTxIn)) | cert <- witnessedCerts]
      txCertificates = Api.mkTxCertificates Api.ShelleyBasedEraConway apiCerts
      extracted = Api.extractWitnessableCertificates Api.AlonzoEraOnwardsConway txCertificates
      oracle = StrictSeq.fromList orderedCerts

  indexed <- indexOldApiWitnessed Api.AlonzoEraOnwardsConway extracted

  length indexed === length witnessedCerts

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitTxCert cert -> do
        idx <- H.nothingFail $ asCertifyingIndex purpose
        indexOf (AsItem cert) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitTxCert always produces a ConwayCertifying purpose"
        failure :: PropertyT IO ()

-- | End-to-end regression test for the deprecated old API path, one level
-- up from 'prop_oldApiCertRedeemerIndexMatchesLedgerIndexable': instead of
-- calling 'Api.extractWitnessableCertificates' directly, this drives it through
-- the real 'Cardano.Api.createTransactionBody' and asks the resulting
-- ledger 'L.TxBody' where each plutus-witnessed certificate's redeemer
-- landed via 'L.redeemerPointer', the ledger's own inverse of the indexing
-- this module tests. This catches a mismatch between the extractor and the
-- rest of body construction, not just a bug in the extractor itself.
prop_createTransactionBody_redeemer_pointers_match_ledger :: Property
prop_createTransactionBody_redeemer_pointers_match_ledger = property $ do
  unwitnessedCount <- forAll $ Gen.int (Range.constant 0 4)
  witnessedCount <- forAll $ Gen.int (Range.constant 1 4)
  let total = unwitnessedCount + witnessedCount
  creds <-
    take total
      <$> forAll
        ( Gen.filter ((>= total) . length) $
            List.nub <$> Gen.list (Range.singleton (total + 3)) genStakeCredential
        )
  shuffledCreds <- forAll $ Gen.shuffle creds
  referenceTxIn <- forAll genTxIn
  srcTxIn <- forAll genTxIn
  destAddress <- forAll $ genAddressInEra Api.ShelleyBasedEraConway

  let (unwitnessedCreds, witnessedCreds) = List.splitAt unwitnessedCount shuffledCreds
      -- Same rule as 'prop_certRedeemerIndexMatchesLedgerIndexable': a plain
      -- stake registration without a deposit never carries a witness.
      mkUnwitnessedCert cred = L.ConwayTxCertDeleg $ L.ConwayRegCert (Api.toShelleyStakeCredential cred) SNothing
      mkWitnessedCert cred =
        L.ConwayTxCertDeleg $
          L.ConwayDelegCert (Api.toShelleyStakeCredential cred) (L.DelegVote L.DRepAlwaysAbstain)
      unwitnessedCerts = map mkUnwitnessedCert unwitnessedCreds
      witnessedCerts = map mkWitnessedCert witnessedCreds

  cover 20 "at least two witnessed certs" $ length witnessedCerts >= 2
  cover 20 "at least two unwitnessed certs" $ length unwitnessedCerts >= 2

  let
    -- Every witnessed cert gets its own redeemer, tagged by its position
    -- in 'witnessedCerts'. Unlike the shared 'oldApiStakeWitness' used
    -- elsewhere in this module (whose content never matters, since those
    -- properties only check index arithmetic), distinct redeemers here
    -- let the assertion below pin each cert to *its own* map entry, not
    -- merely to some entry: a hypothetical pointer swap between two
    -- certs would go undetected with a shared redeemer, since both
    -- entries would be identical.
    mkRedeemer :: Integer -> Script.HashableScriptData
    mkRedeemer tag = Script.unsafeHashableScriptData $ Script.ScriptDataConstructor tag []
    executionUnits = Script.ExecutionUnits 0 0
    mkWitness tag =
      Script.PlutusScriptWitness
        Script.PlutusScriptV3InConway
        Script.PlutusScriptV3
        (Script.PReferenceScript referenceTxIn)
        Script.NoScriptDatumForStake
        (mkRedeemer tag)
        executionUnits
    witnessedCertsWithTags = zip witnessedCerts [0 ..]

  let apiCerts =
        [(Certificate cert, Nothing) | cert <- unwitnessedCerts]
          ++ [(Certificate cert, Just (mkWitness tag)) | (cert, tag) <- witnessedCertsWithTags]
      txBodyContent =
        Api.defaultTxBodyContent Api.ShelleyBasedEraConway
          & Api.setTxIns [(srcTxIn, Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending))]
          & Api.setTxOuts
            [ Api.TxOut
                destAddress
                (Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway 10_000_000)
                Api.TxOutDatumNone
                Script.ReferenceScriptNone
            ]
          & Api.setTxFee (Api.TxFeeExplicit Api.ShelleyBasedEraConway 2_000_000)
          & Api.setTxCertificates (Api.mkTxCertificates Api.ShelleyBasedEraConway apiCerts)

  Api.ShelleyTxBody _ builtLedgerBody _ builtScriptData _ _ <-
    evalEither $ Api.createTransactionBody Api.ShelleyBasedEraConway txBodyContent

  case builtScriptData of
    Api.TxBodyScriptData _ _ (L.Redeemers redeemerMap) -> do
      Map.size redeemerMap === length witnessedCerts

      for_ witnessedCertsWithTags $ \(cert, tag) -> do
        let expectedRedeemerPair = (Api.toAlonzoData (mkRedeemer tag), Api.toAlonzoExUnits executionUnits)
        case L.redeemerPointer builtLedgerBody (L.mkCertifyingPurpose (AsItem cert)) of
          SJust purposeIx -> Map.lookup purposeIx redeemerMap === Just expectedRedeemerPair
          SNothing -> annotate "redeemerPointer returned Nothing for a plutus-witnessed cert" >> failure
    Api.TxBodyNoScriptData ->
      annotate
        "impossible: Conway is Alonzo-onwards, createTransactionBody always attaches TxBodyScriptData"
        >> failure

-- ---------------------------------------------------------------------------
-- Proposals: oracle container is an 'OSet' of ledger 'L.ProposalProcedure', positional
-- (insertion order), matching 'proposalProceduresTxBodyL'.
-- ---------------------------------------------------------------------------

-- | Insertion order is forced to be the exact reverse of 'Ord' order.
-- 'pProcDeposit' is the first field 'Ord' compares; making it strictly
-- decrease as each proposal is inserted guarantees every generated case
-- disagrees with Ord-based indexing, not just some of them.
prop_proposalRedeemerIndexMatchesLedgerIndexable :: Property
prop_proposalRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  baseProposals <- forAll $ Gen.list (Range.singleton n) (genProposal Api.ConwayEraOnwardsConway)
  referenceTxIn <- forAll genTxIn
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool

  let orderedProposals =
        [ proposal{L.pProcDeposit = L.Coin (fromIntegral (n - i) * 1_000_000)}
        | (i, proposal) <- zip [0 :: Int ..] baseProposals
        ]

  cover 20 "at least one witnessed proposal" $ or flags
  cover 20 "at least one unwitnessed proposal" $ not (and flags)

  let toWit witnessed = if witnessed then sharedProposingWitness referenceTxIn else Exp.AnyKeyWitnessPlaceholder
      apiProposals = zipWith (\proposal witnessed -> (proposal, toWit witnessed)) orderedProposals flags
      txProposals = Exp.mkTxProposalProcedures @ConwayEra apiProposals
      extracted = Exp.extractWitnessableProposals @ConwayEra (Just txProposals)
      indexed = createIndexedPlutusScriptWitnesses extracted
      oracle = OSet.fromList orderedProposals

  length indexed === length (filter id flags)
  assertRedeemerMapSize extracted (length indexed)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitProposal proposal -> do
        idx <- H.nothingFail $ asProposingIndex purpose
        indexOf (AsItem proposal) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitProposal always produces a ConwayProposing purpose"
        failure :: PropertyT IO ()

-- | Regression test for the deprecated old API path. Drives
-- 'Cardano.Api.Tx.mkTxProposalProcedures' and
-- 'Cardano.Api.Tx.Internal.Body.extractWitnessableProposals' directly,
-- rather than their experimental counterparts used by
-- 'prop_proposalRedeemerIndexMatchesLedgerIndexable' above. Same insertion
-- order trick, with a mix of witnessed and unwitnessed proposals.
prop_oldApiProposalRedeemerIndexMatchesLedgerIndexable :: Property
prop_oldApiProposalRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  baseProposals <- forAll $ Gen.list (Range.singleton n) (genProposal Api.ConwayEraOnwardsConway)
  referenceTxIn <- forAll genTxIn
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool

  let orderedProposals =
        [ proposal{L.pProcDeposit = L.Coin (fromIntegral (n - i) * 1_000_000)}
        | (i, proposal) <- zip [0 :: Int ..] baseProposals
        ]

  cover 20 "at least one witnessed proposal" $ or flags
  cover 20 "at least one unwitnessed proposal" $ not (and flags)

  let toWit witnessed = if witnessed then Just (oldApiStakeWitness referenceTxIn) else Nothing
      apiProposals = zipWith (\proposal witnessed -> (proposal, toWit witnessed)) orderedProposals flags
      txProposals = Api.mkTxProposalProcedures @ConwayEra apiProposals
      extracted =
        Api.extractWitnessableProposals
          Api.ConwayEraOnwardsConway
          (Just (Api.Featured Api.ConwayEraOnwardsConway txProposals))
      oracle = OSet.fromList orderedProposals

  indexed <- indexOldApiWitnessed Api.AlonzoEraOnwardsConway extracted

  length indexed === length (filter id flags)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitProposal proposal -> do
        idx <- H.nothingFail $ asProposingIndex purpose
        indexOf (AsItem proposal) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitProposal always produces a ConwayProposing purpose"
        failure :: PropertyT IO ()

-- ---------------------------------------------------------------------------
-- Withdrawals: oracle container is a 'Map' of ledger reward accounts, Ord-ranked,
-- matching 'unWithdrawals' of 'withdrawalsTxBodyL'.
-- ---------------------------------------------------------------------------

prop_withdrawalRedeemerIndexMatchesLedgerIndexable :: Property
prop_withdrawalRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  addrs <-
    take n
      <$> forAll
        (Gen.filter ((>= n) . length) $ List.nub <$> Gen.list (Range.singleton (n + 3)) genStakeAddress)
  coins <- forAll $ Gen.list (Range.singleton n) (L.Coin <$> Gen.integral (Range.linear 1 10_000_000))
  referenceTxIn <- forAll genTxIn
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  shuffled <- forAll $ Gen.shuffle (zip3 addrs coins flags)

  cover 20 "at least one witnessed withdrawal" $ any (\(_, _, w) -> w) shuffled
  cover 20 "at least one unwitnessed withdrawal" $ any (\(_, _, w) -> not w) shuffled

  let toWit witnessed = if witnessed then sharedWithdrawingWitness referenceTxIn else Exp.AnyKeyWitnessPlaceholder
      apiWithdrawals = Exp.TxWithdrawals [(addr, coin, toWit w) | (addr, coin, w) <- shuffled]
      extracted = Exp.extractWitnessableWithdrawals @ConwayEra apiWithdrawals
      indexed = createIndexedPlutusScriptWitnesses extracted
      oracle = Map.fromList [(Api.toShelleyStakeAddr addr, coin) | (addr, coin, _) <- shuffled]

  length indexed === length (filter (\(_, _, w) -> w) shuffled)
  assertRedeemerMapSize extracted (length indexed)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitWithdrawal addr _coin -> do
        idx <- H.nothingFail $ asRewardingIndex purpose
        indexOf (AsItem (Api.toShelleyStakeAddr addr)) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitWithdrawal always produces a ConwayRewarding purpose"
        failure :: PropertyT IO ()

-- ---------------------------------------------------------------------------
-- Votes: oracle container is a ledger 'L.VotingProcedures' (Map-derived), Ord-ranked on
-- the voter, matching the 'Indexable Voter (VotingProcedures era)' instance.
-- ---------------------------------------------------------------------------

prop_voteRedeemerIndexMatchesLedgerIndexable :: Property
prop_voteRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  voters <-
    take n
      <$> forAll (Gen.filter ((>= n) . length) $ List.nub <$> Gen.list (Range.singleton (n + 3)) genVoter)
  referenceTxIn <- forAll genTxIn
  govActionId <- forAll genGovActionId
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  shuffled <- forAll $ Gen.shuffle (zip voters flags)

  cover 20 "at least one witnessed vote" $ any snd shuffled
  cover 20 "at least one unwitnessed vote" $ not (all snd shuffled)

  let toWit witnessed = if witnessed then sharedVotingWitness referenceTxIn else Exp.AnyKeyWitnessPlaceholder
      votingProcedure = L.VotingProcedure L.VoteYes SNothing
      votingProcedurePairs =
        [ (L.VotingProcedures (Map.singleton voter (Map.singleton govActionId votingProcedure)), toWit w)
        | (voter, w) <- shuffled
        ]

  txVotingProcedures <-
    H.leftFail $ Exp.mkTxVotingProcedures @(LedgerEra ConwayEra) votingProcedurePairs

  let extracted = Exp.extractWitnessableVotes @ConwayEra (Just txVotingProcedures)
      indexed = createIndexedPlutusScriptWitnesses extracted
      oracle =
        L.VotingProcedures $
          Map.fromList [(voter, Map.singleton govActionId votingProcedure) | (voter, _) <- shuffled]

  length indexed === length (filter snd shuffled)
  assertRedeemerMapSize extracted (length indexed)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitVote voter -> do
        idx <- H.nothingFail $ asVotingIndex purpose
        indexOf (AsItem voter) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitVote always produces a ConwayVoting purpose"
        failure :: PropertyT IO ()

-- | Regression test for the deprecated old API path. Drives
-- 'Cardano.Api.Tx.mkTxVotingProcedures' and
-- 'Cardano.Api.Tx.Internal.Body.extractWitnessableVotes' directly, rather
-- than their experimental counterparts used by
-- 'prop_voteRedeemerIndexMatchesLedgerIndexable' above. Same witness map
-- with some voters missing.
prop_oldApiVoteRedeemerIndexMatchesLedgerIndexable :: Property
prop_oldApiVoteRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  voters <-
    take n
      <$> forAll (Gen.filter ((>= n) . length) $ List.nub <$> Gen.list (Range.singleton (n + 3)) genVoter)
  referenceTxIn <- forAll genTxIn
  govActionId <- forAll genGovActionId
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  shuffled <- forAll $ Gen.shuffle (zip voters flags)

  cover 20 "at least one witnessed vote" $ any snd shuffled
  cover 20 "at least one unwitnessed vote" $ not (all snd shuffled)

  let votingProcedure = L.VotingProcedure L.VoteYes SNothing
      toWit witnessed = if witnessed then Just (oldApiStakeWitness referenceTxIn) else Nothing
      votingProcedurePairs =
        [ ( Api.VotingProcedures
              (L.VotingProcedures (Map.singleton voter (Map.singleton govActionId votingProcedure)))
          , toWit w
          )
        | (voter, w) <- shuffled
        ]

  txVotingProcedures <-
    H.leftFail $ Api.mkTxVotingProcedures @Api.BuildTx @ConwayEra votingProcedurePairs

  let extracted =
        Api.extractWitnessableVotes
          Api.ConwayEraOnwardsConway
          (Just (Api.Featured Api.ConwayEraOnwardsConway txVotingProcedures))
      oracle =
        L.VotingProcedures $
          Map.fromList [(voter, Map.singleton govActionId votingProcedure) | (voter, _) <- shuffled]

  indexed <- indexOldApiWitnessed Api.AlonzoEraOnwardsConway extracted

  length indexed === length (filter snd shuffled)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitVote voter -> do
        idx <- H.nothingFail $ asVotingIndex purpose
        indexOf (AsItem voter) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitVote always produces a ConwayVoting purpose"
        failure :: PropertyT IO ()

-- ---------------------------------------------------------------------------
-- Mint: oracle container is a 'Set' of ledger 'L.PolicyID', Ord-ranked,
-- matching 'mintedTxBodyF'. Minting has no key-witness placeholder: every
-- policy is witnessed by some script, so "unwitnessed" here means
-- simple-script-witnessed, not plutus-witnessed.
-- ---------------------------------------------------------------------------

prop_mintRedeemerIndexMatchesLedgerIndexable :: Property
prop_mintRedeemerIndexMatchesLedgerIndexable = property $ do
  n <- forAll $ Gen.int (Range.linear 2 6)
  policyIds <-
    take n
      <$> forAll (Gen.filter ((>= n) . length) $ List.nub <$> Gen.list (Range.singleton (n + 3)) genPolicyId)
  assetsList <- forAll $ Gen.list (Range.singleton n) genPolicyAssets
  referenceTxIn <- forAll genTxIn
  flags <- forAll $ Gen.list (Range.singleton n) Gen.bool
  shuffled <- forAll $ Gen.shuffle (zip3 policyIds assetsList flags)

  cover 20 "at least one plutus-witnessed policy" $ any (\(_, _, w) -> w) shuffled
  cover 20 "at least one simple-script-witnessed policy" $ any (\(_, _, w) -> not w) shuffled

  let toWit witnessed =
        if witnessed
          then sharedMintingWitness referenceTxIn
          else AnyScriptWitnessSimple (SReferenceScript referenceTxIn)
      mintValue = Exp.TxMintValue $ Map.fromList [(pid, (assets, toWit w)) | (pid, assets, w) <- shuffled]
      extractedRaw = Exp.extractWitnessableMints @ConwayEra mintValue
      extracted = [(wit, anyScriptWitnessToAnyWitness sw) | (wit, sw) <- extractedRaw]
      indexed = createIndexedPlutusScriptWitnesses extracted
      oracle = Set.fromList [toLedgerPolicyID pid | (pid, _, _) <- shuffled]

  length indexed === length (filter (\(_, _, w) -> w) shuffled)
  assertRedeemerMapSize extracted (length indexed)

  for_ indexed $ \(AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness witnessable purpose _)) ->
    case witnessable of
      WitMint policyId _assets -> do
        idx <- H.nothingFail $ asMintingIndex purpose
        indexOf (AsItem (toLedgerPolicyID policyId)) oracle === SJust (L.AsIx idx)
      _ -> do
        annotate "impossible: WitMint always produces a ConwayMinting purpose"
        failure :: PropertyT IO ()

-- ---------------------------------------------------------------------------
-- Purpose index extraction (pure, one per category)
-- ---------------------------------------------------------------------------

-- TODO: replace these projections with toPlutusScriptPurposeIndex (added on master in
-- 284d0bd5dd, after this branch's fork point) when the branch is rebased.

-- | Each function matches one expected 'L.ConwayPlutusPurpose' constructor
-- and falls back to 'Nothing' for the rest (statically unreachable, but
-- not provably so to GHC). Same defensive-wildcard pattern as elsewhere
-- in the ledger/api integration; see AGENTS.md's GADT gotchas.
asSpendingIndex
  , asCertifyingIndex
  , asProposingIndex
  , asRewardingIndex
  , asVotingIndex
  , asMintingIndex
    :: L.PlutusPurpose L.AsIx (LedgerEra ConwayEra) -> Maybe Word32
asSpendingIndex (L.ConwaySpending (L.AsIx idx)) = Just idx
asSpendingIndex _ = Nothing
asCertifyingIndex (L.ConwayCertifying (L.AsIx idx)) = Just idx
asCertifyingIndex _ = Nothing
asProposingIndex (L.ConwayProposing (L.AsIx idx)) = Just idx
asProposingIndex _ = Nothing
asRewardingIndex (L.ConwayRewarding (L.AsIx idx)) = Just idx
asRewardingIndex _ = Nothing
asVotingIndex (L.ConwayVoting (L.AsIx idx)) = Just idx
asVotingIndex _ = Nothing
asMintingIndex (L.ConwayMinting (L.AsIx idx)) = Just idx
asMintingIndex _ = Nothing

-- | Run the shared old-API legacy-witness pipeline: convert extracted
-- witnessable/witness pairs into indexed plutus script witnesses, and
-- check the redeemer map has exactly one entry per indexed witness.
--
-- Shared by every old-API regression property above
-- ('prop_oldApiCertRedeemerIndexMatchesLedgerIndexable',
-- 'prop_oldApiProposalRedeemerIndexMatchesLedgerIndexable',
-- 'prop_oldApiVoteRedeemerIndexMatchesLedgerIndexable') to avoid a
-- three-way copy of the "convert, index, assert-size" glue between the
-- cert, proposal and vote twins.
indexOldApiWitnessed
  :: (MonadTest m, L.AlonzoEraScript (Api.ShelleyLedgerEra era))
  => Api.AlonzoEraOnwards era
  -> [ ( Witnessable witnessable (Api.ShelleyLedgerEra era)
       , Api.BuildTxWith Api.BuildTx (Script.Witness ctx era)
       )
     ]
  -> m [AnyIndexedPlutusScriptWitness (Api.ShelleyLedgerEra era)]
indexOldApiWitnessed aeon extracted = do
  converted <- H.leftFail $ legacyWitnessConversion aeon extracted
  let indexed = createIndexedPlutusScriptWitnesses converted
  assertRedeemerMapSize converted (length indexed)
  pure indexed

-- | Sanity check: the redeemer map's size must equal exactly the count of
-- plutus-witnessed items extracted (nothing extra, nothing missing).
assertRedeemerMapSize
  :: (MonadTest m, L.AlonzoEraScript era)
  => [(Witnessable witnessable era, Exp.AnyWitness era)]
  -> Int
  -> m ()
assertRedeemerMapSize extracted expectedCount = do
  let L.Redeemers redeemerMap = getAnyWitnessRedeemerPointerMap extracted
  Map.size redeemerMap === expectedCount

-- ---------------------------------------------------------------------------
-- Shared witness/redeemer fixtures
-- ---------------------------------------------------------------------------

-- | The single Plutus witness reused for every plutus-witnessed item in
-- this module.
--
-- Witness content (redeemer, execution units, reference script) has no
-- bearing on redeemer indexing. Only the witnessed item's identity and
-- whether it is witnessed at all matter, so one shared witness suffices.
sharedPlutusScriptWitness
  :: TxIn -> PlutusScriptWitness L.PlutusV3 purpose (LedgerEra ConwayEra)
sharedPlutusScriptWitness referenceTxIn =
  PlutusScriptWitness
    L.SPlutusV3
    (PReferenceScript referenceTxIn)
    NoScriptDatum
    sharedRedeemer
    sharedExecutionUnits
 where
  sharedRedeemer = Script.unsafeHashableScriptData $ Script.ScriptDataConstructor 0 []
  sharedExecutionUnits = Script.ExecutionUnits 0 0

sharedSpendingWitness :: TxIn -> Exp.AnyWitness (LedgerEra ConwayEra)
sharedSpendingWitness referenceTxIn =
  Exp.AnyPlutusScriptWitness $
    AnyPlutusSpendingScriptWitness $
      PlutusSpendingScriptWitnessV3 (sharedPlutusScriptWitness referenceTxIn)

sharedCertifyingWitness :: TxIn -> Exp.AnyWitness (LedgerEra ConwayEra)
sharedCertifyingWitness referenceTxIn =
  Exp.AnyPlutusScriptWitness $
    AnyPlutusCertifyingScriptWitness (sharedPlutusScriptWitness referenceTxIn)

sharedProposingWitness :: TxIn -> Exp.AnyWitness (LedgerEra ConwayEra)
sharedProposingWitness referenceTxIn =
  Exp.AnyPlutusScriptWitness $
    AnyPlutusProposingScriptWitness (sharedPlutusScriptWitness referenceTxIn)

sharedWithdrawingWitness :: TxIn -> Exp.AnyWitness (LedgerEra ConwayEra)
sharedWithdrawingWitness referenceTxIn =
  Exp.AnyPlutusScriptWitness $
    AnyPlutusWithdrawingScriptWitness (sharedPlutusScriptWitness referenceTxIn)

sharedVotingWitness :: TxIn -> Exp.AnyWitness (LedgerEra ConwayEra)
sharedVotingWitness referenceTxIn =
  Exp.AnyPlutusScriptWitness $ AnyPlutusVotingScriptWitness (sharedPlutusScriptWitness referenceTxIn)

sharedMintingWitness :: TxIn -> AnyScriptWitness (LedgerEra ConwayEra)
sharedMintingWitness referenceTxIn =
  AnyScriptWitnessPlutus $ AnyPlutusMintingScriptWitness (sharedPlutusScriptWitness referenceTxIn)

-- | The old API's counterpart to 'sharedCertifyingWitness' /
-- 'sharedProposingWitness' / 'sharedVotingWitness'. Unlike the experimental
-- API, the old API has no separate witness type per purpose: certificates,
-- proposals and votes are all witnessed under 'Script.WitCtxStake', so one
-- shared witness value covers all three old-API regression properties
-- ('prop_oldApiCertRedeemerIndexMatchesLedgerIndexable',
-- 'prop_oldApiProposalRedeemerIndexMatchesLedgerIndexable',
-- 'prop_oldApiVoteRedeemerIndexMatchesLedgerIndexable'). Witness content has
-- no bearing on redeemer indexing (see 'sharedPlutusScriptWitness'), so this
-- is built directly rather than shared with the experimental fixtures above.
oldApiStakeWitness :: TxIn -> Script.ScriptWitness Script.WitCtxStake ConwayEra
oldApiStakeWitness referenceTxIn =
  Script.PlutusScriptWitness
    Script.PlutusScriptV3InConway
    Script.PlutusScriptV3
    (Script.PReferenceScript referenceTxIn)
    Script.NoScriptDatumForStake
    sharedRedeemer
    sharedExecutionUnits
 where
  sharedRedeemer = Script.unsafeHashableScriptData $ Script.ScriptDataConstructor 0 []
  sharedExecutionUnits = Script.ExecutionUnits 0 0

-- ---------------------------------------------------------------------------
-- Small generators not already provided by Test.Gen.Cardano.Api.Typed
-- ---------------------------------------------------------------------------

-- | 'genStakeCredential' only ever produces 'StakeCredentialByKey', so converting to
-- ledger and coercing the key role never actually fails the pattern match below.
genVoter :: Gen L.Voter
genVoter = do
  cred <- Api.toShelleyStakeCredential <$> genStakeCredential
  case cred of
    L.KeyHashObj keyHash ->
      Gen.element
        [ L.CommitteeVoter (L.KeyHashObj (coerceKeyRole keyHash))
        , L.DRepVoter (L.KeyHashObj (coerceKeyRole keyHash))
        , L.StakePoolVoter (coerceKeyRole keyHash)
        ]
    L.ScriptHashObj{} -> Gen.discard

-- | The governance action a vote targets has no bearing on redeemer indexing (only the
-- voter's identity does), so one fixed action id shared by every generated vote is fine.
genGovActionId :: Gen L.GovActionId
genGovActionId = do
  L.TxIn txId _ <- Api.toShelleyTxIn <$> genTxIn
  pure $ L.GovActionId txId (L.GovActionIx 0)

toLedgerPolicyID :: Api.PolicyId -> L.PolicyID
toLedgerPolicyID (Api.PolicyId scriptHash) = L.PolicyID (Script.toShelleyScriptHash scriptHash)
