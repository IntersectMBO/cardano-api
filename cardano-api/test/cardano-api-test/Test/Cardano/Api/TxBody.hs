{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.TxBody
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Data.Default (def)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Equality
  ( TestEquality (testEquality)
  )
import GHC.Exts (IsList (..))

import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.Orphans ()

import Hedgehog
  ( MonadTest
  , Property
  , (===)
  )
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen (shuffle)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Check the txOuts in a TxBodyContent after a ledger roundtrip.
prop_roundtrip_txbodycontent_txouts :: forall era. ShelleyBasedEra era -> Property
prop_roundtrip_txbodycontent_txouts era = H.property $ do
  (body, content :: TxBodyContent BuildTx era) <-
    shelleyBasedEraConstraints era $ H.forAll $ genValidTxBody era
  -- Convert ledger body back via 'getTxBodyContent' and 'fromLedgerTxBody'
  let content' = getTxBodyContent body
  matchTxOuts (txOuts content) (txOuts content')
 where
  matchTxOuts :: MonadTest m => [TxOut CtxTx era] -> [TxOut CtxTx era] -> m ()
  matchTxOuts as bs =
    mapM_ matchTxOut $ zip as bs

  matchTxOut :: MonadTest m => (TxOut CtxTx era, TxOut CtxTx era) -> m ()
  matchTxOut (a, b) = do
    let TxOut aAddress aValue aDatum aRefScript = a
    let TxOut bAddress bValue bDatum bRefScript = b
    aAddress === bAddress
    aValue === bValue
    matchDatum (aDatum, bDatum)
    matchRefScript (aRefScript, bRefScript)

  -- NOTE: We accept TxOutSupplementalDatum instead of TxOutDatumHash as it may be
  -- correctly resolved given a datum matching the hash was generated.
  matchDatum :: MonadTest m => (TxOutDatum CtxTx era, TxOutDatum CtxTx era) -> m ()
  matchDatum = \case
    (TxOutDatumHash _ dh, TxOutSupplementalDatum _ d) ->
      dh === hashScriptDataBytes d
    (a, b) ->
      a === b

  -- NOTE: After Allegra, all eras interpret SimpleScriptV1 as SimpleScriptV2
  -- because V2 is a superset of V1. So we accept that as a valid conversion.
  matchRefScript :: MonadTest m => (ReferenceScript era, ReferenceScript era) -> m ()
  matchRefScript (a, b)
    | isSimpleScriptV2 a && isSimpleScriptV2 b =
        shelleyBasedEraConstraints era $
          refScriptToShelleyScript era a
            === refScriptToShelleyScript era b
    | otherwise =
        a === b

  isSimpleScriptV2 :: ReferenceScript era -> Bool
  isSimpleScriptV2 = isLang SimpleScriptLanguage

  isLang :: ScriptLanguage a -> ReferenceScript era -> Bool
  isLang expected = \case
    (ReferenceScript _ (ScriptInAnyLang actual _)) -> isJust $ testEquality expected actual
    _ -> False

prop_roundtrip_txbodycontent_conway_fields :: Property
prop_roundtrip_txbodycontent_conway_fields = H.property $ do
  let sbe = ShelleyBasedEraConway
  (body, content) <- H.forAll $ genValidTxBody sbe
  -- Convert ledger body back via 'getTxBodyContent' and 'fromLedgerTxBody'
  let content' = getTxBodyContent body
      proposals = getProposalProcedures . unFeatured <$> txProposalProcedures content
      proposals' = getProposalProcedures . unFeatured <$> txProposalProcedures content'
      votes = getVotingProcedures . unFeatured <$> txVotingProcedures content
      votes' = getVotingProcedures . unFeatured <$> txVotingProcedures content'
      currTreasury = unFeatured <$> txCurrentTreasuryValue content
      currTreasury' = unFeatured <$> txCurrentTreasuryValue content'
      treasuryDonation = unFeatured <$> txTreasuryDonation content
      treasuryDonation' = unFeatured <$> txTreasuryDonation content'

  proposals === proposals'
  votes === votes'
  currTreasury === currTreasury'
  treasuryDonation === treasuryDonation'
 where
  getVotingProcedures TxVotingProceduresNone = Nothing
  getVotingProcedures (TxVotingProcedures vps _) = Just vps
  getProposalProcedures
    :: TxProposalProcedures build era
    -> Maybe [L.ProposalProcedure (ShelleyLedgerEra era)]
  getProposalProcedures TxProposalProceduresNone = Nothing
  getProposalProcedures (TxProposalProcedures pp) = Just $ fst <$> toList pp

prop_simple_script_witness_count :: Property
prop_simple_script_witness_count = H.property $ do
  let sbe = ShelleyBasedEraConway
  (_, contentWithoutScript) <- H.forAll $ genValidTxBody sbe
  script <- H.forAll genSimpleScriptWithoutEmptyAnys
  newTxIn <-
    H.forAll $
      (,BuildTxWith
          ( ScriptWitness
              ScriptWitnessForSpending
              (SimpleScriptWitness SimpleScriptInConway (SScript script))
          ))
        <$> genTxIn
  witList <- H.forAll $ satisfyScript script
  let witCount = fromIntegral $ Set.size witList
  -- We use the inequality @<=@ instead of @==@ because 'estimateTransactionKeyWitnessCount'
  -- calculates an upper bound on the number of key witnesses required to validate a transaction,
  -- and the @witList@ contains a random subset that can potentially be used to satisfy the script.
  -- So we only know it must be smaller or equal to the upper bound.
  H.diff
    (estimateTransactionKeyWitnessCount contentWithoutScript + witCount)
    (<=)
    (estimateTransactionKeyWitnessCount (addTxIn newTxIn contentWithoutScript))
 where
  satisfyScript :: SimpleScript -> H.Gen (Set (Hash PaymentKey))
  satisfyScript (RequireSignature paymentKeyHash) = return $ Set.singleton paymentKeyHash
  satisfyScript (RequireTimeBefore _) = return mempty
  satisfyScript (RequireTimeAfter _) = return mempty
  satisfyScript (RequireAllOf simpleScripts) = Set.unions <$> traverse satisfyScript simpleScripts
  satisfyScript (RequireMOf n simpleScripts) = shuffle simpleScripts >>= satisfyScript . RequireAllOf . take n
  satisfyScript (RequireAnyOf simpleScripts) = satisfyScript (RequireMOf 1 simpleScripts)

-- | Regression test for: a key-credentialed voter (e.g. a key-hash DRep)
-- requires a VKey witness to satisfy the ledger, but the legacy
-- 'estimateTransactionKeyWitnessCount' does not look at 'txVotingProcedures'
-- at all.
--
-- We isolate the vote contribution rather than asserting an absolute count:
-- 'genValidTxBody' may also populate ins/certs/withdrawals/its own votes
-- randomly, so we compare the estimate for a body carrying our generated
-- votes against the /same/ body with the votes field cleared. Everything
-- else is identical on both sides and cancels out exactly, so the delta
-- must equal exactly the number of key-credentialed voters.
--
-- Voters are drawn from a small pool and each one votes on several of a
-- small pool of governance action ids, so the same voter recurs for several
-- action ids and 'mkTxVotingProcedures' has to merge the resulting entries -
-- the collision requested in review.
prop_estimateTransactionKeyWitnessCount_counts_vote_key_witnesses :: Property
prop_estimateTransactionKeyWitnessCount_counts_vote_key_witnesses = H.property $ do
  let sbe = ShelleyBasedEraConway
      ceo = ConwayEraOnwardsConway
  (_, baseContent) <- H.forAll $ genValidTxBody sbe
  (voteEntries, expectedKeyWitnessCount) <- H.forAll $ genVotingProceduresWithKeyWitnessCount ceo
  votingProcedures <- H.leftFail $ mkTxVotingProcedures voteEntries
  let contentWithoutVotes = setTxVotingProcedures Nothing baseContent
      contentWithVotes = setTxVotingProcedures (Just (Featured ceo votingProcedures)) baseContent
  estimateTransactionKeyWitnessCount contentWithVotes
    === estimateTransactionKeyWitnessCount contentWithoutVotes + fromIntegral expectedKeyWitnessCount
 where
  -- Generate a mix of key-credentialed voters (DRep/committee-hot over a
  -- key hash, plus stake pool voters, which are always key-credentialed)
  -- and script-credentialed voters (DRep/committee-hot over a script hash -
  -- stake pool voters have no script-credentialed form). Each bucket draws
  -- its hashes via 'Gen.set', so voters within a bucket never collide as
  -- 'Map' keys; voters across buckets can never collide either, since they
  -- differ in the 'Voter' or 'Credential' constructor regardless of the
  -- underlying hash. Every entry is witnessed by 'Nothing': the legacy
  -- estimator's vote-counting branch never consults the witness map at all
  -- (it only exists for script witnesses, and this function is estimating
  -- KEY witnesses), so the witness value is irrelevant here - only the
  -- ledger-side 'Voter'/'Credential' constructor drives the count.
  --
  -- Votes are drawn from a small pool of governance action ids: every voter
  -- votes on a random non-empty subset of the pool, as one list entry per
  -- action id, so a voter voting on several actions becomes several entries
  -- for the same 'L.Voter' that 'mkTxVotingProcedures' must merge. Returns
  -- the voting procedure entries (ready for 'mkTxVotingProcedures') together
  -- with the number of key-credentialed voters.
  genVotingProceduresWithKeyWitnessCount
    :: ConwayEraOnwards era
    -> H.Gen ([(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))], Int)
  genVotingProceduresWithKeyWitnessCount ceo = do
    drepKeyHashes <- Gen.set (Range.linear 0 3) (unDRepKeyHash <$> genVerificationKeyHash AsDRepKey)
    committeeKeyHashes <-
      Gen.set (Range.linear 0 3) (unCommitteeHotKeyHash <$> genVerificationKeyHash AsCommitteeHotKey)
    stakePoolKeyHashes <-
      Gen.set (Range.linear 0 3) (unStakePoolKeyHash <$> genVerificationKeyHash AsStakePoolKey)
    drepScriptHashes <- Gen.set (Range.linear 0 3) (toShelleyScriptHash <$> genScriptHash)
    committeeScriptHashes <- Gen.set (Range.linear 0 3) (toShelleyScriptHash <$> genScriptHash)
    govActionIdPool <- toList <$> Gen.set (Range.linear 1 3) genGovActionId
    let keyVoters =
          [L.DRepVoter (L.KeyHashObj kh) | kh <- toList drepKeyHashes]
            <> [L.CommitteeVoter (L.KeyHashObj kh) | kh <- toList committeeKeyHashes]
            <> [L.StakePoolVoter kh | kh <- toList stakePoolKeyHashes]
        scriptVoters =
          [L.DRepVoter (L.ScriptHashObj sh) | sh <- toList drepScriptHashes]
            <> [L.CommitteeVoter (L.ScriptHashObj sh) | sh <- toList committeeScriptHashes]
    keyEntries <- concat <$> traverse (voterEntries ceo govActionIdPool) keyVoters
    scriptEntries <- concat <$> traverse (voterEntries ceo govActionIdPool) scriptVoters
    pure (keyEntries <> scriptEntries, length keyVoters)

  votingProcedureValue :: L.VotingProcedure (ShelleyLedgerEra era)
  votingProcedureValue = L.VotingProcedure{L.vProcVote = L.VoteYes, L.vProcAnchor = L.SNothing}

  voterEntries
    :: ConwayEraOnwards era
    -> [L.GovActionId]
    -> L.Voter
    -> H.Gen [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  voterEntries ceo govActionIdPool voter = do
    votedActionIds <- genVotedActionIds govActionIdPool
    pure
      [ (singletonVotingProcedures ceo voter actionId votingProcedureValue, Nothing)
      | actionId <- votedActionIds
      ]

  genGovActionId :: H.Gen L.GovActionId
  genGovActionId =
    (L.GovActionId . toShelleyTxId <$> genTxId)
      <*> (L.GovActionIx <$> Gen.word16 (Range.linear 0 5))

  -- A random non-empty subset of the pool - the source of the same voter
  -- voting on several action ids.
  genVotedActionIds :: [L.GovActionId] -> H.Gen [L.GovActionId]
  genVotedActionIds actionIdPool = do
    count <- Gen.int (Range.linear 1 (length actionIdPool))
    take count <$> shuffle actionIdPool

-- | Cross-role dedupe: a stake key that both withdraws rewards and is
-- deregistered by a certificate in the same transaction needs one key
-- witness, not two, while an unrelated extra key witness still counts
-- separately. A certificate for a different stake key collides with
-- nothing, so all three keys count.
prop_estimateTransactionKeyWitnessCount_dedupes_across_roles :: Property
prop_estimateTransactionKeyWitnessCount_dedupes_across_roles = H.property $ do
  let sbe = ShelleyBasedEraConway
  withdrawingStakeKeyHash <- H.forAll $ genVerificationKeyHash AsStakeKey
  unrelatedStakeKeyHash <- H.forAll $ genVerificationKeyHash AsStakeKey
  extraPaymentKeyHash <- H.forAll $ genVerificationKeyHash AsPaymentKey
  let stakeCredential = StakeCredentialByKey withdrawingStakeKeyHash
      stakeAddress = makeStakeAddress Mainnet stakeCredential
      unregistrationCert credential =
        Exp.Certificate $
          L.ConwayTxCertDeleg $
            L.ConwayUnRegCert (toShelleyStakeCredential credential) (L.SJust (L.Coin 2000000))
      contentWithCertFor credential =
        setTxWithdrawals
          (TxWithdrawals sbe [(stakeAddress, L.Coin 0, BuildTxWith (KeyWitness KeyWitnessForStakeAddr))])
          . setTxCertificates (mkTxCertificates sbe [(unregistrationCert credential, Nothing)])
          . setTxExtraKeyWits (TxExtraKeyWitnesses AlonzoEraOnwardsConway [extraPaymentKeyHash])
          $ defaultTxBodyContent sbe
  -- the deregistered key is the withdrawing key: counted once, plus the extra key witness
  estimateTransactionKeyWitnessCount (contentWithCertFor stakeCredential) === 2
  -- an unrelated deregistered key: nothing collides
  estimateTransactionKeyWitnessCount (contentWithCertFor (StakeCredentialByKey unrelatedStakeKeyHash))
    === 3

-- | A pool registration certificate requires a signature from the operator
-- and from every owner, mirroring ledger's 'getShelleyWitsVKeyNeeded'. An
-- owner who also withdraws rewards in the same transaction still counts once.
prop_estimateTransactionKeyWitnessCount_counts_pool_owners :: Property
prop_estimateTransactionKeyWitnessCount_counts_pool_owners = H.property $ do
  let sbe = ShelleyBasedEraConway
  operatorKeyHash <- H.forAll $ genVerificationKeyHash AsStakePoolKey
  withdrawingStakeKeyHash <- H.forAll $ genVerificationKeyHash AsStakeKey
  otherOwnerStakeKeyHash <- H.forAll $ genVerificationKeyHash AsStakeKey
  unrelatedOwnerStakeKeyHash <- H.forAll $ genVerificationKeyHash AsStakeKey
  VrfKeyHash vrfKeyHash <- H.forAll $ genVerificationKeyHash AsVrfKey
  let stakeAddress = makeStakeAddress Mainnet (StakeCredentialByKey withdrawingStakeKeyHash)
      poolParamsWithOwners owners =
        L.StakePoolParams
          { L.sppId = unStakePoolKeyHash operatorKeyHash
          , L.sppVrf = L.toVRFVerKeyHash vrfKeyHash
          , L.sppPledge = L.Coin 0
          , L.sppCost = L.Coin 0
          , L.sppMargin = minBound
          , L.sppAccountAddress = def
          , L.sppOwners = owners
          , L.sppRelays = mempty
          , L.sppMetadata = L.SNothing
          }
      registrationCertFor owners =
        Exp.Certificate $ L.RegPoolTxCert (poolParamsWithOwners owners)
      contentWithOwners owners =
        setTxWithdrawals
          (TxWithdrawals sbe [(stakeAddress, L.Coin 0, BuildTxWith (KeyWitness KeyWitnessForStakeAddr))])
          . setTxCertificates (mkTxCertificates sbe [(registrationCertFor owners, Nothing)])
          $ defaultTxBodyContent sbe
  -- the withdrawing key is also a pool owner: counted once, plus the operator and the other owner
  estimateTransactionKeyWitnessCount
    ( contentWithOwners
        (Set.fromList [unStakeKeyHash withdrawingStakeKeyHash, unStakeKeyHash otherOwnerStakeKeyHash])
    )
    === 3
  -- owners disjoint from the withdrawing key: nothing collides, so all four keys count
  estimateTransactionKeyWitnessCount
    ( contentWithOwners
        (Set.fromList [unStakeKeyHash otherOwnerStakeKeyHash, unStakeKeyHash unrelatedOwnerStakeKeyHash])
    )
    === 4

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.TxBody"
    [ testProperty "roundtrip txbodycontent txouts Babbage" $
        prop_roundtrip_txbodycontent_txouts ShelleyBasedEraBabbage
    , testProperty "roundtrip txbodycontent txouts Conway" $
        prop_roundtrip_txbodycontent_txouts ShelleyBasedEraConway
    , testProperty
        "roundtrip txbodycontent new conway fields"
        prop_roundtrip_txbodycontent_conway_fields
    , testProperty
        "simple script witness count"
        prop_simple_script_witness_count
    , testProperty
        "vote key witness count"
        prop_estimateTransactionKeyWitnessCount_counts_vote_key_witnesses
    , testProperty
        "cross-role key witness dedupe"
        prop_estimateTransactionKeyWitnessCount_dedupes_across_roles
    , testProperty
        "pool registration counts operator and every owner"
        prop_estimateTransactionKeyWitnessCount_counts_pool_owners
    ]
