{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Cardano.Api.TxBody
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

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
import Hedgehog.Gen (shuffle)
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
    ]
