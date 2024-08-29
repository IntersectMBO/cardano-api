{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use list comprehension" -}
{- HLINT ignore "Use camelCase" -}

module Test.Cardano.Api.Typed.TxBody
  ( tests
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Script
import           Cardano.Api.Shelley (Address (..), LedgerProtocolParameters (..), ShelleyLedgerEra)

import qualified Cardano.Ledger.Mary.Value as L
import qualified Cardano.Ledger.Shelley.Scripts as L
import qualified Cardano.Slotting.EpochInfo as CS
import qualified Cardano.Slotting.Slot as CS
import qualified Cardano.Slotting.Time as CS

import qualified Data.ByteString as B
import           Data.Function
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import qualified Data.Time.Format as DT
import           Data.Type.Equality (TestEquality (testEquality))
import           GHC.Exts (IsList (..), IsString (..))
import           GHC.Stack

import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Typed.Orphans ()

import           Hedgehog (MonadTest, Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- | Check the txOuts in a TxBodyContent after a ledger roundtrip.
prop_roundtrip_txbodycontent_txouts :: forall era. ShelleyBasedEra era -> Property
prop_roundtrip_txbodycontent_txouts era = H.property $ do
  (body, content :: TxBodyContent BuildTx era) <-
    shelleyBasedEraConstraints era $ H.forAll $ genValidTxBody era
  -- Convert ledger body back via 'getTxBodyContent' and 'fromLedgerTxBody'
  let (TxBody content') = body
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

  -- NOTE: We accept TxOutDatumInTx instead of TxOutDatumHash as it may be
  -- correctly resolved given a datum matching the hash was generated.
  matchDatum :: MonadTest m => (TxOutDatum CtxTx era, TxOutDatum CtxTx era) -> m ()
  matchDatum = \case
    (TxOutDatumHash _ dh, TxOutDatumInTx _ d) ->
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
  let (TxBody content') = body

  let proposals = getProposalProcedures . unFeatured <$> txProposalProcedures content
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
  getProposalProcedures txpp@(TxProposalProcedures _ _) = Just . toList $ convProposalProcedures txpp

-- | Test that the fee is the same when spending minted asset manually or when autobalancing it
prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset :: Property
prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset = H.propertyOnce $ do
  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
  aeo <- H.nothingFail $ forEraMaybeEon @AlonzoEraOnwards era

  systemStart <-
    fmap SystemStart . H.evalIO $
      DT.parseTimeM True DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2021-09-01T00:00:00Z"

  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters @ConwayEra
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  plutusWitness <- loadPlutusWitness

  let scriptHashStr = "e2b715a86bee4f14fef84081217f9e2646893a7d60a38af69e0aa572"
  let policyId' = fromString scriptHashStr
  let scriptHash = L.ScriptHash $ fromString scriptHashStr
  -- one UTXO with an asset - the same we're minting in the transaction
  let utxos =
        UTxO
          [
            ( TxIn
                "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
                (TxIx 0)
            , TxOut
                ( AddressInEra
                    (ShelleyAddressInEra ShelleyBasedEraConway)
                    ( ShelleyAddress
                        L.Testnet
                        ( L.KeyHashObj $
                            L.KeyHash "ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137"
                        )
                        L.StakeRefNull
                    )
                )
                ( TxOutValueShelleyBased
                    ShelleyBasedEraConway
                    ( L.MaryValue
                        (L.Coin 4_000_000)
                        (L.MultiAsset [(L.PolicyID scriptHash, [(L.AssetName "eeee", 1)])])
                    )
                )
                TxOutDatumNone
                ReferenceScriptNone
            )
          ]

      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsCollateral = TxInsCollateral aeo $ toList . M.keys . unUTxO $ utxos

  let address =
        AddressInEra
          (ShelleyAddressInEra ShelleyBasedEraConway)
          ( ShelleyAddress
              L.Testnet
              (L.ScriptHashObj scriptHash)
              L.StakeRefNull
          )
  let txOutputs doesIncludeAsset =
        [ TxOut
            address
            ( TxOutValueShelleyBased
                ShelleyBasedEraConway
                ( L.MaryValue
                    (L.Coin 2_000_000)
                    ( L.MultiAsset $
                        if doesIncludeAsset
                          then [(L.PolicyID scriptHash, [(L.AssetName "eeee", 2)])]
                          else []
                    )
                )
            )
            TxOutDatumNone
            ReferenceScriptNone
        ]

  let txMint =
        TxMintValue
          MaryEraOnwardsConway
          [(AssetId policyId' "eeee", 1)]
          (BuildTxWith [(policyId', plutusWitness)])

  -- tx body content without an asset in TxOut
  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxInsCollateral txInputsCollateral
          & setTxOuts (txOutputs False) -- include minted asset in txout manually
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

  -- tx body content with manually added asset to TxOut
  let contentWithTxoutAsset = content & setTxOuts (txOutputs True)

  -- change txout only with ADA
  (BalancedTxBody balancedContentWithTxoutAsset _ _ feeWithTxoutAsset) <-
    H.leftFail $
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        epochInfo
        pparams
        mempty
        mempty
        mempty
        utxos
        contentWithTxoutAsset
        address
        Nothing
  -- the correct amount with manual balancing of assets
  335_729 === feeWithTxoutAsset

  -- autobalanced body has assets and ADA in the change txout
  (BalancedTxBody balancedContent _ _ fee) <-
    H.leftFail $
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        epochInfo
        pparams
        mempty
        mempty
        mempty
        utxos
        content
        address
        Nothing

  H.noteShow_ feeWithTxoutAsset
  H.noteShow_ fee
  H.note_ "There are differences between fees for two autobalanced TxBodyContents. Diff:"
  H.diff balancedContentWithTxoutAsset (\_ _ -> feeWithTxoutAsset == fee) balancedContent
  feeWithTxoutAsset === fee
 where
  loadPlutusWitness
    :: HasCallStack
    => MonadFail m
    => MonadIO m
    => MonadTest m
    => m (ScriptWitness WitCtxMint ConwayEra)
  loadPlutusWitness = do
    envelope <-
      H.leftFailM $
        fmap (deserialiseFromJSON AsTextEnvelope) . H.evalIO $
          B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
    ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) (PlutusScript PlutusScriptV3 script) <-
      H.leftFail $ deserialiseFromTextEnvelopeAnyOf textEnvTypes envelope
    pure $
      PlutusScriptWitness
        PlutusScriptV3InConway
        PlutusScriptV3
        (PScript script)
        NoScriptDatumForMint
        (unsafeHashableScriptData (ScriptDataMap []))
        (ExecutionUnits 0 0)

  textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
  textEnvTypes =
    [ FromSomeType
        (AsScript AsPlutusScriptV3)
        (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3))
    ]

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
        "makeTransactionBodyAutoBalance test correct fees when mutli-asset tx"
        prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset
    ]
