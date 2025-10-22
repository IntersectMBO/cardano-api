{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Transaction.Autobalance
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Tx
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Tx qualified as L

import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Val ((<->))
import Cardano.Ledger.Val qualified as L
import Cardano.Slotting.EpochInfo qualified as CS
import Cardano.Slotting.Slot qualified as CS
import Cardano.Slotting.Time qualified as CS

import Control.Monad
import Control.Monad.Trans.Fail (errorFail)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.Default (def)
import Data.Function
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time.Format qualified as DT
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro ((^.))

import Test.Gen.Cardano.Api.Typed

import Test.Cardano.Api.Orphans ()

import Hedgehog (MonadTest, Property, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_make_transaction_body_autobalance_invariants :: Property
prop_make_transaction_body_autobalance_invariants = H.property $ do
  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  -- assume a value larger the one from protocol params to account for min utxo scaling with minted assets
  let minUtxo = 2_000_000

  -- generate utxos with random values
  utxos <- fmap (UTxO . fromList) . forAll $ do
    Gen.list (Range.constant 1 10) $ do
      txIn <- genTxIn
      addr <- genAddressInEra sbe
      utxoValue <- L.Coin <$> Gen.integral (Range.linear minUtxo 20_000_000)
      let mintValue = mempty -- TODO generate and check in invariants
          txOut =
            TxOut
              addr
              (TxOutValueShelleyBased sbe $ L.MaryValue utxoValue mintValue)
              TxOutDatumNone
              ReferenceScriptNone
      pure (txIn, txOut)

  let utxoSum =
        mconcat
          [ maryValue
          | (_, TxOut _ (TxOutValueShelleyBased _ maryValue) _ _) <- toList utxos
          ]
  H.noteShowPretty_ utxoSum

  -- split inputs into min utxo txouts
  let nTxOuts = L.unCoin (L.coin utxoSum) `div` minUtxo - 1 -- leave one out for change
  H.noteShow_ nTxOuts
  txOut <- forAll $ forM ([1 .. nTxOuts] :: [Integer]) $ \_ -> do
    addr <- genAddressInEra sbe
    let mintValue = mempty -- TODO generate and check in invariants
    pure $
      TxOut
        addr
        (TxOutValueShelleyBased sbe $ L.MaryValue (L.Coin minUtxo) mintValue)
        TxOutDatumNone
        ReferenceScriptNone

  changeAddress <- forAll $ genAddressInEra sbe

  -- use all UTXOs as inputs
  let txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos

  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxOuts txOut
          & setTxProtocolParams (pure $ pure pparams)

  (BalancedTxBody balancedContent _ change fee) <-
    H.leftFail . first prettyError $
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
        changeAddress
        Nothing

  H.note_ "Check that fee is greater than 0"
  H.assertWith (L.unCoin fee) $ (<) 0

  H.noteShow_ fee
  H.noteShowPretty_ change
  H.noteShowPretty_ $ txOuts balancedContent

  let txOutSum =
        mconcat
          [ maryValue
          | TxOut _ (TxOutValueShelleyBased _ maryValue) _ _ <- txOuts balancedContent
          ]

  H.note_ "Check that all inputs are spent"
  utxoSum === (txOutSum <> inject fee)

prop_make_transaction_body_autobalance_no_change :: Property
prop_make_transaction_body_autobalance_no_change = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  let expectedFee = 170_077
      utxoValue = 5_000_000

  let address =
        AddressInEra
          (ShelleyAddressInEra sbe)
          ( ShelleyAddress
              L.Testnet
              (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
              L.StakeRefNull
          )
  let utxos =
        UTxO
          [
            ( mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
            , TxOut
                address
                ( TxOutValueShelleyBased
                    sbe
                    (L.MaryValue utxoValue mempty)
                )
                TxOutDatumNone
                ReferenceScriptNone
            )
          ]

      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos

      -- tx out fully spending the txin minus the fee
      txOut =
        [ TxOut
            address
            ( TxOutValueShelleyBased
                sbe
                (L.MaryValue (utxoValue - expectedFee) mempty)
            )
            TxOutDatumNone
            ReferenceScriptNone
        ]

  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxOuts txOut
          & setTxProtocolParams (pure $ pure pparams)

  (BalancedTxBody balancedContent _ (TxOut _ (TxOutValueShelleyBased _ change) _ _) fee) <-
    H.leftFail . first prettyError $
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

  H.noteShowPretty_ change
  H.noteShowPretty_ $ txOuts balancedContent

  expectedFee === fee

  -- check that the txins were fully spent before autobalancing
  H.assertWith change L.isZero

-- | Test that the fee is the same when spending minted asset manually or when autobalancing it
prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset :: Property
prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      meo = convert beo
      sbe = convert ceo
      aeo = convert beo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  (sh@(ScriptHash scriptHash), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
  -- one UTXO with an asset - the same we're minting in the transaction
  let utxos = mkUtxos beo (Just scriptHash)
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsCollateral = TxInsCollateral aeo $ toList . M.keys . unUTxO $ utxos
  let address = mkAddress sbe scriptHash

  let txMint =
        TxMintValue
          meo
          [(policyId', ([(UnsafeAssetName "eeee", 1)], BuildTxWith plutusWitness))]

  -- tx body content without an asset in TxOut
  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxInsCollateral txInputsCollateral
          & setTxOuts (mkTxOutput beo address (L.Coin 2_000_000) Nothing) -- include minted asset in txout manually
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

  -- tx body content with manually added asset to TxOut
  let contentWithTxoutAsset = content & setTxOuts (mkTxOutput beo address (L.Coin 2_000_000) (Just scriptHash))

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

  scriptWitReqsWithAsset <-
    H.evalEither $ collectTxBodyScriptWitnessRequirements aeo balancedContentWithTxoutAsset

  -- check if execution units have changed
  [ ExecutionUnits
      { executionSteps = 84_851_308
      , executionMemory = 325_610
      }
    ]
    === extractExecutionUnits scriptWitReqsWithAsset

  -- the correct amount with manual balancing of assets
  335_299 === feeWithTxoutAsset

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

  scriptWitReqsBalanced <-
    H.evalEither $ collectTxBodyScriptWitnessRequirements aeo balancedContent

  -- check if execution units have changed
  [ ExecutionUnits
      { executionSteps = 84_851_308
      , executionMemory = 325_610
      }
    ]
    === extractExecutionUnits scriptWitReqsBalanced

  H.noteShow_ feeWithTxoutAsset
  H.noteShow_ fee
  H.note_ "There are differences between fees for two autobalanced TxBodyContents. Diff:"
  H.diff balancedContentWithTxoutAsset (\_ _ -> feeWithTxoutAsset == fee) balancedContent
  feeWithTxoutAsset === fee

prop_make_transaction_body_autobalance_when_deregistering_certs :: Property
prop_make_transaction_body_autobalance_when_deregistering_certs = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      sbe = convert beo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  (ScriptHash scriptHash, _) <- loadPlutusWitness ceo

  let utxos = mkUtxos beo Nothing
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsTotalCoin = mconcat $ getTxOutCoin =<< M.elems (unUTxO utxos)
      address = mkAddress sbe scriptHash
      deregDeposit = L.Coin 20_000_000
      txOutCoin = L.Coin 20_800_000

  -- that's the same stake credential as in UTXO
  lStakeCred@(L.KeyHashObj kh) <-
    pure $ mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137"
  let stakeCred = StakeCredentialByKey $ StakeKeyHash kh
  let certs =
        [
          ( ConwayCertificate ceo $
              L.ConwayTxCertDeleg (L.ConwayUnRegCert lStakeCred (L.SJust deregDeposit))
          , Nothing
          )
        ]

      content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxOuts (mkTxOutput beo address txOutCoin Nothing)
          & setTxProtocolParams (pure $ pure pparams)
          & setTxCertificates (mkTxCertificates sbe certs)

  -- autobalanced body has assets and ADA in the change txout
  (BalancedTxBody _ _ changeOut fee) <-
    H.leftFail $
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        epochInfo
        pparams
        mempty
        [(stakeCred, deregDeposit)]
        mempty
        utxos
        content
        address
        Nothing

  changeCoin <- getTxOutCoin changeOut

  H.note_ "Sanity check: inputs == outputs"
  mconcat [deregDeposit, txInputsTotalCoin] === mconcat [txOutCoin, fee, changeCoin]

  176_457 === fee

prop_make_transaction_body_autobalance_multi_asset_collateral :: Property
prop_make_transaction_body_autobalance_multi_asset_collateral = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = convert ceo
      sbe = convert beo
      meo = convert beo
      aeo = convert beo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  (sh@(ScriptHash scriptHash), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
  -- one UTXO with an asset - the same we're minting in the transaction
  let utxos = mkUtxos beo (Just scriptHash)
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsCollateral = TxInsCollateral aeo $ toList . M.keys . unUTxO $ utxos
  let address = mkAddress sbe scriptHash
  let txMint =
        TxMintValue
          meo
          [(policyId', ([(UnsafeAssetName "eeee", 1)], BuildTxWith plutusWitness))]

  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxInsCollateral txInputsCollateral
          & setTxOuts (mkTxOutput beo address (L.Coin 2_000_000) Nothing)
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

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

  scriptWitReqsBalanced <-
    H.evalEither $ collectTxBodyScriptWitnessRequirements aeo balancedContent

  -- check if execution units have changed
  [ ExecutionUnits
      { executionSteps = 84_851_308
      , executionMemory = 325_610
      }
    ]
    === extractExecutionUnits scriptWitReqsBalanced

  335_299 === fee
  TxReturnCollateral _ (TxOut _ txOutValue _ _) <- H.noteShow $ txReturnCollateral balancedContent
  let assets = [a | a@(AssetId _ _, _) <- toList $ txOutValueToValue txOutValue]
  H.note_ "Check that all assets from UTXO, from the collateral txin, are in the return collateral."
  [(AssetId policyId' $ UnsafeAssetName "eeee", 1)] === assets

-- | Implements collateral validation from Babbage spec, from
-- https://github.com/IntersectMBO/cardano-ledger/releases, babbage-ledger.pdf, Figure 2.
--
-- Seems that under 400 runs the test is not able to detect the violation of properties.
prop_calcReturnAndTotalCollateral :: Property
prop_calcReturnAndTotalCollateral = H.withTests 400 . H.property $ do
  let beo = BabbageEraOnwardsConway
      sbe = convert beo
      era = convert beo
  feeCoin@(L.Coin fee) <- forAll genLovelace
  totalCollateral <- forAll $ genLedgerValueForTxOut sbe
  let totalCollateralAda = totalCollateral ^. L.adaAssetL sbe
  pparams <-
    H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"
  requiredCollateralPct <- H.noteShow . fromIntegral $ pparams ^. L.ppCollateralPercentageL
  requiredCollateralAda <-
    H.noteShow . L.rationalToCoinViaCeiling $ (fee * requiredCollateralPct) % 100
  txInsColl <- forAll $ genTxInsCollateral era
  txRetColl <-
    forAll $ Gen.frequency [(4, pure TxReturnCollateralNone), (1, genTxReturnCollateral sbe)]
  txTotColl <- forAll $ Gen.frequency [(4, pure TxTotalCollateralNone), (1, genTxTotalCollateral era)]
  let address = AddressInEra (ShelleyAddressInEra sbe) (ShelleyAddress L.Testnet def L.StakeRefNull)

  let (resRetColl, resTotColl) =
        calcReturnAndTotalCollateral
          beo
          feeCoin
          pparams
          txInsColl
          txRetColl
          txTotColl
          address
          totalCollateral

  H.annotateShow resRetColl
  H.annotateShow resTotColl

  let resRetCollValue =
        mconcat
          [ txOutValue
          | TxReturnCollateral _ (TxOut _ (TxOutValueShelleyBased _ txOutValue) _ _) <- pure resRetColl
          ]
      collBalance = totalCollateral <-> resRetCollValue

  resTotCollValue <-
    H.noteShow $ mconcat [L.mkAdaValue sbe lovelace | TxTotalCollateral _ lovelace <- pure resTotColl]

  if
    | txInsColl == TxInsCollateralNone -> do
        -- no inputs - no outputs
        TxReturnCollateralNone === resRetColl
        TxTotalCollateralNone === resTotColl
    | txRetColl /= TxReturnCollateralNone || txTotColl /= TxTotalCollateralNone -> do
        -- got collateral values as function arguments - not calculating anything
        txRetColl === resRetColl
        txTotColl === resTotColl
    | totalCollateralAda < requiredCollateralAda -> do
        -- provided collateral not enough, not calculating anything
        TxReturnCollateralNone === resRetColl
        TxTotalCollateralNone === resTotColl
    | otherwise -> do
        -- no explicit collateral or return collateral was provided, we do the calculation
        H.annotateShow collBalance
        H.note_ "Check if collateral balance is positive"
        H.assertWith collBalance $ L.pointwise (<=) mempty
        H.note_ "Check if collateral balance contains only ada"
        H.assertWith collBalance L.isAdaOnly
        H.note_ "Check if collateral balance is at least minimum required"
        H.assertWith collBalance $ L.pointwise (<=) (L.inject requiredCollateralAda)
        H.note_ "Check that collateral balance is equal to collateral in tx body"
        resTotCollValue === collBalance

-- | Regression test for: https://github.com/IntersectMBO/cardano-cli/issues/1073
prop_ensure_gov_actions_are_preserved_by_autobalance :: Property
prop_ensure_gov_actions_are_preserved_by_autobalance = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo

  systemStart <- parseSystemStart "2021-09-01T00:00:00Z"
  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  -- One UTXO with 2000 ADA
  let utxos = mkSimpleUTxOs sbe
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
  address <- H.forAll (genAddressInEra sbe)

  anchorUrl <- H.evalEither $ eitherDecodeStrict "\"https://tinyurl.com/cardano-qa-anchor\""
  anchorDataHash <-
    H.evalEither $
      eitherDecodeStrict "\"f08cc9640136b1ae47428f646a9b5aadc0045fafb5529ca3ba1723784e6f0750\""
  let anchor =
        L.Anchor
          { L.anchorUrl = anchorUrl
          , L.anchorDataHash = anchorDataHash
          }
      proposalProcedure =
        L.ProposalProcedure
          { L.pProcDeposit = 100_000_000
          , L.pProcReturnAddr =
              L.RewardAccount
                { L.raNetwork = L.Testnet
                , L.raCredential = mkCredential "keyHash-0b1b872f7953bccfc4245f3282b3363f3d19e9e001a5c41e307363d7"
                }
          , L.pProcGovAction = L.InfoAction
          , L.pProcAnchor = anchor
          }

  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxProtocolParams (pure $ pure pparams)
          & setTxProposalProcedures
            ( pure $
                Featured
                  ConwayEraOnwardsConway
                  ( TxProposalProcedures
                      (fromList [(proposalProcedure, BuildTxWith Nothing)])
                  )
            )

  -- Autobalanced body should preserve the governance action
  (BalancedTxBody _ balancedTxBody _ _) <-
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

  let balancedContent = getTxBodyContent balancedTxBody
  Featured _ (TxProposalProcedures balancedProposalProcedureOMap) <-
    H.evalMaybe $ txProposalProcedures balancedContent
  let balancedProposalProcedureList = toList balancedProposalProcedureOMap
  balancedProposalProcedureList === [(proposalProcedure, ViewTx)]

-- * Utilities

mkSimpleUTxOs :: ShelleyBasedEra ConwayEra -> UTxO ConwayEra
mkSimpleUTxOs sbe =
  UTxO
    [
      ( mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      , TxOut
          ( AddressInEra
              (ShelleyAddressInEra sbe)
              ( ShelleyAddress
                  L.Testnet
                  (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
                  L.StakeRefNull
              )
          )
          ( lovelaceToTxOutValue
              sbe
              2_000_000_000
          )
          TxOutDatumNone
          ReferenceScriptNone
      )
    ]

loadPlutusWitness
  :: HasCallStack
  => MonadFail m
  => MonadIO m
  => MonadTest m
  => ConwayEraOnwards era
  -> m (ScriptHash, ScriptWitness WitCtxMint era)
loadPlutusWitness ceo = do
  envelope <-
    H.leftFailM $
      fmap (deserialiseFromJSON @TextEnvelope) . H.evalIO $
        B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) s@(PlutusScript PlutusScriptV3 script) <-
    H.leftFail $ deserialiseFromTextEnvelopeAnyOf textEnvTypes envelope
  let scriptLangInEra = case ceo of
        ConwayEraOnwardsConway -> PlutusScriptV3InConway
        ConwayEraOnwardsDijkstra -> PlutusScriptV3InDijkstra
  pure
    ( hashScript s
    , PlutusScriptWitness
        scriptLangInEra
        PlutusScriptV3
        (PScript script)
        NoScriptDatumForMint
        (unsafeHashableScriptData (ScriptDataMap []))
        (ExecutionUnits 0 0)
    )

textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
textEnvTypes =
  [ FromSomeType
      (AsScript AsPlutusScriptV3)
      (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3))
  ]

mkUtxos
  :: BabbageEraOnwards era
  -> Maybe L.ScriptHash
  -- ^ add an asset to the utxo if the script hash is provided
  -> UTxO era
mkUtxos beo mScriptHash = babbageEraOnwardsConstraints beo $ do
  let sbe = convert beo
  UTxO
    [
      ( mkTxIn "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#0"
      , TxOut
          ( AddressInEra
              (ShelleyAddressInEra sbe)
              ( ShelleyAddress
                  L.Testnet
                  (mkCredential "keyHash-ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137")
                  L.StakeRefNull
              )
          )
          ( TxOutValueShelleyBased
              sbe
              ( L.MaryValue
                  (L.Coin 4_000_000)
                  ( L.MultiAsset $
                      fromList
                        [(L.PolicyID scriptHash, [(L.AssetName "eeee", 1)]) | scriptHash <- maybeToList mScriptHash]
                  )
              )
          )
          TxOutDatumNone
          ReferenceScriptNone
      )
    ]

-- | Make an address from a script hash
mkAddress :: ShelleyBasedEra era -> L.ScriptHash -> AddressInEra era
mkAddress sbe scriptHash =
  AddressInEra
    (ShelleyAddressInEra sbe)
    ( ShelleyAddress
        L.Testnet
        (L.ScriptHashObj scriptHash)
        L.StakeRefNull
    )

-- | Make a single txout with an optional asset
mkTxOutput
  :: BabbageEraOnwards era
  -> AddressInEra era
  -> L.Coin
  -- ^ output ADA
  -> Maybe L.ScriptHash
  -- ^ there will be an asset in the txout if provided
  -> [TxOut CtxTx era]
mkTxOutput beo address coin mScriptHash = babbageEraOnwardsConstraints beo $ do
  let sbe = convert beo
  [ TxOut
      address
      ( TxOutValueShelleyBased
          sbe
          ( L.MaryValue
              coin
              ( L.MultiAsset $
                  fromList
                    [(L.PolicyID scriptHash, [(L.AssetName "eeee", 2)]) | scriptHash <- maybeToList mScriptHash]
              )
          )
      )
      TxOutDatumNone
      ReferenceScriptNone
    ]

parseSystemStart :: (HasCallStack, MonadTest m, MonadIO m) => String -> m SystemStart
parseSystemStart timeString =
  withFrozenCallStack $
    fmap SystemStart . H.evalIO $
      DT.parseTimeM True DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" timeString

getTxOutCoin
  :: forall era ctx m
   . (HasCallStack, MonadFail m, IsMaryBasedEra era)
  => TxOut ctx era
  -> m L.Coin
getTxOutCoin txout = withFrozenCallStack $ maryEraOnwardsConstraints (maryBasedEra @era) $ do
  TxOut _ (TxOutValueShelleyBased _ (L.MaryValue changeCoin _)) _ _ <- pure txout
  pure changeCoin

mkCredential :: HasCallStack => Text -> L.Credential k
mkCredential = errorFail @String . L.parseCredential

mkTxIn :: HasCallStack => Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.TxBody"
    [ testProperty
        "makeTransactionBodyAutoBalance invariants"
        prop_make_transaction_body_autobalance_invariants
    , testProperty
        "makeTransactionBodyAutoBalance no change"
        prop_make_transaction_body_autobalance_no_change
    , testProperty
        "makeTransactionBodyAutoBalance test correct fees when mutli-asset tx"
        prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset
    , testProperty
        "makeTransactionBodyAutoBalance autobalances multi-asset collateral"
        prop_make_transaction_body_autobalance_multi_asset_collateral
    , testProperty
        "makeTransactionBodyAutoBalance autobalances when deregistering certificates"
        prop_make_transaction_body_autobalance_when_deregistering_certs
    , testProperty "calcReturnAndTotalCollateral constraints hold" prop_calcReturnAndTotalCollateral
    , testProperty
        "Governance actions are preserved by autobalance"
        prop_ensure_gov_actions_are_preserved_by_autobalance
    ]
