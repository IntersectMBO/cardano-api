{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Test.Cardano.Api.Transaction.Autobalance
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Internal.Address (toShelleyStakeCredential)
import Cardano.Api.Internal.Fees
import Cardano.Api.Internal.Script
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger.Lens qualified as L
import Cardano.Api.Shelley (LedgerProtocolParameters (..))

import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Val ((<->))
import Cardano.Ledger.Val qualified as L
import Cardano.Slotting.EpochInfo qualified as CS
import Cardano.Slotting.Slot qualified as CS
import Cardano.Slotting.Time qualified as CS

import Data.ByteString qualified as B
import Data.Default (def)
import Data.Function
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ratio ((%))
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

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
          [(policyId', [("eeee", 1, BuildTxWith plutusWitness)])]

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

  stakeCred <- forAll genStakeCredential
  let certs =
        [
          ( ConwayCertificate ceo $
              L.ConwayTxCertDeleg (L.ConwayUnRegCert (toShelleyStakeCredential stakeCred) (L.SJust deregDeposit))
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

  180_901 === fee

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
          [(policyId', [("eeee", 1, BuildTxWith plutusWitness)])]

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

  335_299 === fee
  TxReturnCollateral _ (TxOut _ txOutValue _ _) <- H.noteShow $ txReturnCollateral balancedContent
  let assets = [a | a@(AssetId _ _, _) <- toList $ txOutValueToValue txOutValue]
  H.note_ "Check that all assets from UTXO, from the collateral txin, are in the return collateral."
  [(AssetId policyId' "eeee", 1)] === assets

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
  totalCollateral <- forAll $ genValueForTxOut sbe
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

-- * Utilities

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
      fmap (deserialiseFromJSON AsTextEnvelope) . H.evalIO $
        B.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) s@(PlutusScript PlutusScriptV3 script) <-
    H.leftFail $ deserialiseFromTextEnvelopeAnyOf textEnvTypes envelope
  let scriptLangInEra = case ceo of
        ConwayEraOnwardsConway -> PlutusScriptV3InConway
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
      ( TxIn
          "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
          (TxIx 0)
      , TxOut
          ( AddressInEra
              (ShelleyAddressInEra sbe)
              ( ShelleyAddress
                  L.Testnet
                  ( L.KeyHashObj $
                      L.KeyHash "ebe9de78a37f84cc819c0669791aa0474d4f0a764e54b9f90cfe2137"
                  )
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

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.TxBody"
    [ testProperty
        "makeTransactionBodyAutoBalance test correct fees when mutli-asset tx"
        prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset
    , testProperty
        "makeTransactionBodyAutoBalance autobalances multi-asset collateral"
        prop_make_transaction_body_autobalance_multi_asset_collateral
    , testProperty
        "makeTransactionBodyAutoBalance autobalances when deregistering certificates"
        prop_make_transaction_body_autobalance_when_deregistering_certs
    , testProperty "calcReturnAndTotalCollateral constraints hold" prop_calcReturnAndTotalCollateral
    ]
