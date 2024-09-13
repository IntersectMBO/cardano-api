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

import           Cardano.Api
import           Cardano.Api.Fees
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Ledger.Lens as L
import           Cardano.Api.Script
import           Cardano.Api.Shelley (LedgerProtocolParameters (..))

import qualified Cardano.Ledger.Alonzo.Core as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Mary.Value as L
import           Cardano.Ledger.Val ((<->))
import qualified Cardano.Ledger.Val as L
import qualified Cardano.Slotting.EpochInfo as CS
import qualified Cardano.Slotting.Slot as CS
import qualified Cardano.Slotting.Time as CS

import qualified Data.ByteString as B
import           Data.Default (def)
import           Data.Function
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ratio ((%))
import qualified Data.Time.Format as DT
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Lens.Micro ((^.))

import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (MonadTest, Property, forAll, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- | Test that the fee is the same when spending minted asset manually or when autobalancing it
prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset :: Property
prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = inject ceo
      meo = inject beo
      sbe = inject ceo
      era = toCardanoEra sbe
  aeo <- H.nothingFail $ forEraMaybeEon @AlonzoEraOnwards era

  systemStart <-
    fmap SystemStart . H.evalIO $
      DT.parseTimeM True DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2021-09-01T00:00:00Z"

  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  (sh@(ScriptHash scriptHash), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
  -- one UTXO with an asset - the same we're minting in the transaction
  let utxos = mkUtxos beo scriptHash
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsCollateral = TxInsCollateral aeo $ toList . M.keys . unUTxO $ utxos
  let address = mkAddress sbe scriptHash

  let txMint =
        TxMintValue
          meo
          [(AssetId policyId' "eeee", 1)]
          (BuildTxWith [(policyId', plutusWitness)])

  -- tx body content without an asset in TxOut
  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxInsCollateral txInputsCollateral
          & setTxOuts (mkTxOutput beo address Nothing) -- include minted asset in txout manually
          & setTxMintValue txMint
          & setTxProtocolParams (pure $ pure pparams)

  -- tx body content with manually added asset to TxOut
  let contentWithTxoutAsset = content & setTxOuts (mkTxOutput beo address (Just scriptHash))

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
  335_475 === feeWithTxoutAsset

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

prop_make_transaction_body_autobalance_multi_asset_collateral :: Property
prop_make_transaction_body_autobalance_multi_asset_collateral = H.propertyOnce $ do
  let ceo = ConwayEraOnwardsConway
      beo = inject ceo
      sbe = inject beo
      meo = inject beo
      era = toCardanoEra sbe
  aeo <- H.nothingFail $ forEraMaybeEon @AlonzoEraOnwards era

  systemStart <-
    fmap SystemStart . H.evalIO $
      DT.parseTimeM True DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" "2021-09-01T00:00:00Z"

  let epochInfo = LedgerEpochInfo $ CS.fixedEpochInfo (CS.EpochSize 100) (CS.mkSlotLength 1000)

  pparams <-
    LedgerProtocolParameters
      <$> H.readJsonFileOk "test/cardano-api-test/files/input/protocol-parameters/conway.json"

  (sh@(ScriptHash scriptHash), plutusWitness) <- loadPlutusWitness ceo
  let policyId' = PolicyId sh
  -- one UTXO with an asset - the same we're minting in the transaction
  let utxos = mkUtxos beo scriptHash
      txInputs = map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . toList . M.keys . unUTxO $ utxos
      txInputsCollateral = TxInsCollateral aeo $ toList . M.keys . unUTxO $ utxos
  let address = mkAddress sbe scriptHash
  let txMint =
        TxMintValue
          meo
          [(AssetId policyId' "eeee", 1)]
          (BuildTxWith [(policyId', plutusWitness)])

  let content =
        defaultTxBodyContent sbe
          & setTxIns txInputs
          & setTxInsCollateral txInputsCollateral
          & setTxOuts (mkTxOutput beo address Nothing)
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

  335_475 === fee
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
      sbe = inject beo
      era = inject beo
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

mkUtxos :: BabbageEraOnwards era -> L.ScriptHash L.StandardCrypto -> UTxO era
mkUtxos beo scriptHash = babbageEraOnwardsConstraints beo $ do
  let sbe = inject beo
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
                  (L.MultiAsset [(L.PolicyID scriptHash, [(L.AssetName "eeee", 1)])])
              )
          )
          TxOutDatumNone
          ReferenceScriptNone
      )
    ]

-- | Make an address from a script hash
mkAddress :: ShelleyBasedEra era -> L.ScriptHash L.StandardCrypto -> AddressInEra era
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
  -> Maybe (L.ScriptHash L.StandardCrypto)
  -- ^ there will be an asset in the txout if provided
  -> [TxOut CtxTx era]
mkTxOutput beo address mScriptHash = babbageEraOnwardsConstraints beo $ do
  let sbe = inject beo
  [ TxOut
      address
      ( TxOutValueShelleyBased
          sbe
          ( L.MaryValue
              (L.Coin 2_000_000)
              ( L.MultiAsset $
                  fromList
                    [(L.PolicyID scriptHash, [(L.AssetName "eeee", 2)]) | scriptHash <- maybeToList mScriptHash]
              )
          )
      )
      TxOutDatumNone
      ReferenceScriptNone
    ]

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
    , testProperty "calcReturnAndTotalCollateral constraints hold" prop_calcReturnAndTotalCollateral
    ]
