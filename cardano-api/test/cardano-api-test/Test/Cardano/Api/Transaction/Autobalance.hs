{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use list comprehension" -}
{- HLINT ignore "Use camelCase" -}

module Test.Cardano.Api.Transaction.Autobalance
  ( tests
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Script
import           Cardano.Api.Shelley (Address (..), LedgerProtocolParameters (..))

import qualified Cardano.Ledger.Mary.Value as L
import qualified Cardano.Ledger.Shelley.Scripts as L
import qualified Cardano.Slotting.EpochInfo as CS
import qualified Cardano.Slotting.Slot as CS
import qualified Cardano.Slotting.Time as CS

import qualified Data.ByteString as B
import           Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Time.Format as DT
import           GHC.Exts (IsList (..), IsString (..))
import           GHC.Stack

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (MonadTest, Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

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
    [ testProperty
        "makeTransactionBodyAutoBalance test correct fees when mutli-asset tx"
        prop_make_transaction_body_autobalance_return_correct_fee_for_multi_asset
    ]
