{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Experimental.Collateral.MissingInputs (tests) where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as P
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Slotting.EpochInfo qualified as Slotting
import Cardano.Slotting.Slot qualified as Slotting
import Cardano.Slotting.Time qualified as Slotting

import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Time
import GHC.Exts (fromList)
import Lens.Micro

import Test.Gen.Cardano.Api.Typed (genSigningKey)

import Test.Cardano.Api.Experimental (exampleProtocolParamsEra)
import Test.Cardano.Api.Transaction.Fixtures (mkTxIn)

import Hedgehog (Gen, Property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_conway_missing_collateral :: Property
prop_conway_missing_collateral = H.withTests 1 $ H.property $ do
  envelope <- H.evalIO $ BS.readFile "test/cardano-api-test/files/input/plutus/v3.alwaysTrue.json"
  Exp.AnyPlutusScript script <- H.evalEither $ Exp.readAnyScriptBytes Exp.ConwayEra envelope
  let scriptHash = P.hashPlutusScriptInEra script
      witness =
        Exp.AnyScriptWitnessPlutus
          $ Exp.AnyPlutusMintingScriptWitness
          $ Exp.PlutusScriptWitness
            (P.plutusScriptInEraSLanguage script)
            (Exp.PScript script)
            Exp.NoScriptDatum
            (Api.unsafeHashableScriptData $ Api.ScriptDataNumber 42)
            (Api.ExecutionUnits 0 0)
      body =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(testTxIn 0, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxMintValue
            ( Exp.TxMintValue $
                Map.singleton
                  (Api.PolicyId $ Api.ScriptHash scriptHash)
                  (fromList [(Api.UnsafeAssetName "token", 1)], witness)
            )
      params = exampleProtocolParamsEra Exp.ConwayEra
      address =
        Api.fromShelleyAddr Api.ShelleyBasedEraConway $
          L.Addr
            L.Testnet
            (L.KeyHashObj $ L.KeyHash "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5")
            L.StakeRefNull
  H.annotate "Expected MissingCollateralInputs for minting witness 0"
  let autoBalanceResult =
        Exp.makeTransactionBodyAutoBalance
          systemStart
          epochInfo
          params
          mempty
          mempty
          mempty
          body
          address
          Nothing
  case autoBalanceResult of
    Left (Exp.TxBodyErrorCollateral (Api.MissingCollateralInputs indices subs)) -> do
      indices === [Api.ScriptWitnessIndexMint 0]
      subs === Map.empty
    unexpected -> do
      H.annotateShow $ void unexpected
      H.failure
  let feeEstimationResult =
        Exp.estimateBalancedTxBody
          Exp.ConwayEra
          body
          params
          mempty
          mempty
          mempty
          (L.Coin 0)
          1
          0
          0
          address
          mempty
  case feeEstimationResult of
    Left
      ( Exp.TxFeeEstimationBalanceError
          (Exp.TxBodyErrorCollateral (Api.MissingCollateralInputs indices subs))
        ) -> do
        indices === [Api.ScriptWitnessIndexMint 0]
        subs === Map.empty
    unexpected -> do
      H.annotateShow $ void unexpected
      H.failure

-- A transaction needs collateral when it has Plutus witnesses at the top level,
-- in its sub-transactions, or both. Check that the error identifies every Plutus
-- witness at the correct level and excludes native scripts, which need no collateral.
prop_dijkstra_missing_collateral :: Property
prop_dijkstra_missing_collateral = H.property $ do
  -- The same error should be reported whether the Plutus script is included in
  -- the witness or supplied by a reference input. Exercise both forms.
  source <- H.forAll genScriptSource
  H.cover 20 "inline script" (source == InlineScript)
  H.cover 20 "reference script" (source == ReferenceScript)
  -- The top-level body has guards but no attached sub-transactions.
  (top, subs, _, address, _) <-
    H.forAllWith (\(_, _, _, addr, _) -> show addr) $ genDijkstraTransactions source
  -- The generated top-level body and each of the two Plutus sub-transactions
  -- have a native guard at index 0 followed by a Plutus guard at index 1.
  -- Guard indices count both kinds of script, so only index 1 belongs in the error.
  let indices = [Api.ScriptWitnessIndexGuarding 1]
      -- The error stores top-level indices in a list and sub-transaction indices
      -- in a map keyed by transaction ID. Both sub-transactions must be present,
      -- even though they use the same script and guard index.
      subIndices = Map.fromList [(Exp.getSignedSubTxId tx, indices) | tx <- subs]
      -- Remove its guards and attach the two sub-transactions to isolate the
      -- case where only sub-transactions require collateral.
      onlySubs = top & Exp.setTxGuards Exp.emptyTxGuards & Exp.setTxSignedSubTransactions subs
  -- None of these bodies has collateral inputs. Vary where the Plutus witnesses
  -- occur: the top-level body alone, the sub-transactions alone, then both.
  -- Compare the entire error so missing witnesses and extra entries both fail.
  forM_
    [ ("top-level", top, Api.MissingCollateralInputs indices Map.empty)
    , ("sub-transactions", onlySubs, Api.MissingCollateralInputs [] subIndices)
    ,
      ( "both levels"
      , top & Exp.setTxSignedSubTransactions subs
      , Api.MissingCollateralInputs indices subIndices
      )
    ]
    $ \(label, body, expected) -> do
      H.annotate label
      H.annotate $ "Expected collateral error: " <> show expected
      -- Auto-balancing must reject missing collateral before resolving inputs or
      -- evaluating scripts. Deliberately pass an empty UTxO: an input-resolution
      -- error here would mean that the collateral check happened too late.
      let autoBalanceResult =
            Exp.makeTransactionBodyAutoBalance
              systemStart
              epochInfo
              dijkstraParams
              mempty
              mempty
              mempty
              body
              address
              Nothing
      case autoBalanceResult of
        Left (Exp.TxBodyErrorCollateral actual@Api.MissingCollateralInputs{}) -> actual === expected
        unexpected -> do
          H.annotateShow $ void unexpected
          H.failure
      -- Fee estimation is a separate entry point and must report the same witness
      -- locations, wrapped in its own error type. The zero balances are deliberate:
      -- missing collateral must be reported before any balance or fee error.
      let feeEstimationResult =
            Exp.estimateBalancedTxBody
              Exp.DijkstraEra
              body
              dijkstraParams
              mempty
              mempty
              mempty
              (L.Coin 0)
              1
              0
              0
              address
              mempty
      case feeEstimationResult of
        Left
          (Exp.TxFeeEstimationBalanceError (Exp.TxBodyErrorCollateral actual@Api.MissingCollateralInputs{})) -> actual === expected
        unexpected -> do
          H.annotateShow $ void unexpected
          H.failure

-- Plutus sub-transactions balance with collateral; native sub-transactions
-- balance without it. These controls use the real inputs and UTxO.
prop_dijkstra_collateral_supplied :: Property
prop_dijkstra_collateral_supplied = H.property $ do
  source <- H.forAll genScriptSource
  H.cover 20 "inline script" (source == InlineScript)
  H.cover 20 "reference script" (source == ReferenceScript)
  (top, plutusSubs, nativeSub, address, utxo) <-
    H.forAllWith (\(_, _, _, addr, _) -> show addr) $ genDijkstraTransactions source
  let base = top & Exp.setTxGuards Exp.emptyTxGuards
      plutusBody =
        base
          & Exp.setTxSignedSubTransactions plutusSubs
          & Exp.setTxInsCollateral [testTxIn 3]
      nativeBody = base & Exp.setTxSignedSubTransactions [nativeSub]
  forM_
    [ ("Plutus sub-transactions with collateral", plutusBody)
    , ("native sub-transaction without collateral", nativeBody)
    ]
    $ \(label, body) -> do
      H.annotate label
      void
        $ H.evalEither
        $ Exp.makeTransactionBodyAutoBalance
          systemStart
          epochInfo
          dijkstraParams
          mempty
          mempty
          utxo
          body
          address
          Nothing
      let totalValue = L.MaryValue (L.Coin $ 100_000_000 * (1 + toInteger (length $ Exp.txSubTransactions body))) mempty
      void
        $ H.evalEither
        $ Exp.estimateBalancedTxBody
          Exp.DijkstraEra
          body
          dijkstraParams
          mempty
          mempty
          mempty
          (L.Coin 100_000_000)
          1
          0
          0
          address
          totalValue

data ScriptSource = InlineScript | ReferenceScript
  deriving (Eq, Show)

genScriptSource :: Gen ScriptSource
genScriptSource = Gen.element [InlineScript, ReferenceScript]

-- Generate a top-level body, two Plutus sub-transactions, a native sub-transaction,
-- and their address and UTxO. Each spending input holds 100 ada. Sub-transactions
-- return their full input value, leaving the top-level input to pay fees and provide change.
genDijkstraTransactions
  :: ScriptSource
  -> Gen
       ( Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
       , [Exp.SignedSubTx]
       , Exp.SignedSubTx
       , Api.AddressInEra Api.DijkstraEra
       , L.UTxO (Exp.LedgerEra Exp.DijkstraEra)
       )
genDijkstraTransactions source = do
  key <- genSigningKey Api.AsPaymentKey
  script <-
    either (error . show) pure
      $ P.deserialisePlutusScriptInEra @(Exp.LedgerEra Exp.DijkstraEra) @Plutus.PlutusV4
      $ Base16.decodeLenient "46450101002499"
  let address =
        Api.makeShelleyAddressInEra
          Api.ShelleyBasedEraDijkstra
          (Api.Testnet $ Api.NetworkMagic 42)
          (Api.PaymentCredentialByKey $ Api.verificationKeyHash $ Api.getVerificationKey key)
          Api.NoStakeAddress
      output = L.mkBasicTxOut (Api.toShelleyAddr address) (L.MaryValue (L.Coin 100_000_000) mempty)
      native = Exp.SimpleScript $ Api.toDijkstraNativeScript $ Api.RequireAllOf []
      nativeEntry =
        (L.ScriptHashObj $ Exp.hashSimpleScript native, Just $ Exp.NativeGuardWitness $ Exp.SScript native)
      plutusEntry =
        ( L.ScriptHashObj $ P.hashPlutusScriptInEra script
        , Just
            $ Exp.PlutusGuardWitness
            $ Exp.PlutusScriptWitness
              Plutus.SPlutusV4
              ( case source of
                  InlineScript -> Exp.PScript script
                  ReferenceScript -> Exp.PReferenceScript (testTxIn 4)
              )
              Exp.NoScriptDatum
              (Api.unsafeHashableScriptData $ Api.ScriptDataNumber 42)
              (Api.ExecutionUnits 1_000_000 100_000)
        )
      references = Exp.TxInsReference [testTxIn 4 | source == ReferenceScript] Set.empty
  nativeGuards <- either (error . show) pure $ Exp.mkTxGuards [nativeEntry]
  guards <- either (error . show) pure $ Exp.mkTxGuards [nativeEntry, plutusEntry]
  let signedSub guards' txIn =
        either (error . show) pure
          $ Exp.makeSignedSubTx [Api.WitnessPaymentKey key]
          $ Exp.defaultSubTxBodyContent
            & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxOuts [Exp.TxOut output]
            & Exp.setTxGuards guards'
            & Exp.setTxInsReference references
            & Exp.setTxProtocolParams dijkstraParams
  subs <- traverse (signedSub guards . testTxIn) [1, 2]
  nativeSub <- signedSub nativeGuards (testTxIn 5)
  let top =
        Exp.defaultTxBodyContent
          & Exp.setTxIns [(testTxIn 0, Exp.AnyKeyWitnessPlaceholder)]
          & Exp.setTxGuards guards
          & Exp.setTxInsReference references
          & Exp.setTxProtocolParams dijkstraParams
      utxo =
        L.UTxO
          $ Map.insert
            (Api.toShelleyTxIn $ testTxIn 4)
            (output & L.referenceScriptTxOutL .~ L.SJust (P.plutusScriptInEraToScript script))
          $ Map.fromList [(Api.toShelleyTxIn $ testTxIn n, output) | n <- [0 .. 5]]
  pure (top, subs, nativeSub, address, utxo)

testTxIn :: Word -> Api.TxIn
testTxIn n =
  mkTxIn $ "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53#" <> Text.pack (show n)

dijkstraParams :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
dijkstraParams =
  exampleProtocolParamsEra Exp.DijkstraEra
    & L.ppProtocolVersionL .~ L.ProtVer (L.eraProtVerLow @(Exp.LedgerEra Exp.DijkstraEra)) 0

systemStart :: Api.SystemStart
systemStart = Api.SystemStart $ Time.posixSecondsToUTCTime 0

epochInfo :: Api.LedgerEpochInfo
epochInfo =
  Api.LedgerEpochInfo $ Slotting.fixedEpochInfo (Slotting.EpochSize 100) (Slotting.mkSlotLength 1000)

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental.Collateral.MissingInputs"
    [ testProperty "Conway minting reports missing collateral" prop_conway_missing_collateral
    , testProperty "Dijkstra reports every Plutus witness" prop_dijkstra_missing_collateral
    , testProperty "Dijkstra balances with the required collateral" prop_dijkstra_collateral_supplied
    ]
