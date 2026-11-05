{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Generators for Plutus V4 script guard balancing tests.
-- Generate funding, collateral and reference-script outputs at 'testAddress'.
-- Plutus V4 script guards apply to the transaction or sub-transaction, not that address.
--
-- 'genPlutusGuardsWithScriptWitnesses' supplies the Plutus V4 script directly;
-- 'genPlutusGuardsWithReferenceScripts' supplies a reference to its UTxO output.
-- Both return 'Exp.TxGuards' and the generated redeemer for assertions.
-- Tests add the reference input when needed and choose the execution budget.
--
-- 'genLegacyWithdrawal' generates a zero-valued Plutus V3 withdrawal to test
-- language restrictions without adding funds.
module Test.Gen.Cardano.Api.Experimental.Guard
  ( PlutusV4ReferenceScriptGuardTestInputs (..)
  , genPlutusV4ReferenceScriptGuardTestInputs
  , alwaysSucceedsGuardScript
  , genPlutusGuardsWithScriptWitnesses
  , genPlutusGuardsWithReferenceScripts
  , genLegacyWithdrawal
  , GuardScriptSource (..)
  , ScriptBalanceCase (..)
  , genScriptBalanceCase
  , genTopLevelGuardCase
  , genFailingTopLevelGuardCase
  , genLegacyTopLevelBalanceTestData
  , genLegacyPoolRegistrationBalanceTestData
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
  ( AnyPlutusScriptWitness (AnyPlutusWithdrawingScriptWitness)
  )
import Cardano.Api.Experimental.Plutus (plutusScriptInEraToScript)
import Cardano.Api.Experimental.Plutus qualified as P
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Monad.Error (failEitherWith)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Val qualified as L (invert)

import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lens.Micro

import Test.Gen.Cardano.Api.Experimental (genSignedSubTransaction)
import Test.Gen.Cardano.Api.Experimental.SubTransaction
  ( SubTransactionBalanceTestData (..)
  , SubTxAmounts (..)
  , TopLevelTxAmounts (..)
  , TxAmounts (..)
  , genSubTransactionBalanceTestData
  )
import Test.Gen.Cardano.Api.Typed (genAddressInEra, genHashableScriptData, genTxIn)

import Hedgehog (Gen)
import Hedgehog.Gen.QuickCheck qualified as Q

-- | Funding, collateral and reference-script outputs for Plutus V4 script guard tests.
data PlutusV4ReferenceScriptGuardTestInputs = PlutusV4ReferenceScriptGuardTestInputs
  { guardFundingInput :: Api.TxIn
  -- ^ Input available to fund either the top-level transaction or a sub-transaction.
  , guardCollateralInput :: Api.TxIn
  -- ^ Separate input available as collateral for the top-level transaction.
  , guardReferenceInput :: Api.TxIn
  -- ^ Input whose output carries the script supplied to the generator.
  , testAddress :: Api.AddressInEra Api.DijkstraEra
  -- ^ Output and change address; Plutus V4 script guards apply to transactions, not this address.
  , guardUTxO :: L.UTxO (Exp.LedgerEra Exp.DijkstraEra)
  -- ^ Resolved outputs for all three inputs, each containing 100 ada.
  }
  deriving Show

-- | Generate three 100-ada outputs; only the reference output carries the Plutus V4 script.
genPlutusV4ReferenceScriptGuardTestInputs
  :: P.PlutusScriptInEra Plutus.PlutusV4 (Exp.LedgerEra Exp.DijkstraEra)
  -> Gen PlutusV4ReferenceScriptGuardTestInputs
genPlutusV4ReferenceScriptGuardTestInputs script = do
  input <- genTxIn
  let Api.TxIn inputId (Api.TxIx inputIx) = input
      collateral = Api.TxIn inputId (Api.TxIx (inputIx + 1))
      reference = Api.TxIn inputId (Api.TxIx (inputIx + 2))
  address <- genAddressInEra Api.ShelleyBasedEraDijkstra
  let funding = L.mkBasicTxOut (Api.toShelleyAddr address) (L.MaryValue (L.Coin 100_000_000) mempty)
      utxo =
        L.UTxO $
          Map.fromList
            [ (Api.toShelleyTxIn input, funding)
            , (Api.toShelleyTxIn collateral, funding)
            ,
              ( Api.toShelleyTxIn reference
              , funding & L.referenceScriptTxOutL .~ L.SJust (plutusScriptInEraToScript script)
              )
            ]
  pure $ PlutusV4ReferenceScriptGuardTestInputs input collateral reference address utxo

-- | Decode the always-succeeding Plutus V4 script for script guard tests.
alwaysSucceedsGuardScript
  :: Either CBOR.DecoderError (P.PlutusScriptInEra Plutus.PlutusV4 (Exp.LedgerEra Exp.DijkstraEra))
alwaysSucceedsGuardScript =
  P.deserialisePlutusScriptInEra @(Exp.LedgerEra Exp.DijkstraEra) @Plutus.PlutusV4 $
    Base16.decodeLenient "46450101002499"

-- | Generate a Plutus V4 script guard and return its redeemer for assertions.
-- The 'Exp.PlutusGuardWitness' supplies the script through 'Exp.PScript'.
genPlutusGuardsWithScriptWitnesses
  :: P.PlutusScriptInEra Plutus.PlutusV4 (Exp.LedgerEra Exp.DijkstraEra)
  -> Api.ExecutionUnits
  -> Gen (Api.HashableScriptData, Exp.TxGuards (Exp.LedgerEra Exp.DijkstraEra))
genPlutusGuardsWithScriptWitnesses script budget = do
  redeemer <- genHashableScriptData
  let credential = L.ScriptHashObj $ P.hashPlutusScriptInEra script
      witness =
        Exp.PlutusGuardWitness $
          Exp.PlutusScriptWitness
            Plutus.SPlutusV4
            (Exp.PScript script)
            Exp.NoScriptDatum
            redeemer
            budget
  guards <- failEitherWith show $ Exp.mkTxGuards [(credential, Just witness)]
  pure (redeemer, guards)

-- | Generate a Plutus V4 script guard using 'Exp.PReferenceScript'; return its redeemer.
-- The caller must add the reference input to the body and its script output to the UTxO.
genPlutusGuardsWithReferenceScripts
  :: P.PlutusScriptInEra Plutus.PlutusV4 (Exp.LedgerEra Exp.DijkstraEra)
  -> Api.TxIn
  -> Api.ExecutionUnits
  -> Gen (Api.HashableScriptData, Exp.TxGuards (Exp.LedgerEra Exp.DijkstraEra))
genPlutusGuardsWithReferenceScripts script reference budget = do
  redeemer <- genHashableScriptData
  let credential = L.ScriptHashObj $ P.hashPlutusScriptInEra script
      witness =
        Exp.PlutusGuardWitness $
          Exp.PlutusScriptWitness
            Plutus.SPlutusV4
            (Exp.PReferenceScript reference)
            Exp.NoScriptDatum
            redeemer
            budget
  guards <- failEitherWith show $ Exp.mkTxGuards [(credential, Just witness)]
  pure (redeemer, guards)

-- | Generate a zero-valued Plutus V3 withdrawal to test language restrictions.
genLegacyWithdrawal
  :: Api.ExecutionUnits
  -> Gen (Exp.TxWithdrawals (Exp.LedgerEra Exp.DijkstraEra))
genLegacyWithdrawal budget = do
  redeemer <- genHashableScriptData
  script <-
    failEitherWith show $
      P.deserialisePlutusScriptInEra @(Exp.LedgerEra Exp.DijkstraEra) @Plutus.PlutusV3 $
        Base16.decodeLenient "46450101002499"
  pure $
    Exp.TxWithdrawals
      [
        ( Api.makeStakeAddress
            Api.Mainnet
            (Api.StakeCredentialByScript $ Api.fromShelleyScriptHash $ P.hashPlutusScriptInEra script)
        , L.Coin 0
        , Exp.AnyPlutusScriptWitness $
            AnyPlutusWithdrawingScriptWitness $
              Exp.PlutusScriptWitness
                Plutus.SPlutusV3
                (Exp.PScript script)
                Exp.NoScriptDatum
                redeemer
                budget
        )
      ]

-- | How the sub-transaction supplies its Plutus V4 guard script.
data GuardScriptSource = ScriptWitness | ReferenceScript
  deriving Show

-- | One signed sub-transaction with a guard; the top-level supplies only collateral.
data ScriptBalanceCase = ScriptBalanceCase
  { scriptInputs :: PlutusV4ReferenceScriptGuardTestInputs
  , signedSubTransaction :: Exp.SignedSubTx
  , scriptTxContent :: Exp.TxBodyContent (Exp.LedgerEra Exp.DijkstraEra)
  , scriptCaseGuards :: Exp.TxGuards (Exp.LedgerEra Exp.DijkstraEra)
  }

-- | Generate a signed V4 guard sub-transaction and a top-level body with collateral.
-- Leave balancing and script evaluation to the property.
genScriptBalanceCase
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> GuardScriptSource
  -> Api.ExecutionUnits
  -> Gen ScriptBalanceCase
genScriptBalanceCase params source budget = do
  script <- failEitherWith show alwaysSucceedsGuardScript
  inputs <- genPlutusV4ReferenceScriptGuardTestInputs script
  (_, guards) <- case source of
    ScriptWitness -> genPlutusGuardsWithScriptWitnesses script budget
    ReferenceScript -> genPlutusGuardsWithReferenceScripts script (guardReferenceInput inputs) budget
  let references = case source of
        ScriptWitness -> []
        ReferenceScript -> [guardReferenceInput inputs]
  signed <-
    genSignedSubTransaction $
      Exp.defaultSubTxBodyContent
        & Exp.setTxIns [(guardFundingInput inputs, Exp.AnyKeyWitnessPlaceholder)]
        & Exp.setTxProtocolParams params
        & Exp.setTxGuards guards
        & Exp.setTxInsReference (Exp.TxInsReference references Set.empty)
  let content =
        Exp.defaultTxBodyContent
          & Exp.setTxInsCollateral [guardCollateralInput inputs]
          & Exp.setTxSignedSubTransactions [signed]
          & Exp.setTxProtocolParams params
  pure $ ScriptBalanceCase inputs signed content guards

-- | Generate matching top-level and sub-transaction guards without balancing them.
genTopLevelGuardCase
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> GuardScriptSource
  -> Api.ExecutionUnits
  -> Gen ScriptBalanceCase
genTopLevelGuardCase params source budget = do
  example <- genScriptBalanceCase params source budget
  pure example{scriptTxContent = scriptTxContent example & Exp.setTxGuards (scriptCaseGuards example)}

-- | Generate a top-level guard that always fails and a signed sub-transaction
-- whose guard succeeds if its supplied budget is sufficient.
genFailingTopLevelGuardCase
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> Api.ExecutionUnits
  -> Gen ScriptBalanceCase
genFailingTopLevelGuardCase params subBudget = do
  example <- genScriptBalanceCase params ScriptWitness subBudget
  script <-
    failEitherWith show $
      P.deserialisePlutusScriptInEra @(Exp.LedgerEra Exp.DijkstraEra) @Plutus.PlutusV4 $
        Base16.decodeLenient "46450101002601"
  (_, guards) <- genPlutusGuardsWithScriptWitnesses script (Api.ExecutionUnits 0 0)
  pure example{scriptTxContent = scriptTxContent example & Exp.setTxGuards guards}

-- | Generate the requested amounts with a zero-ada V3 withdrawal and separate collateral.
genLegacyTopLevelBalanceTestData
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> TopLevelTxAmounts
  -> [SubTxAmounts]
  -> Gen SubTransactionBalanceTestData
genLegacyTopLevelBalanceTestData params top subs = do
  testData <- genSubTransactionBalanceTestData params top subs
  withdrawal <- genLegacyWithdrawal (Api.ExecutionUnits 0 0)
  let Api.TxIn txId _ = balanceTestTopLevelInput testData
      collateral = Api.TxIn txId (Api.TxIx 100)
      output =
        L.mkBasicTxOut
          (Api.toShelleyAddr $ balanceTestAddress testData)
          (L.MaryValue (L.Coin 100_000_000) mempty)
  pure
    testData
      { balanceTestUTxO =
          L.UTxO $ Map.insert (Api.toShelleyTxIn collateral) output $ L.unUTxO $ balanceTestUTxO testData
      , balanceTestTxBodyContent =
          balanceTestTxBodyContent testData
            & Exp.setTxInsCollateral [collateral]
            & Exp.setTxWithdrawals withdrawal
      }

-- | A sub-transaction pays a new pool's deposit; the legacy top-level transaction
-- registers the same pool again without owing a second deposit.
genLegacyPoolRegistrationBalanceTestData
  :: L.PParams (Exp.LedgerEra Exp.DijkstraEra)
  -> Gen SubTransactionBalanceTestData
genLegacyPoolRegistrationBalanceTestData params = do
  let tenAda = L.MaryValue (L.Coin 10_000_000) mempty
      deposit = L.MaryValue (params ^. L.ppPoolDepositL) mempty
  testData <-
    genLegacyTopLevelBalanceTestData
      params
      (TopLevelTxAmounts $ TxAmounts (tenAda <> tenAda) tenAda)
      [SubTxAmounts $ TxAmounts (tenAda <> deposit) tenAda]
  certificate <- Exp.Certificate . L.mkRegPoolTxCert <$> Q.arbitrary
  let certificates = Exp.mkTxCertificates Exp.DijkstraEra [(certificate, Exp.AnyKeyWitnessPlaceholder)]
  signed <-
    genSignedSubTransaction $
      Exp.defaultSubTxBodyContent
        & Exp.setTxIns [(input, Exp.AnyKeyWitnessPlaceholder) | input <- balanceTestSubTxInputs testData]
        & Exp.setTxOuts [Exp.TxOut $ L.mkBasicTxOut (Api.toShelleyAddr $ balanceTestAddress testData) tenAda]
        & Exp.setTxCertificates certificates
  pure
    testData
      { balanceTestTxBodyContent =
          balanceTestTxBodyContent testData
            & Exp.setTxSignedSubTransactions [signed]
            & Exp.setTxCertificates certificates
      , balanceTestSignedSubTransactions = [signed]
      , -- The shared change assertion must account for the one pool deposit.
        balanceTestSurplus = balanceTestSurplus testData <> L.invert deposit
      }
