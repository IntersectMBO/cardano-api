{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Api.Experimental.Tx.Internal.Guards
  ( TxGuards
  , GuardWitness (..)
  , TxGuardsError (..)
  , mkTxGuards
  , emptyTxGuards
  , txGuardsToList
  , guardWitnessToAnyWitness
  , traverseTxGuardsExecutionUnits
  )
where

import Cardano.Api.Error
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Plutus.Internal.Script (ExecutionUnits)
import Cardano.Api.Pretty

import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Plutus.Language qualified as L

import Control.Monad (foldM_, unless)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Set qualified as Set
import Data.Word (Word32)
import GHC.Exts (toList)

-- | A supplied guard script witness. Key signatures are supplied when signing.
-- The Plutus constructor fixes both the language and the script purpose.
data GuardWitness era
  = NativeGuardWitness (SimpleScriptOrReferenceInput era)
  | PlutusGuardWitness (PlutusScriptWitness L.PlutusV4 GuardingScript era)
  deriving (Eq, Show)

-- | Guard credentials and their optional witnesses, in ledger insertion order.
-- Construct with 'mkTxGuards' so credentials are unique and witnesses agree
-- with them. 'Nothing' means no script witness was supplied; it does not
-- imply that the credential is a key credential or that no witness is required.
newtype TxGuards era = TxGuards (OMap (L.Credential L.Guard) (Maybe (GuardWitness era)))
  deriving (Eq, Show)

data TxGuardsError
  = DuplicateGuardCredential (L.Credential L.Guard)
  | GuardWitnessForKeyCredential (L.Credential L.Guard)
  | GuardScriptHashMismatch L.ScriptHash L.ScriptHash
  deriving (Eq, Show)

instance Error TxGuardsError where
  prettyError (DuplicateGuardCredential credential) =
    "Duplicate guard credential: " <> pshow credential
  prettyError (GuardWitnessForKeyCredential credential) =
    "Script witness supplied for key guard credential: " <> pshow credential
  prettyError (GuardScriptHashMismatch expected actual) =
    "Guard script hash " <> pshow actual <> " does not match credential hash " <> pshow expected

-- | Preserve the supplied order and reject duplicate credentials, script
-- witnesses for key credentials, and mismatching inline script hashes.
-- Reference-script hashes must be checked against the referenced UTxO later.
-- Constructing this collection does not check whether the transaction's era
-- supports guards, or whether all required witnesses have been supplied.
mkTxGuards
  :: L.EraScript era
  => [(L.Credential L.Guard, Maybe (GuardWitness era))]
  -> Either TxGuardsError (TxGuards era)
mkTxGuards guards = do
  foldM_ validate Set.empty guards
  pure $ TxGuards $ OMap.fromList guards
 where
  validate seen (credential, witness) = do
    unless (Set.notMember credential seen) $ Left $ DuplicateGuardCredential credential
    case (credential, witness) of
      (L.KeyHashObj _, Just _) -> Left $ GuardWitnessForKeyCredential credential
      (L.ScriptHashObj expected, Just scriptWitness) ->
        case getAnyWitnessScript (guardWitnessToAnyWitness scriptWitness) of
          Just script ->
            let actual = L.hashScript script
             in unless (expected == actual) $ Left $ GuardScriptHashMismatch expected actual
          Nothing -> pure ()
      (_, Nothing) -> pure ()
    pure $ Set.insert credential seen

emptyTxGuards :: TxGuards era
emptyTxGuards = TxGuards OMap.empty

-- | Inspect guards in insertion order, including entries without witnesses.
txGuardsToList :: TxGuards era -> [(L.Credential L.Guard, Maybe (GuardWitness era))]
txGuardsToList (TxGuards guards) = toList guards

guardWitnessToAnyWitness :: GuardWitness era -> AnyWitness era
guardWitnessToAnyWitness (NativeGuardWitness witness) = AnySimpleScriptWitness witness
guardWitnessToAnyWitness (PlutusGuardWitness witness) =
  AnyPlutusScriptWitness $ AnyPlutusGuardingScriptWitness witness

-- | Update only Plutus budgets, preserving credentials, scripts, and insertion
-- order. Native and witnessless guards occupy indices but need no budget.
traverseTxGuardsExecutionUnits
  :: Applicative f
  => (Word32 -> ExecutionUnits -> f ExecutionUnits)
  -> TxGuards era
  -> f (TxGuards era)
traverseTxGuardsExecutionUnits update guards =
  TxGuards . OMap.fromList <$> traverse updateEntry (zip [0 ..] (txGuardsToList guards))
 where
  updateEntry (index, (credential, witness)) =
    (credential,) <$> traverse (updateWitness index) witness
  updateWitness index (PlutusGuardWitness (PlutusScriptWitness lang script dat redeemer units)) =
    PlutusGuardWitness . PlutusScriptWitness lang script dat redeemer <$> update index units
  updateWitness _ witness@NativeGuardWitness{} = pure witness
