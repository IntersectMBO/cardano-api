{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Guards add required key signatures or script checks to Dijkstra transactions
-- and sub-transactions independently of spending inputs. Key guards are satisfied
-- by signatures supplied when signing; script guards use native or Plutus V4
-- witnesses, supplied inline or through reference inputs.
--
-- 'TxGuards' associates guard credentials with optional script witnesses and
-- preserves insertion order, which determines Plutus guard redeemer indices.
-- Use 'mkTxGuards' to validate the supplied entries before building a transaction.
module Cardano.Api.Experimental.Tx.Internal.Guard
  ( TxGuards
  , ScriptGuardWitness (..)
  , TxGuardsError (..)
  , mkTxGuards
  , emptyTxGuards
  , scriptGuardWitnessToAnyWitness
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
import Data.Foldable (for_)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Set qualified as Set
import Data.Word (Word32)
import GHC.Exts (IsList (..))

-- | A supplied guard script witness. Key signatures are supplied when signing.
-- The Plutus constructor fixes both the language and the script purpose.
data ScriptGuardWitness era
  = NativeGuardWitness (SimpleScriptOrReferenceInput era)
  | PlutusGuardWitness (PlutusScriptWitness L.PlutusV4 GuardingScript era)
  deriving (Eq, Show)

-- | Guard credentials and their optional witnesses, in ledger insertion order.
-- Construct with 'mkTxGuards' so credentials are unique and witnesses agree
-- with them. Key credentials must have 'Nothing' as their script witness;
-- 'mkTxGuards' rejects a 'Just' witness for a key credential. Key signatures
-- are supplied separately when signing.
-- 'Nothing' means no script witness was supplied; it does not
-- imply that the credential is a key credential or that no witness is required.
-- Use 'toList' to inspect entries in insertion order. The derived 'IsList'
-- instance constructs the underlying ordered map without the validation
-- performed by 'mkTxGuards'.
newtype TxGuards era = TxGuards (OMap (L.Credential L.Guard) (Maybe (ScriptGuardWitness era)))
  deriving (Eq, Show, IsList)

-- | Validation errors returned by 'mkTxGuards'.
data TxGuardsError
  = -- | The same guard credential occurs more than once in the supplied entries.
    DuplicateGuardCredential (L.Credential L.Guard)
  | -- | A script witness was supplied for a key credential, which must have 'Nothing'.
    GuardWitnessForKeyCredential (L.Credential L.Guard)
  | -- | An inline native or Plutus script does not match its guard credential.
    -- The first hash is the expected credential hash; the second is the actual
    -- script hash. Reference-script hashes are not checked by 'mkTxGuards'.
    GuardScriptHashMismatch L.ScriptHash L.ScriptHash
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
  => [(L.Credential L.Guard, Maybe (ScriptGuardWitness era))]
  -> Either TxGuardsError (TxGuards era)
mkTxGuards guards = do
  foldM_ validate Set.empty guards
  pure $ TxGuards $ OMap.fromList guards
 where
  validate
    :: L.EraScript era
    => Set.Set (L.Credential L.Guard)
    -> (L.Credential L.Guard, Maybe (ScriptGuardWitness era))
    -> Either TxGuardsError (Set.Set (L.Credential L.Guard))
  validate seen (credential, witness) = do
    unless (Set.notMember credential seen) $ Left $ DuplicateGuardCredential credential
    case (credential, witness) of
      (L.KeyHashObj _, Just _) -> Left $ GuardWitnessForKeyCredential credential
      (L.ScriptHashObj expected, Just scriptWitness) ->
        for_ (getAnyWitnessScript (scriptGuardWitnessToAnyWitness scriptWitness)) $ \script -> do
          let actual = L.hashScript script
          unless (expected == actual) $ Left $ GuardScriptHashMismatch expected actual
      (L.KeyHashObj _, Nothing) -> pure ()
      (L.ScriptHashObj _, Nothing) -> pure ()
    pure $ Set.insert credential seen

-- | An empty guard collection, containing no credentials or script witnesses.
emptyTxGuards :: TxGuards era
emptyTxGuards = TxGuards OMap.empty

scriptGuardWitnessToAnyWitness :: ScriptGuardWitness era -> AnyWitness era
scriptGuardWitnessToAnyWitness (NativeGuardWitness witness) = AnySimpleScriptWitness witness
scriptGuardWitnessToAnyWitness (PlutusGuardWitness witness) =
  AnyPlutusScriptWitness $ AnyPlutusGuardingScriptWitness witness

-- | Update only Plutus budgets, preserving credentials, scripts, and insertion
-- order. Native and witnessless guards occupy indices but need no budget.
traverseTxGuardsExecutionUnits
  :: Applicative f
  => (Word32 -> ExecutionUnits -> f ExecutionUnits)
  -- ^ Update a Plutus guard's budget using its zero-based index in the complete
  -- guard collection and its current execution units. Native and witnessless
  -- guards count towards the index but do not invoke this callback.
  -> TxGuards era
  -- ^ Guard credentials and optional witnesses in insertion order.
  -> f (TxGuards era)
  -- ^ Guards with updated Plutus budgets, preserving all other contents and order.
traverseTxGuardsExecutionUnits update guards =
  TxGuards . OMap.fromList <$> traverse updateEntry (zip [0 ..] (toList guards))
 where
  updateEntry (index, (credential, witness)) =
    (credential,) <$> traverse (updateWitness index) witness
  updateWitness index (PlutusGuardWitness (PlutusScriptWitness lang script dat redeemer units)) =
    PlutusGuardWitness . PlutusScriptWitness lang script dat redeemer <$> update index units
  updateWitness _ witness@NativeGuardWitness{} = pure witness
