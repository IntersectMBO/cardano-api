{-# LANGUAGE DataKinds #-}

-- | Convenience transaction construction functions
module Cardano.Api.Internal.Convenience.Construction
  ( constructBalancedTx

    -- * Misc
  , TxInsExistError (..)
  , ScriptLockedTxInsError (..)
  , notScriptLockedTxIns
  , renderNotScriptLockedTxInsError
  , renderTxInsExistError
  , txInsExistInUTxO
  )
where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Certificate
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Fees
import Cardano.Api.Internal.ProtocolParameters
import Cardano.Api.Internal.Query
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.Sign
import Cardano.Api.Internal.Tx.UTxO (UTxO (..))
import Cardano.Api.Internal.Utils

import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Keys qualified as L

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsList (..))

-- | Construct a balanced transaction.
-- See Cardano.Api.Internal.Convenience.Query.queryStateForBalancedTx for a
-- convenient way of querying the node to get the required arguements
-- for constructBalancedTx.
constructBalancedTx
  :: ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> AddressInEra era
  -- ^ Change address
  -> Maybe Word
  -- ^ Override key witnesses
  -> UTxO era
  -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> LedgerProtocolParameters era
  -> LedgerEpochInfo
  -> SystemStart
  -> Set PoolId
  -- ^ The set of registered stake pools
  -> Map.Map StakeCredential L.Coin
  -> Map.Map (L.Credential L.DRepRole) L.Coin
  -> [ShelleyWitnessSigningKey]
  -> Either (TxBodyErrorAutoBalance era) (Tx era)
constructBalancedTx
  sbe
  txbodcontent
  changeAddr
  mOverrideWits
  utxo
  lpp
  ledgerEpochInfo
  systemStart
  stakePools
  stakeDelegDeposits
  drepDelegDeposits
  shelleyWitSigningKeys = do
    BalancedTxBody _ txbody _txBalanceOutput _fee <-
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        ledgerEpochInfo
        lpp
        stakePools
        stakeDelegDeposits
        drepDelegDeposits
        utxo
        txbodcontent
        changeAddr
        mOverrideWits

    let keyWits = map (makeShelleyKeyWitness sbe txbody) shelleyWitSigningKeys
    return $ makeSignedTransaction keyWits txbody

data TxInsExistError
  = TxInsDoNotExist [TxIn]
  | EmptyUTxO

renderTxInsExistError :: TxInsExistError -> Text
renderTxInsExistError EmptyUTxO =
  "The UTxO is empty"
renderTxInsExistError (TxInsDoNotExist txins) =
  "The following tx input(s) were not present in the UTxO: "
    <> Text.singleton '\n'
    <> Text.intercalate (Text.singleton '\n') (map renderTxIn txins)

txInsExistInUTxO :: [TxIn] -> UTxO era -> Either TxInsExistError ()
txInsExistInUTxO ins (UTxO utxo)
  | null utxo = Left EmptyUTxO
  | otherwise = do
      let utxoIns = Map.keys utxo
          occursInUtxo = [txin | txin <- ins, txin `elem` utxoIns]
      if length occursInUtxo == length ins
        then return ()
        else Left . TxInsDoNotExist $ ins List.\\ occursInUtxo

newtype ScriptLockedTxInsError = ScriptLockedTxIns [TxIn]

renderNotScriptLockedTxInsError :: ScriptLockedTxInsError -> Text
renderNotScriptLockedTxInsError (ScriptLockedTxIns txins) =
  "The followings tx inputs were expected to be key witnessed but are actually script witnessed: "
    <> textShow (map renderTxIn txins)

notScriptLockedTxIns :: [TxIn] -> UTxO era -> Either ScriptLockedTxInsError ()
notScriptLockedTxIns collTxIns (UTxO utxo) = do
  let onlyCollateralUTxOs = Map.restrictKeys utxo $ fromList collTxIns
      scriptLockedTxIns =
        filter (\(_, TxOut aInEra _ _ _) -> not $ isKeyAddress aInEra) $ Map.assocs onlyCollateralUTxOs
  if null scriptLockedTxIns
    then return ()
    else Left . ScriptLockedTxIns $ map fst scriptLockedTxIns
