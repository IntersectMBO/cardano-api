{-# LANGUAGE DataKinds #-}

-- | Convenience transaction construction functions
module Cardano.Api.Tx.Internal.Convenience
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

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Pretty
import Cardano.Api.ProtocolParameters
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.Body
import Cardano.Api.Tx.Internal.Fee
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.UTxO (UTxO (..))

import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Keys qualified as L

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text qualified as Text
import GHC.Exts (IsList (..))

-- | Construct a balanced transaction.
-- See Cardano.Api.Query.Internal.Convenience.queryStateForBalancedTx for a
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
