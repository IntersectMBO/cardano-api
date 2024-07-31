{-# LANGUAGE DataKinds #-}

-- | Convenience transaction construction functions
module Cardano.Api.Convenience.Construction
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

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.Experimental.Eras (sbeToEra)
import           Cardano.Api.Experimental.Tx
import           Cardano.Api.Fees
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Tx.Body
import           Cardano.Api.Tx.Sign
import           Cardano.Api.Utils

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Keys as L

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Construct a balanced transaction.
-- See Cardano.Api.Convenience.Query.queryStateForBalancedTx for a
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
  -> Map.Map (L.Credential L.DRepRole L.StandardCrypto) L.Coin
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
    let availableEra = fromMaybe (error "TODO") $ sbeToEra sbe

    BalancedTxBody _ unsignedTx _txBalanceOutput _fee <-
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

    let alternateKeyWits = map (makeKeyWitness availableEra unsignedTx) shelleyWitSigningKeys
        signedTx = signTx availableEra [] alternateKeyWits unsignedTx

    caseShelleyToAlonzoOrBabbageEraOnwards
      (const $ error "constructBalancedTx: TODO Fail")
      (\w -> return $ ShelleyTx sbe $ obtainShimConstraints w signedTx)
      sbe

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
  let onlyCollateralUTxOs = Map.restrictKeys utxo $ Set.fromList collTxIns
      scriptLockedTxIns =
        filter (\(_, TxOut aInEra _ _ _) -> not $ isKeyAddress aInEra) $ Map.assocs onlyCollateralUTxOs
  if null scriptLockedTxIns
    then return ()
    else Left . ScriptLockedTxIns $ map fst scriptLockedTxIns
