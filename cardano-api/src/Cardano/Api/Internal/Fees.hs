{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Calculating fees.
module Cardano.Api.Internal.Fees
  ( -- * Introduction

    -- |
    -- The "Cardano.Api.Internal.Tx.Body" documentation demonstrates how to create a 'TxBodyContent' for a
    -- transaction that takes 12 ada and sends 10 ada to an address, and spends 2 ada for fees. If a
    -- UTXO with exactly 12 ada is available, this would be a valid transaction, because it is balanced, and has
    -- sufficient fees, as 2 ada is more than enough to cover the fees for such a simple transaction on mainnet
    -- at the time of writing.
    --
    -- A simple transaction that spends UTXOs is considered balanced if the value consumed equals
    -- the value produced. Simply put, a transaction that spends only lovelace must account for
    -- the total lovelace at the inputs, output(s), and transaction fee.
    --
    -- In other words:
    --
    -- @
    -- inputs = outputs + fee
    -- @
    --
    -- In this equation, the inputs would include the minted tokens, and the outputs would include the
    -- burned tokens.
    --
    -- However, we don't always want to spend all the ada from a UTXO. Balancing a transaction ensures
    -- that we send the desired amount, pay only the necessary fees, and calculate any extra currency
    -- to be sent back to a change address.
    --
    -- Since changes to the transaction body can affect the required, finding this balance can be
    -- challenging. Fortunately, there are functions available to help achieve this.
    --
    -- This module offers several methods for calculating transaction fees or fully balancing a draft
    -- transaction. Each method requires varying amounts of information and provides different levels
    -- of accuracy and automation.
    --
    -- Next, this module explores three examples of fee calculation methods. Other methods exist
    -- but they have similar requirements.
    --
    -- Examples below use the following qualified modules:
    --
    -- @
    -- import qualified Cardano.Api as Api                -- the general `cardano-api` exports
    -- @

    -- ** Example 1: Simple fee calculation and manual balancing

    -- |
    -- This method requires minimal information and some manual work.
    --
    -- It calculates the transaction fees, but we need to balance the transaction manually.
    --
    -- We need to know:
    --
    -- 1. The Shelley-based era witness for the current era, which can be obtained by using 'shelleyBasedEra'.
    --
    -- @
    -- let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    -- @
    --
    -- 2. The protocol parameters for the current era, which can be obtained using the 'QueryProtocolParameters'
    -- query defined in "Cardano.Api.Internal.Query". "Cardano.Api.Internal.IPC" documentation illustrates how
    -- to make a query using IPC protocol.
    -- Let's assume they are stored in the @exampleProtocolParams@ variable.
    --
    -- 3. The draft transaction body, which can be created using 'createTransactionBody' defined in "Cardano.Api.Internal.Tx.Body":
    --
    -- @
    -- let (Right txBody) = Api.createTransactionBody sbe txBodyContent
    -- @
    --
    -- 4. The number of Byron and Shelley key witnesses, which corresponds to the number of keys required
    -- to sign the transaction. We can estimate this by using 'estimateTransactionKeyWitnessCount'.
    -- For a simple transaction with a single UTXO locked by a single modern Shelley key,
    -- there would be @0@ Byron witnesses and @1@ Shelley witness.
    --
    -- 5. The size of any reference scripts in bytes (if any). For simple transactions,
    -- this would be @0@.
    --
    -- With this in mind, it is possible to estimate the fees required by the transaction by calling
    -- 'evaluateTransactionFee' as follows:
    --
    -- @
    -- let fees = Api.evaluateTransactionFee sbe exampleProtocolParams txBody 0 1 0
    -- @
    --
    -- Once we know the required fees, we can balance the transaction by
    -- subtracting fees from the total value of the UTXO being spent.
    --
    -- For example, if we have a UTXO with 12 ada, the fees are 0.2 ada, and we want
    -- to send 10 ada to an address, the transaction could look like this:
    --
    -- * 1 input of 12 ada (from the UTXO)
    -- * 1 output of 10 ada (to the recipient address)
    -- * 1 output of 1.8 ada (to the change address)
    -- * 0.2 ada in fees (calculated fees, in this case, an overestimation).
    --
    -- We would then have to update the 'TxBodyContent' accordingly and continue building
    -- the transaction as demonstrated in "Cardano.Api.Internal.Experimental.Tx".

    -- ** Example 2: Automated balancing without chain information (no UTXO, no ledger state)

    -- |
    -- This method requires more information, but better automates the process, reducing
    -- the need for estimation. It also works with a 'TxBodyContent', rather than a
    -- full 'TxBody'.
    --
    -- The following information is required:
    --
    -- 1. The MaryEraOnwards witness for the current era. In this case, we use the
    -- one for the 'Conway' era:
    --
    -- @
    -- let meo = Api.MaryEraOnwardsConway
    -- @
    --
    -- 2. The draft 'TxBodyContent' for the transaction we want to balance. See how to create one
    -- in "Cardano.Api.Internal.Tx.Body". It is assumed to be stored in the @txBodyContent@ variable.
    --
    -- 3. The protocol parameters for the current era, which can be obtained using the 'QueryProtocolParameters'
    -- query defined in "Cardano.Api.Internal.Query". "Cardano.Api.Internal.IPC" documentation illustrates how
    -- to make a query using IPC protocol. Let's assume they are stored in the @exampleProtocolParams@ variable.
    --
    -- 4. For stake pool and governance actions, we will also need:
    --
    --     * The set of registered stake pools being unregistered in this transaction
    --     * The map of all deposits for stake credentials being unregistered in this transaction
    --     * The map of all deposits for DRep credentials that are unregistered in this transaction
    --     * Plutus script execution units for all script witnesses used in the transaction.
    --
    --    This example assumes we are only spending key-locked UTXOs. Therefore, we can ignore the above
    --    and use 'mempty'.
    --
    -- 5. The following amounts:
    --
    --     * Collateral amount: required only for transactions involving Plutus scripts. Since
    --       this transaction does not include any, the collateral is @0@.
    --     * Amount of Shelley key witnesses to be added: estimated using 'estimateTransactionKeyWitnessCount'.
    --       For a simple transaction spending a single UTXO locked by a modern Shelley key, there is @1@
    --       Shelley witness.
    --     * Amount of Byron key witnesses to be added: assumed to be @0@ for this transaction.
    --     * Size of all reference scripts (in bytes): since no reference scripts are used, this value is @0@.
    --
    -- 6. The address for sending the change. We can deserialize it from its bech32 representation
    -- using the 'deserialiseAddress' function defined in "Cardano.Api.Internal.Address":
    --
    -- @
    -- let Just exampleChangeAddress =
    --            Api.deserialiseAddress
    --              (Api.AsAddressInEra eraAsType)
    --              "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
    -- @
    --
    -- Alternatively, we can get it from our signing key:
    --
    -- @
    -- let exampleChangeAddress =
    --       Api.shelleyAddressInEra sbe $
    --         Api.makeShelleyAddress
    --           (Api.Testnet $ Api.NetworkMagic 2)    -- The magic for the network we are using
    --           (Api.PaymentCredentialByKey
    --              $ Api.verificationKeyHash
    --                  $ Api.getVerificationKey
    --                      signingKey)
    --           Api.NoStakeAddress                    -- Potentially, the stake credential if we want to use one
    -- @
    --
    -- 7. Finally, we need the total amount of ada (and other tokens) in the UTXOs being spent. In this example,
    -- the transaction spends 12 ada:
    --
    -- @
    -- let totalUTxOValue = Api.lovelaceToValue 12_000_000
    -- @
    --
    -- With all this information, a balanced transaction can be obtained by calling 'estimateBalancedTxBody'
    -- as follows:
    --
    -- @
    -- let (Right (Api.BalancedTxBody
    --               updatedTxBodyContent
    --               updatedTxBody
    --               changeOutput
    --               fees)) =
    --       Api.estimateBalancedTxBody
    --         meo
    --         txBodyContent
    --         exampleProtocolParams
    --         mempty
    --         mempty
    --         mempty
    --         mempty
    --         0
    --         1
    --         0
    --         0
    --         exampleChangeAddress
    -- @
    --
    -- This will produce a balanced transaction body with calculated fees and a change output, but
    -- it still needs to be signed and submitted.

    -- ** Example 3: Fully automated balancing with chain information (requires UTXO and ledger state data)

    -- |
    -- The previous example required manually providing various details, which was feasible for a simple
    -- transaction. However, for more complex transactions involving scripts or advanced functionalities
    -- such as governance actions, a more automated approach is available. This method performs most
    -- calculations in exchange for general ledger information.
    --
    -- The following details are required:
    --
    -- 1. Shelley-based era witness for the current era, retrievable using 'shelleyBasedEra'.
    --
    -- @
    -- let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    -- @
    --
    -- 2. Network start time, obtainable using the 'QuerySystemStart' query defined in
    -- "Cardano.Api.Internal.Query". "Cardano.Api.Internal.IPC" documentation illustrates how
    -- to make a query using IPC protocol. Assume we have it in the @exampleSystemStart@ variable.
    --
    -- 3. Ledger epoch information, derivable by applying 'toLedgerEpochInfo' to the
    -- 'EraHistory', which can be retrieved using the 'QueryEraHistory' query defined in
    -- "Cardano.Api.Internal.Query". Assume this is stored in the @exampleLedgerEpochInfo@ variable.
    --
    -- 4. Protocol parameters for the current era, accessible through the 'QueryProtocolParameters'
    -- query defined in "Cardano.Api.Internal.Query". Assume this is stored in the @exampleProtocolParams@ variable.
    --
    -- 5. For stake pool and gov actions, additional data is requried:
    --
    --      * The set of registered stake pools being unregistered in this transaction
    --      * The map of all deposits associated with stake credentials being unregistered in this transaction
    --      * The map of all deposits associated with DRep credentials being unregistered in this transaction.
    --
    --    For this example, no stake pool deregistration or governance actions are considered, so 'mempty' is used
    --    for the corresponding parameters.
    --
    -- 6. UTXO set being spent -- the full UTXO set can be obtained using the 'QueryUTxO' query
    -- from "Cardano.Api.Internal.Query". Assume this is stored in the @utxoToUse@ variable.
    --
    -- 7. Draft transaction body content -- the 'TxBodyContent' for the transaction that requires balancing.
    -- The process for creating this structure is demonstrated in "Cardano.Api.Internal.Tx.Body".
    --
    -- 8. Change address -- the address where any remaining balance should be returned. This can be
    -- deserialized from its Bech32 representation using the 'deserialiseAddress' function from
    -- "Cardano.Api.Internal.Address":
    --
    -- @
    -- let (Just exampleChangeAddress) =
    --             Api.deserialiseAddress
    --               (Api.AsAddressInEra eraAsType)
    --               "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
    -- @
    --
    -- Alternatively, it is possible to get it from the signing key:
    --
    -- @
    -- let exampleChangeAddress =
    --       Api.shelleyAddressInEra sbe $
    --         Api.makeShelleyAddress
    --           (Api.Testnet $ Api.NetworkMagic 2)    -- The magic for the network we are using
    --           (Api.PaymentCredentialByKey
    --              $ Api.verificationKeyHash
    --                  $ Api.getVerificationKey
    --                      signingKey)
    --           Api.NoStakeAddress                    -- Potentially, the stake credential if we want to use one
    -- @
    --
    --
    -- 9. Finally, the number of key witnesses required for the transaction can be manually specified.
    -- However, if set to 'Nothing', the function will automatically estimate the required number.
    --
    -- With all the necessary information available, a balanced transaction can be obtained
    -- by calling 'makeTransactionBodyAutoBalance' as follows:
    --
    -- @
    -- let Right (Api.BalancedTxBody
    --              updatedTxBodyContent
    --              updatedTxBody
    --              changeOutput
    --              fees) =
    --    Api.makeTransactionBodyAutoBalance
    --      sbe
    --      exampleSystemStart
    --      exampleLedgerEpochInfo
    --      (Api.LedgerProtocolParameters exampleProtocolParams)
    --      mempty
    --      mempty
    --      mempty
    --      utxoToUse
    --      txBodyContent
    --      exampleChangeAddress
    --      Nothing
    -- @
    --
    -- This will give us a balanced transaction with the fees calculated and the change output
    -- included. The transaction can now be signed and submitted.

    -- * Contents

    -- ** Transaction fees
    evaluateTransactionFee
  , calculateMinTxFee
  , estimateTransactionKeyWitnessCount

    -- ** Script execution units
  , evaluateTransactionExecutionUnits
  , evaluateTransactionExecutionUnitsShelley
  , ScriptExecutionError (..)
  , TransactionValidityError (..)

    -- ** Transaction balance
  , evaluateTransactionBalance

    -- ** Automated transaction building
  , estimateBalancedTxBody
  , estimateOrCalculateBalancedTxBody
  , makeTransactionBodyAutoBalance
  , calcReturnAndTotalCollateral
  , AutoBalanceError (..)
  , BalancedTxBody (..)
  , FeeEstimationMode (..)
  , RequiredShelleyKeyWitnesses (..)
  , RequiredByronKeyWitnesses (..)
  , TotalReferenceScriptsSize (..)
  , TxBodyErrorAutoBalance (..)
  , TxFeeEstimationError (..)

    -- ** Minimum UTxO calculation
  , calculateMinimumUTxO

    -- ** Internal helpers
  , ResolvablePointers (..)
  , substituteExecutionUnits
  , handleExUnitsErrors
  )
where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Certificate
import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Internal.Eon.MaryEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Case
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.Error
import Cardano.Api.Internal.Feature
import Cardano.Api.Internal.Plutus
import Cardano.Api.Internal.Pretty
import Cardano.Api.Internal.ProtocolParameters
import Cardano.Api.Internal.Query
import Cardano.Api.Internal.ReexposeLedger qualified as L
import Cardano.Api.Internal.Script
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.Sign
import Cardano.Api.Internal.Tx.UTxO (UTxO (..))
import Cardano.Api.Internal.Value
import Cardano.Api.Ledger.Lens qualified as A

import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Alonzo.Plutus.Context qualified as Plutus
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.Credential as Ledger (Credential)
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Val qualified as L
import Ouroboros.Consensus.HardFork.History qualified as Consensus

import Data.Bifunctor (bimap, first, second)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Short (ShortByteString)
import Data.Function ((&))
import Data.List (sortBy)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OSet.Strict qualified as OSet
import Data.Ord (Down (Down), comparing)
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro ((.~), (^.))
import Prettyprinter (punctuate)

-- | Type synonym for logs returned by the ledger's @evalTxExUnitsWithLogs@ function.
-- for scripts in transactions.
type EvalTxExecutionUnitsLog = [Text]

data AutoBalanceError era
  = AutoBalanceEstimationError (TxFeeEstimationError era)
  | AutoBalanceCalculationError (TxBodyErrorAutoBalance era)
  deriving Show

instance Error (AutoBalanceError era) where
  prettyError = \case
    AutoBalanceEstimationError e -> prettyError e
    AutoBalanceCalculationError e -> prettyError e

estimateOrCalculateBalancedTxBody
  :: ShelleyBasedEra era
  -> FeeEstimationMode era
  -> L.PParams (ShelleyLedgerEra era)
  -> TxBodyContent BuildTx era
  -> Set PoolId
  -> Map StakeCredential L.Coin
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -> AddressInEra era
  -> Either (AutoBalanceError era) (BalancedTxBody era)
estimateOrCalculateBalancedTxBody era feeEstMode pparams txBodyContent poolids stakeDelegDeposits drepDelegDeposits changeAddr =
  case feeEstMode of
    CalculateWithSpendableUTxO utxo systemstart ledgerEpochInfo mOverride ->
      first AutoBalanceCalculationError $
        makeTransactionBodyAutoBalance
          era
          systemstart
          ledgerEpochInfo
          (LedgerProtocolParameters pparams)
          poolids
          stakeDelegDeposits
          drepDelegDeposits
          utxo
          txBodyContent
          changeAddr
          mOverride
    EstimateWithoutSpendableUTxO
      totalPotentialCollateral
      totalUTxOValue
      exUnitsMap
      (RequiredShelleyKeyWitnesses numKeyWits)
      (RequiredByronKeyWitnesses numByronWits)
      (TotalReferenceScriptsSize totalRefScriptsSize) ->
        forShelleyBasedEraInEon
          era
          (Left $ AutoBalanceEstimationError TxFeeEstimationOnlyMaryOnwardsSupportedError)
          ( \w ->
              first AutoBalanceEstimationError $
                estimateBalancedTxBody
                  w
                  txBodyContent
                  pparams
                  poolids
                  stakeDelegDeposits
                  drepDelegDeposits
                  exUnitsMap
                  totalPotentialCollateral
                  numKeyWits
                  numByronWits
                  totalRefScriptsSize
                  changeAddr
                  totalUTxOValue
          )

data TxFeeEstimationError era
  = TxFeeEstimationTransactionTranslationError (TransactionValidityError era)
  | TxFeeEstimationScriptExecutionError (TxBodyErrorAutoBalance era)
  | TxFeeEstimationBalanceError (TxBodyErrorAutoBalance era)
  | TxFeeEstimationxBodyError TxBodyError
  | TxFeeEstimationFinalConstructionError TxBodyError
  | TxFeeEstimationOnlyMaryOnwardsSupportedError
  deriving Show

instance Error (TxFeeEstimationError era) where
  prettyError = \case
    TxFeeEstimationTransactionTranslationError e -> prettyError e
    TxFeeEstimationScriptExecutionError e -> prettyError e
    TxFeeEstimationBalanceError e -> prettyError e
    TxFeeEstimationxBodyError e -> prettyError e
    TxFeeEstimationFinalConstructionError e -> prettyError e
    TxFeeEstimationOnlyMaryOnwardsSupportedError ->
      "Only mary era onwards supported."

-- | Use when you do not have access to the UTxOs you intend to spend
estimateBalancedTxBody
  :: forall era
   . HasCallStack
  => MaryEraOnwards era
  -> TxBodyContent BuildTx era
  -> L.PParams (ShelleyLedgerEra era)
  -> Set PoolId
  -- ^ The set of registered stake pools, being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ A map of all deposits for stake credentials that are being
  --   unregistered in this transaction.
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -- ^ A map of all deposits for DRep credentials that are being
  --   unregistered in this transaction.
  -> Map ScriptWitnessIndex ExecutionUnits
  -- ^ Plutus script execution units.
  -> Coin
  -- ^ Total potential collateral amount.
  -> Int
  -- ^ The number of key witnesses to be added to the transaction.
  -> Int
  -- ^ The number of Byron key witnesses to be added to the transaction.
  -> Int
  -- ^ The size of all reference scripts in bytes.
  -> AddressInEra era
  -- ^ Change address.
  -> Value
  -- ^ Total value of UTXOs being spent.
  -> Either (TxFeeEstimationError era) (BalancedTxBody era)
estimateBalancedTxBody
  w
  txbodycontent
  pparams
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  exUnitsMap
  totalPotentialCollateral
  intendedKeyWits
  byronwits
  sizeOfAllReferenceScripts
  changeaddr
  totalUTxOValue = do
    -- Step 1. Substitute those execution units into the tx

    let sbe = convert w
    txbodycontent1 <-
      maryEraOnwardsConstraints w $
        first TxFeeEstimationScriptExecutionError $
          substituteExecutionUnits exUnitsMap txbodycontent

    -- Step 2. We need to calculate the current balance of the tx. The user
    -- must at least provide the total value of the UTxOs they intend to spend
    -- for us to calulate the balance. NB: We must:
    --  1. Subtract certificate and proposal deposits
    -- from the total available Ada value!
    -- Page 24 Shelley ledger spec
    let certificates = convCertificates sbe $ txCertificates txbodycontent1

        proposalProcedures :: OSet.OSet (L.ProposalProcedure (ShelleyLedgerEra era))
        proposalProcedures =
          maryEraOnwardsConstraints w $
            maybe mempty (convProposalProcedures . unFeatured) (txProposalProcedures txbodycontent1)

        totalDeposits :: L.Coin
        totalDeposits =
          -- Because we do not have access to the ledger state and to reduce the complexity of this function's
          -- type signature, we assume the user is trying to register a stake pool that has not been
          -- registered before and has not included duplicate stake pool registration certificates.
          let assumeStakePoolHasNotBeenRegistered = const False
           in sum
                [ maryEraOnwardsConstraints w $
                    L.getTotalDepositsTxCerts pparams assumeStakePoolHasNotBeenRegistered certificates
                , maryEraOnwardsConstraints w $
                    mconcat $
                      map (^. L.pProcDepositL) $
                        toList proposalProcedures
                ]

        availableUTxOValue =
          mconcat
            [ totalUTxOValue
            , negateValue (lovelaceToValue totalDeposits)
            ]

    let partialChange = toLedgerValue w $ calculatePartialChangeValue sbe availableUTxOValue txbodycontent1
        maxLovelaceChange = L.Coin (2 ^ (64 :: Integer)) - 1
        changeWithMaxLovelace = partialChange & A.adaAssetL sbe .~ maxLovelaceChange
        changeTxOut =
          forShelleyBasedEraInEon
            sbe
            (lovelaceToTxOutValue sbe maxLovelaceChange)
            (\w' -> maryEraOnwardsConstraints w' $ TxOutValueShelleyBased sbe changeWithMaxLovelace)

    let (dummyCollRet, dummyTotColl) = maybeDummyTotalCollAndCollReturnOutput sbe txbodycontent changeaddr

    -- Step 3. Create a tx body with out max lovelace fee. This is strictly for
    -- calculating our fee with evaluateTransactionFee.
    let maxLovelaceFee = L.Coin (2 ^ (32 :: Integer) - 1)
    txbody1ForFeeEstimateOnly <-
      first TxFeeEstimationxBodyError $ -- TODO: impossible to fail now
        createTransactionBody
          sbe
          txbodycontent1
            { txFee = TxFeeExplicit sbe maxLovelaceFee
            , txOuts =
                TxOut changeaddr changeTxOut TxOutDatumNone ReferenceScriptNone
                  : txOuts txbodycontent
            , txReturnCollateral = dummyCollRet
            , txTotalCollateral = dummyTotColl
            }
    let fee =
          evaluateTransactionFee
            sbe
            pparams
            txbody1ForFeeEstimateOnly
            (fromIntegral intendedKeyWits)
            (fromIntegral byronwits)
            sizeOfAllReferenceScripts

        -- Step 4. We use the fee to calculate the required collateral
        (retColl, reqCol) =
          caseShelleyToAlonzoOrBabbageEraOnwards
            (const (TxReturnCollateralNone, TxTotalCollateralNone))
            ( \w' ->
                calcReturnAndTotalCollateral
                  w'
                  fee
                  pparams
                  (txInsCollateral txbodycontent)
                  (txReturnCollateral txbodycontent)
                  (txTotalCollateral txbodycontent)
                  changeaddr
                  (A.mkAdaValue sbe totalPotentialCollateral)
            )
            sbe

    -- Step 5. Now we can calculate the balance of the tx. What matter here are:
    --  1. The original outputs
    --  2. Tx fee
    --  3. Return and total collateral
    txbody2 <-
      first TxFeeEstimationxBodyError $ -- TODO: impossible to fail now
        createTransactionBody
          sbe
          txbodycontent1
            { txFee = TxFeeExplicit sbe fee
            , txReturnCollateral = retColl
            , txTotalCollateral = reqCol
            }

    let fakeUTxO = createFakeUTxO sbe txbodycontent1 $ selectLovelace availableUTxOValue
        balance =
          evaluateTransactionBalance sbe pparams poolids stakeDelegDeposits drepDelegDeposits fakeUTxO txbody2
        balanceTxOut = TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone

    -- Step 6. Check all txouts have the min required UTxO value
    first (TxFeeEstimationBalanceError . uncurry TxBodyErrorMinUTxONotMet)
      . mapM_ (checkMinUTxOValue sbe pparams)
      $ txOuts txbodycontent1

    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    finalTxOuts <-
      first TxFeeEstimationBalanceError $
        checkAndIncludeChange sbe pparams balanceTxOut (txOuts txbodycontent1)

    -- Step 7.

    -- Create the txbody with the final fee and change output. This should work
    -- provided that the fee and change are less than 2^32-1, and so will
    -- fit within the encoding size we picked above when calculating the fee.
    -- Yes this could be an over-estimate by a few bytes if the fee or change
    -- would fit within 2^16-1. That's a possible optimisation.
    let finalTxBodyContent =
          txbodycontent1
            { txFee = TxFeeExplicit sbe fee
            , txOuts = finalTxOuts
            , txReturnCollateral = retColl
            , txTotalCollateral = reqCol
            }
    txbody3 <-
      first TxFeeEstimationFinalConstructionError $ -- TODO: impossible to fail now. We need to implement a function
      -- that simply creates a transaction body because we have already
      -- validated the transaction body earlier within makeTransactionBodyAutoBalance
        createTransactionBody sbe finalTxBodyContent
    return
      ( BalancedTxBody
          finalTxBodyContent
          txbody3
          balanceTxOut
          fee
      )

--- ----------------------------------------------------------------------------
--- Transaction fees
---

-- | Transaction fees can be computed for a proposed transaction based on the
-- expected number of key witnesses (i.e. signatures).
--
-- When possible, use 'calculateMinTxFee', as it provides a more accurate
-- estimate:
evaluateTransactionFee
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxBody era
  -> Word
  -- ^ The number of Shelley key witnesses
  -> Word
  -- ^ The number of Byron key witnesses
  -> Int
  -- ^ Reference script size in bytes
  -> L.Coin
evaluateTransactionFee sbe pp txbody keywitcount byronwitcount refScriptsSize =
  shelleyBasedEraConstraints sbe $
    case makeSignedTransaction' (toCardanoEra sbe) [] txbody of
      ShelleyTx _ tx ->
        L.estimateMinFeeTx pp tx (fromIntegral keywitcount) (fromIntegral byronwitcount) refScriptsSize

-- | Estimate the minimum transaction fee by analyzing the transaction structure
-- and determining the required number and type of key witnesses.
--
-- It requires access to the relevant portion of the UTXO set to look up any
-- transaction inputs (txins) included in the transaction. However, it cannot
-- reliably determine the number of witnesses required for native scripts.
--
-- Therefore, the number of witnesses needed for native scripts must be provided
-- as an additional argument.
calculateMinTxFee
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> UTxO era
  -> TxBody era
  -> Word
  -- ^ The number of Shelley key witnesses
  -> L.Coin
calculateMinTxFee sbe pp utxo txbody keywitcount =
  shelleyBasedEraConstraints sbe $
    case makeSignedTransaction' (toCardanoEra sbe) [] txbody of
      ShelleyTx _ tx ->
        L.calcMinFeeTx (toLedgerUTxO sbe utxo) pp tx (fromIntegral keywitcount)

-- | Provide and approximate count of the key witnesses (i.e. signatures)
-- required for a transaction.
--
-- This estimate is not exact and may overestimate the required number of witnesses.
-- The function makes conservative assumptions, including:
--
-- * Treating all inputs as originating from distinct addresses. In reality,
--   multiple inputs may share the same address, requiring only one witness per address.
--
-- * Assuming regular and collateral inputs are distinct, even though they may overlap.
--
-- TODO: Consider implementing a more precise calculation that leverages the UTXO set
-- to determine which inputs correspond to distinct addresses. Additionally, the
-- estimate can be refined by distinguishing between Shelley and Byron-style witnesses.
estimateTransactionKeyWitnessCount :: TxBodyContent BuildTx era -> Word
estimateTransactionKeyWitnessCount
  TxBodyContent
    { txIns
    , txInsCollateral
    , txExtraKeyWits
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    } =
    fromIntegral $
      sum (map estimateTxInWitnesses txIns)
        + case txInsCollateral of
          TxInsCollateral _ txins ->
            length txins
          _ -> 0
        + case txExtraKeyWits of
          TxExtraKeyWitnesses _ khs ->
            length khs
          _ -> 0
        + case txWithdrawals of
          TxWithdrawals _ withdrawals ->
            length [() | (_, _, BuildTxWith KeyWitness{}) <- withdrawals]
          _ -> 0
        + case txCertificates of
          TxCertificates _ credWits ->
            length
              [() | (_, BuildTxWith (Just (_, KeyWitness{}))) <- toList credWits]
          _ -> 0
        + case txUpdateProposal of
          TxUpdateProposal _ (UpdateProposal updatePerGenesisKey _) ->
            Map.size updatePerGenesisKey
          _ -> 0
   where
    estimateTxInWitnesses :: (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era)) -> Int
    estimateTxInWitnesses (_, BuildTxWith (KeyWitness _)) = 1
    estimateTxInWitnesses (_, BuildTxWith (ScriptWitness _ (SimpleScriptWitness _ (SScript simpleScript)))) = maxWitnessesInSimpleScript simpleScript
    estimateTxInWitnesses (_, BuildTxWith (ScriptWitness _ (SimpleScriptWitness _ (SReferenceScript _)))) = 0
    estimateTxInWitnesses (_, BuildTxWith (ScriptWitness _ (PlutusScriptWitness{}))) = 0

    -- This is a rough conservative estimate of the maximum number of witnesses
    -- needed for a simple script to be satisfied. It is conservative because it
    -- assumes that each key hash only appears once, and it assumes the worst
    -- scenario. A more accurate estimate for the maximum could be computed by
    -- keeping track of the possible combinations of key hashes that have
    -- potentially already been counted, but that would increase complexity a lot,
    -- and it would still be a conservative estimate.
    maxWitnessesInSimpleScript :: SimpleScript -> Int
    maxWitnessesInSimpleScript (RequireSignature _) = 1
    maxWitnessesInSimpleScript (RequireTimeBefore _) = 0
    maxWitnessesInSimpleScript (RequireTimeAfter _) = 0
    maxWitnessesInSimpleScript (RequireAllOf simpleScripts) = sum $ map maxWitnessesInSimpleScript simpleScripts
    maxWitnessesInSimpleScript (RequireAnyOf simpleScripts) = maximum $ map maxWitnessesInSimpleScript simpleScripts
    maxWitnessesInSimpleScript (RequireMOf n simpleScripts) = sum $ take n $ sortBy (comparing Down) (map maxWitnessesInSimpleScript simpleScripts)

-- ----------------------------------------------------------------------------
-- Script execution units
--

type PlutusScriptBytes = ShortByteString

data ResolvablePointers where
  ResolvablePointers
    :: ( Ledger.Era (ShelleyLedgerEra era)
       , Show (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
       , Show (L.PlutusPurpose L.AsItem (ShelleyLedgerEra era))
       , Show (Alonzo.PlutusScript (ShelleyLedgerEra era))
       )
    => ShelleyBasedEra era
    -> !( Map
            (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
            ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
            , Maybe (PlutusScriptBytes, Plutus.Language)
            , Ledger.ScriptHash
            )
        )
    -> ResolvablePointers

deriving instance Show ResolvablePointers

-- | This data type represents the possible reasons for a script’s execution
-- failure, as reported by the  'evaluateTransactionExecutionUnits' function.
--
-- The first three errors relate to issues before executing the script,
-- while the last two arise during script execution.
--
-- TODO: Consider replacing @ScriptWitnessIndex@ with the ledger’s @PlutusPurpose
-- AsIx ledgerera@. This change would require parameterizing the
-- @ScriptExecutionError@.
data ScriptExecutionError
  = -- | The script depends on a 'TxIn' that has not been provided in the
    -- given 'UTxO' subset. The given 'UTxO' must cover all the inputs
    -- the transaction references.
    ScriptErrorMissingTxIn TxIn
  | -- | The 'TxIn' the script is spending does not have a 'ScriptDatum'.
    -- All inputs guarded by Plutus scripts need to have been created with
    -- a 'ScriptDatum'.
    ScriptErrorTxInWithoutDatum TxIn
  | -- | The 'ScriptDatum' provided does not match the one from the 'UTxO'.
    -- This means the wrong 'ScriptDatum' value has been provided.
    ScriptErrorWrongDatum (Hash ScriptData)
  | -- | The script evaluation failed. This usually means it evaluated to an
    -- error value. This is not a case of running out of execution units
    -- (which is not possible for 'evaluateTransactionExecutionUnits' since
    -- the whole point of it is to discover how many execution units are
    -- needed).
    ScriptErrorEvaluationFailed DebugPlutusFailure
  | -- | The execution units overflowed a 64bit word. Congratulations if
    -- you encounter this error. With the current style of cost model this
    -- would need a script to run for over 7 months, which is somewhat more
    -- than the expected maximum of a few milliseconds.
    ScriptErrorExecutionUnitsOverflow
  | -- | An attempt was made to spend a key witnessed tx input
    -- with a script witness.
    ScriptErrorNotPlutusWitnessedTxIn ScriptWitnessIndex ScriptHash
  | -- | The redeemer pointer points to a script hash that does not exist
    -- in the transaction nor in the UTxO as a reference script"
    ScriptErrorRedeemerPointsToUnknownScriptHash ScriptWitnessIndex
  | -- | A redeemer pointer points to a script that does not exist.
    ScriptErrorMissingScript
      ScriptWitnessIndex -- The invalid pointer
      ResolvablePointers -- A mapping a pointers that are possible to resolve
  | -- | A cost model was missing for a language which was used.
    ScriptErrorMissingCostModel Plutus.Language
  | forall era.
    ( Plutus.EraPlutusContext (ShelleyLedgerEra era)
    , Show (Plutus.ContextError (ShelleyLedgerEra era))
    ) =>
    ScriptErrorTranslationError (Plutus.ContextError (ShelleyLedgerEra era))

deriving instance Show ScriptExecutionError

instance Error ScriptExecutionError where
  prettyError = \case
    ScriptErrorMissingTxIn txin ->
      "The supplied UTxO is missing the txin " <> pretty (renderTxIn txin)
    ScriptErrorTxInWithoutDatum txin ->
      mconcat
        [ "The Plutus script witness for the txin does not have a script datum "
        , "(according to the UTxO). The txin in question is "
        , pretty (renderTxIn txin)
        ]
    ScriptErrorWrongDatum dh ->
      mconcat
        [ "The Plutus script witness has the wrong datum (according to the UTxO). "
        , "The expected datum value has hash " <> pshow dh
        ]
    ScriptErrorEvaluationFailed plutusDebugFailure ->
      pretty $ renderDebugPlutusFailure plutusDebugFailure
    ScriptErrorExecutionUnitsOverflow ->
      mconcat
        [ "The execution units required by this Plutus script overflows a 64bit "
        , "word. In a properly configured chain this should be practically "
        , "impossible. So this probably indicates a chain configuration problem, "
        , "perhaps with the values in the cost model."
        ]
    ScriptErrorNotPlutusWitnessedTxIn scriptWitness scriptHash ->
      mconcat
        [ pretty (renderScriptWitnessIndex scriptWitness)
        , " is not a Plutus script witnessed tx input and cannot be spent using a "
        , "Plutus script witness.The script hash is " <> pshow scriptHash <> "."
        ]
    ScriptErrorRedeemerPointsToUnknownScriptHash scriptWitness ->
      mconcat
        [ pretty (renderScriptWitnessIndex scriptWitness)
        , " points to a script hash that is not known."
        ]
    ScriptErrorMissingScript rdmrPtr resolveable ->
      mconcat
        [ "The redeemer pointer: " <> pshow rdmrPtr <> " points to a Plutus "
        , "script that does not exist.\n"
        , "The pointers that can be resolved are: " <> pshow resolveable
        ]
    ScriptErrorMissingCostModel language ->
      "No cost model was found for language " <> pshow language
    ScriptErrorTranslationError e ->
      "Error translating the transaction context: " <> pshow e

data TransactionValidityError era where
  -- | The transaction validity interval is too far into the future.
  --
  -- Transactions containing Plutus scripts must have a validity interval that is
  -- not excessively far in the future. This ensures that the UTC
  -- corresponding to the validity interval expressed in slot numbers,
  -- can be reliably determined.
  --
  -- Plutus scripts are given the transaction validity interval in UTC to
  -- prevent sensitivity to variations in slot lengths.
  --
  -- If either end of the validity interval exceeds the \"time horizon\", the
  -- consensus algorithm cannot reliably establish the relationship between
  -- slots and time.
  --
  -- This error occurs when thevalidity interval exceeds the time horizon.
  -- For the Cardano mainnet, the time horizon is set to 36 hours beyond the
  -- current time. This effectively restricts the submission and validation
  -- of transactions that include Plutus scripts if the end of their validity
  -- interval extends more than 36 hours into the future.
  TransactionValidityIntervalError
    :: Consensus.PastHorizonException -> TransactionValidityError era
  TransactionValidityCostModelError
    :: (Map AnyPlutusScriptVersion CostModel) -> String -> TransactionValidityError era

deriving instance Show (TransactionValidityError era)

instance Error (TransactionValidityError era) where
  prettyError = \case
    TransactionValidityIntervalError pastTimeHorizon ->
      mconcat
        [ "The transaction validity interval is too far in the future. "
        , "For this network it must not be more than "
        , pretty (timeHorizonSlots pastTimeHorizon)
        , "slots ahead of the current time slot. "
        , "(Transactions with Plutus scripts must have validity intervals that "
        , "are close enough in the future that we can reliably turn the slot "
        , "numbers into UTC wall clock times.)"
        ]
     where
      timeHorizonSlots :: Consensus.PastHorizonException -> Word
      timeHorizonSlots Consensus.PastHorizon{Consensus.pastHorizonSummary}
        | eraSummaries@(_ : _) <- pastHorizonSummary
        , Consensus.StandardSafeZone slots <-
            (Consensus.eraSafeZone . Consensus.eraParams . last) eraSummaries =
            fromIntegral slots
        | otherwise =
            0 -- This should be impossible.
    TransactionValidityCostModelError cModels err ->
      mconcat
        [ "An error occurred while converting from the cardano-api cost"
        , " models to the cardano-ledger cost models. Error: " <> pretty err
        , " Cost models: " <> pshow cModels
        ]

-- | Compute the 'ExecutionUnits' required for each script in the transaction.
--
-- This process involves executing all scripts and counting the actual execution units
-- consumed.
evaluateTransactionExecutionUnits
  :: forall era
   . ()
  => CardanoEra era
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters era
  -> UTxO era
  -> TxBody era
  -> Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
evaluateTransactionExecutionUnits era systemstart epochInfo pp utxo txbody =
  case makeSignedTransaction' era [] txbody of
    ShelleyTx sbe tx' -> evaluateTransactionExecutionUnitsShelley sbe systemstart epochInfo pp utxo tx'

evaluateTransactionExecutionUnitsShelley
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters era
  -> UTxO era
  -> L.Tx (ShelleyLedgerEra era)
  -> Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
evaluateTransactionExecutionUnitsShelley sbe systemstart epochInfo (LedgerProtocolParameters pp) utxo tx =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const Map.empty)
    ( \w ->
        fromLedgerScriptExUnitsMap w $
          alonzoEraOnwardsConstraints w $
            L.evalTxExUnitsWithLogs pp tx (toLedgerUTxO sbe utxo) ledgerEpochInfo systemstart
    )
    sbe
 where
  LedgerEpochInfo ledgerEpochInfo = epochInfo

  fromLedgerScriptExUnitsMap
    :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
    => AlonzoEraOnwards era
    -> Map
         (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
         (Either (L.TransactionScriptFailure (ShelleyLedgerEra era)) (EvalTxExecutionUnitsLog, Alonzo.ExUnits))
    -> Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
  fromLedgerScriptExUnitsMap aOnwards exmap =
    fromList
      [ ( toScriptIndex aOnwards rdmrptr
        , bimap (fromAlonzoScriptExecutionError aOnwards) (second fromAlonzoExUnits) exunitsOrFailure
        )
      | (rdmrptr, exunitsOrFailure) <- toList exmap
      ]

  fromAlonzoScriptExecutionError
    :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
    => AlonzoEraOnwards era
    -> L.TransactionScriptFailure (ShelleyLedgerEra era)
    -> ScriptExecutionError
  fromAlonzoScriptExecutionError aOnwards =
    shelleyBasedEraConstraints sbe $ \case
      L.UnknownTxIn txin -> ScriptErrorMissingTxIn txin'
       where
        txin' = fromShelleyTxIn txin
      L.InvalidTxIn txin -> ScriptErrorTxInWithoutDatum txin'
       where
        txin' = fromShelleyTxIn txin
      L.MissingDatum dh -> ScriptErrorWrongDatum (ScriptDataHash dh)
      L.ValidationFailure execUnits evalErr logs scriptWithContext ->
        ScriptErrorEvaluationFailed $ DebugPlutusFailure evalErr scriptWithContext execUnits logs
      L.IncompatibleBudget _ -> ScriptErrorExecutionUnitsOverflow
      L.RedeemerPointsToUnknownScriptHash rdmrPtr ->
        ScriptErrorRedeemerPointsToUnknownScriptHash $ toScriptIndex aOnwards rdmrPtr
      -- This should not occur while using cardano-cli because we zip together
      -- the Plutus script and the use site (txin, certificate etc). Therefore
      -- the redeemer pointer will always point to a Plutus script.
      L.MissingScript indexOfScriptWitnessedItem resolveable ->
        let scriptWitnessedItemIndex = toScriptIndex aOnwards indexOfScriptWitnessedItem
         in ScriptErrorMissingScript scriptWitnessedItemIndex $
              ResolvablePointers sbe $
                Map.map extractScriptBytesAndLanguage resolveable
      L.NoCostModelInLedgerState l -> ScriptErrorMissingCostModel l
      L.ContextError e ->
        alonzoEraOnwardsConstraints aOnwards $
          ScriptErrorTranslationError e

extractScriptBytesAndLanguage
  :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
  => ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
     , Maybe (Alonzo.PlutusScript (ShelleyLedgerEra era))
     , L.ScriptHash
     )
  -> ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
     , Maybe (PlutusScriptBytes, Plutus.Language)
     , Ledger.ScriptHash
     )
extractScriptBytesAndLanguage (purpose, mbScript, scriptHash) =
  (purpose, fmap extractPlutusScriptAndLanguage mbScript, scriptHash)

extractPlutusScriptAndLanguage
  :: Alonzo.AlonzoEraScript (ShelleyLedgerEra era)
  => Alonzo.PlutusScript (ShelleyLedgerEra era)
  -> (PlutusScriptBytes, Plutus.Language)
extractPlutusScriptAndLanguage p =
  let bin = Plutus.unPlutusBinary $ Alonzo.plutusScriptBinary p
      l = Alonzo.plutusScriptLanguage p
   in (bin, l)

-- ----------------------------------------------------------------------------
-- Transaction balance
--

-- | Compute the total balance of the proposed transaction. Ultimately, a valid
-- transaction must be fully balanced, which means that it has a total value
-- of zero.
--
-- Finding the (non-zero) balance of a partially constructed transaction is
-- useful for adjusting a transaction to be fully balanced.
evaluateTransactionBalance
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> Set PoolId
  -> Map StakeCredential L.Coin
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -> UTxO era
  -> TxBody era
  -> TxOutValue era
evaluateTransactionBalance sbe pp poolids stakeDelegDeposits drepDelegDeposits utxo (ShelleyTxBody _ txbody _ _ _ _) =
  shelleyBasedEraConstraints sbe $
    TxOutValueShelleyBased sbe $
      L.evalBalanceTxBody
        pp
        lookupDelegDeposit
        lookupDRepDeposit
        isRegPool
        (toLedgerUTxO sbe utxo)
        txbody
 where
  isRegPool :: Ledger.KeyHash Ledger.StakePool -> Bool
  isRegPool kh = StakePoolKeyHash kh `Set.member` poolids

  lookupDelegDeposit
    :: Ledger.Credential 'Ledger.Staking -> Maybe L.Coin
  lookupDelegDeposit stakeCred =
    Map.lookup (fromShelleyStakeCredential stakeCred) stakeDelegDeposits

  lookupDRepDeposit
    :: Ledger.Credential 'Ledger.DRepRole -> Maybe L.Coin
  lookupDRepDeposit drepCred =
    Map.lookup drepCred drepDelegDeposits

-- ----------------------------------------------------------------------------
-- Automated transaction building
--

-- | The possible errors that can arise from 'makeTransactionBodyAutoBalance'.
data TxBodyErrorAutoBalance era
  = -- | The same errors that can arise from 'makeTransactionBody'.
    TxBodyError TxBodyError
  | -- | One or more scripts failed to execute correctly.
    TxBodyScriptExecutionError [(ScriptWitnessIndex, ScriptExecutionError)]
  | -- | One or more scripts were expected to fail validation, but none did.
    TxBodyScriptBadScriptValidity
  | -- | There is not enough ada and non-ada to cover both the outputs and the fees.
    -- The transaction should be changed to provide more input assets, or
    -- otherwise adjusted to need less (e.g. outputs, script etc).
    TxBodyErrorBalanceNegative L.Coin L.MultiAsset
  | -- | There is enough ada to cover both the outputs and the fees, but the
    -- resulting change is too small: it is under the minimum value for
    -- new UTXO entries. The transaction should be changed to provide more
    -- input ada.
    TxBodyErrorAdaBalanceTooSmall
      TxOutInAnyEra
      -- ^ Offending TxOut
      L.Coin
      -- ^ Minimum UTxO
      L.Coin
      -- ^ Tx balance
  | -- | 'makeTransactionBodyAutoBalance' does not yet support the Byron era.
    TxBodyErrorByronEraNotSupported
  | -- | The 'ProtocolParameters' must provide the value for the min utxo
    -- parameter, for eras that use this parameter.
    TxBodyErrorMissingParamMinUTxO
  | -- | The minimum spendable UTxO threshold has not been met.
    TxBodyErrorMinUTxONotMet
      TxOutInAnyEra
      -- ^ Offending TxOut
      L.Coin
      -- ^ Minimum UTXO
  | TxBodyErrorNonAdaAssetsUnbalanced Value
  | TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap
      ScriptWitnessIndex
      (Map ScriptWitnessIndex ExecutionUnits)
  deriving Show

instance Error (TxBodyErrorAutoBalance era) where
  prettyError = \case
    TxBodyError err ->
      prettyError err
    TxBodyScriptExecutionError failures ->
      mconcat
        [ "The following scripts have execution failures:\n"
        , vsep
            [ mconcat
                [ "the script for " <> pretty (renderScriptWitnessIndex index)
                , " failed with: " <> "\n" <> prettyError failure
                ]
            | (index, failure) <- failures
            ]
        ]
    TxBodyScriptBadScriptValidity ->
      "One or more of the scripts were expected to fail validation, but none did."
    TxBodyErrorBalanceNegative lovelace assets ->
      mconcat $
        [ "The transaction does not balance in its use of assets. The net balance "
        , "of the transaction is negative: "
        ]
          <> punctuate ", " ([pretty lovelace] <> [pretty assets | assets /= mempty])
          <> [ ". The usual solution is to provide more inputs, or inputs with more assets."
             ]
    TxBodyErrorAdaBalanceTooSmall changeOutput minUTxO balance ->
      mconcat
        [ "The transaction does balance in its use of ada, however the net "
        , "balance does not meet the minimum UTxO threshold. \n"
        , "Balance: " <> pretty balance <> "\n"
        , "Offending output (change output): " <> pretty (prettyRenderTxOut changeOutput) <> "\n"
        , "Minimum UTxO threshold: " <> pretty minUTxO <> "\n"
        , "The usual solution is to provide more inputs, or inputs with more ada to "
        , "meet the minimum UTxO threshold"
        ]
    TxBodyErrorByronEraNotSupported ->
      "The Byron era is not yet supported by makeTransactionBodyAutoBalance"
    TxBodyErrorMissingParamMinUTxO ->
      "The minUTxOValue protocol parameter is required but missing"
    TxBodyErrorMinUTxONotMet txout minUTxO ->
      mconcat
        [ "Minimum UTxO threshold not met for tx output: " <> pretty (prettyRenderTxOut txout) <> "\n"
        , "Minimum required UTxO: " <> pretty minUTxO
        ]
    TxBodyErrorNonAdaAssetsUnbalanced val ->
      "Non-Ada assets are unbalanced: " <> pretty (renderValue val)
    TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap sIndex eUnitsMap ->
      mconcat
        [ "ScriptWitnessIndex (redeemer pointer): " <> pshow sIndex <> " is missing from the execution "
        , "units (redeemer pointer) map: " <> pshow eUnitsMap
        ]

handleExUnitsErrors
  :: ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> Map ScriptWitnessIndex ScriptExecutionError
  -> Map ScriptWitnessIndex ExecutionUnits
  -> Either (TxBodyErrorAutoBalance era) (Map ScriptWitnessIndex ExecutionUnits)
handleExUnitsErrors ScriptValid failuresMap exUnitsMap =
  if null failures
    then Right exUnitsMap
    else Left (TxBodyScriptExecutionError failures)
 where
  failures :: [(ScriptWitnessIndex, ScriptExecutionError)]
  failures = toList failuresMap
handleExUnitsErrors ScriptInvalid failuresMap exUnitsMap
  | null failuresMap = Left TxBodyScriptBadScriptValidity
  | otherwise = Right $ Map.map (\_ -> ExecutionUnits 0 0) failuresMap <> exUnitsMap

data BalancedTxBody era
  = BalancedTxBody
      (TxBodyContent BuildTx era)
      (TxBody era)
      (TxOut CtxTx era)
      -- ^ Transaction balance (change output)
      L.Coin
      -- ^ Estimated transaction fee
  deriving Show

newtype RequiredShelleyKeyWitnesses
  = RequiredShelleyKeyWitnesses {unRequiredShelleyKeyWitnesses :: Int}
  deriving Show

newtype RequiredByronKeyWitnesses
  = RequiredByronKeyWitnesses {unRequiredByronKeyWitnesses :: Int}
  deriving Show

newtype TotalReferenceScriptsSize
  = TotalReferenceScriptsSize {unTotalReferenceScriptsSize :: Int}
  deriving Show

data FeeEstimationMode era
  = -- | Accurate fee calculation.
    CalculateWithSpendableUTxO
      (UTxO era)
      -- ^ Spendable UTxO
      SystemStart
      LedgerEpochInfo
      (Maybe Word)
      -- ^ Override number of key witnesses
  | -- | Less accurate fee estimation.
    EstimateWithoutSpendableUTxO
      Coin
      -- ^ Total potential collateral amount
      Value
      -- ^ Total value of UTxOs being spent
      (Map ScriptWitnessIndex ExecutionUnits)
      -- ^ Plutus script execution units
      RequiredShelleyKeyWitnesses
      -- ^ The number of key witnesses to be added to the transaction.
      RequiredByronKeyWitnesses
      -- ^ The number of Byron key witnesses to be added to the transaction.
      TotalReferenceScriptsSize
      -- ^ The total size in bytes of reference scripts

-- | This is similar to 'makeTransactionBody' but with greater automation to
-- calculate suitable values for several things.
--
-- In particular:
--
-- * It calculates the correct script 'ExecutionUnits' (ignoring the provided
--   values, which can thus be zero).
--
-- * It calculates the transaction fees based on the script 'ExecutionUnits',
--   the current 'ProtocolParameters', and an estimate of the number of
--   key witnesses (i.e. signatures). There is an override for the number of
--   key witnesses.
--
-- * It accepts a change address, calculates the balance of the transaction
--   and puts the excess change into the change output.
--
-- * It also checks that the balance is positive and the change is above the
--   minimum threshold.
--
-- To do this, it requires more information than 'makeTransactionBody', all of
-- which can be queried from a local node.
makeTransactionBodyAutoBalance
  :: forall era
   . ()
  => HasCallStack
  => ShelleyBasedEra era
  -> SystemStart
  -> LedgerEpochInfo
  -> LedgerProtocolParameters era
  -> Set PoolId
  -- ^ The set of registered stake pools, being
  --   unregistered in this transaction.
  -> Map StakeCredential L.Coin
  -- ^ The map of all deposits for stake credentials that are being
  --   unregistered in this transaction
  -> Map (Ledger.Credential Ledger.DRepRole) L.Coin
  -- ^ The map of all deposits for DRep credentials that are being
  --   unregistered in this transaction
  -> UTxO era
  -- ^ The transaction inputs (including reference and collateral ones), not the entire 'UTxO'.
  -> TxBodyContent BuildTx era
  -> AddressInEra era
  -- ^ Change address
  -> Maybe Word
  -- ^ Override key witnesses
  -> Either (TxBodyErrorAutoBalance era) (BalancedTxBody era)
makeTransactionBodyAutoBalance
  sbe
  systemstart
  history
  lpp@(LedgerProtocolParameters pp)
  poolids
  stakeDelegDeposits
  drepDelegDeposits
  utxo
  txbodycontent
  changeaddr
  mnkeys =
    shelleyBasedEraConstraints sbe $ do
      -- Our strategy is to:
      -- 1. evaluate all the scripts to get the exec units, update with ex units
      -- 2. figure out the overall min fees
      -- 3. update tx with fees
      -- 4. balance the transaction and update tx change output

      txbodyForChange <- first TxBodyError $ createTransactionBody sbe txbodycontent
      let initialChangeTxOutValue =
            evaluateTransactionBalance sbe pp poolids stakeDelegDeposits drepDelegDeposits utxo txbodyForChange
          initialChangeTxOut =
            TxOut
              changeaddr
              initialChangeTxOutValue
              TxOutDatumNone
              ReferenceScriptNone

      -- Initial change is only used for execution units evaluation, so we don't require minimum UTXO requirement
      -- to be satisfied at this point
      _ <- checkNonNegative sbe pp initialChangeTxOut

      -- Tx body used only for evaluating execution units. Because txout exact
      -- values do not matter much here, we are using an initial change value,
      -- which is slightly overestimated, because it does not include fee or
      -- scripts execution costs.
      txbody <-
        first TxBodyError
          $ createTransactionBody
            sbe
          $ txbodycontent
            & modTxOuts
              (<> [initialChangeTxOut])
      let exUnitsMapWithLogs =
            evaluateTransactionExecutionUnits
              era
              systemstart
              history
              lpp
              utxo
              txbody

      let exUnitsMap = Map.map (fmap snd) exUnitsMapWithLogs

      exUnitsMap' <-
        case Map.mapEither id exUnitsMap of
          (failures, exUnitsMap') ->
            handleExUnitsErrors
              (txScriptValidityToScriptValidity (txScriptValidity txbodycontent))
              failures
              exUnitsMap'

      txbodycontent1 <- substituteExecutionUnits exUnitsMap' txbodycontent

      -- Make a txbody that we will use for calculating the fees. For the purpose
      -- of fees we just need to make a txbody of the right size in bytes. We
      -- do not need the right values for the fee. We use "big enough" value
      -- for the fee and set so that the CBOR encoding size of the tx will be
      -- big enough to cover the size of the final output and fee. Yes this
      -- means this current code will only work for final fee of less than
      -- around 4000 ada (2^32-1 lovelace).
      let maxLovelaceFee = L.Coin (2 ^ (32 :: Integer) - 1)
      -- Make a txbody that we will use for calculating the fees.
      let (dummyCollRet, dummyTotColl) = maybeDummyTotalCollAndCollReturnOutput sbe txbodycontent changeaddr
      txbody1 <-
        first TxBodyError $ -- TODO: impossible to fail now
          createTransactionBody
            sbe
            txbodycontent1
              { txFee = TxFeeExplicit sbe maxLovelaceFee
              , txOuts =
                  txOuts txbodycontent
                    <> [initialChangeTxOut]
              , txReturnCollateral = dummyCollRet
              , txTotalCollateral = dummyTotColl
              }
      -- NB: This has the potential to over estimate the fees because estimateTransactionKeyWitnessCount
      -- makes the conservative assumption that all inputs are from distinct
      -- addresses.
      let nkeys =
            fromMaybe
              (estimateTransactionKeyWitnessCount txbodycontent1)
              mnkeys
          fee = calculateMinTxFee sbe pp utxo txbody1 nkeys
          (retColl, reqCol) =
            caseShelleyToAlonzoOrBabbageEraOnwards
              (const (TxReturnCollateralNone, TxTotalCollateralNone))
              ( \w -> do
                  let totalPotentialCollateral =
                        mconcat
                          [ txOutValue
                          | TxInsCollateral _ collInputs <- pure $ txInsCollateral txbodycontent
                          , collTxIn <- collInputs
                          , Just (TxOut _ (TxOutValueShelleyBased _ txOutValue) _ _) <- pure $ Map.lookup collTxIn (unUTxO utxo)
                          ]
                  calcReturnAndTotalCollateral
                    w
                    fee
                    pp
                    (txInsCollateral txbodycontent)
                    (txReturnCollateral txbodycontent)
                    (txTotalCollateral txbodycontent)
                    changeaddr
                    totalPotentialCollateral
              )
              sbe

      -- Make a txbody for calculating the balance. For this the size of the tx
      -- does not matter, instead it's just the values of the fee and outputs.
      -- Here we do not want to start with any change output, since that's what
      -- we need to calculate.
      txbody2 <-
        first TxBodyError $ -- TODO: impossible to fail now
          createTransactionBody
            sbe
            txbodycontent1
              { txFee = TxFeeExplicit sbe fee
              , txReturnCollateral = retColl
              , txTotalCollateral = reqCol
              }
      let balance = evaluateTransactionBalance sbe pp poolids stakeDelegDeposits drepDelegDeposits utxo txbody2
          balanceTxOut = TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone
      first (uncurry TxBodyErrorMinUTxONotMet)
        . mapM_ (checkMinUTxOValue sbe pp)
        $ txOuts txbodycontent1

      -- check if change meets txout criteria, and include if non-zero
      finalTxOuts <- checkAndIncludeChange sbe pp balanceTxOut (txOuts txbodycontent1)

      -- TODO: we could add the extra fee for the CBOR encoding of the change,
      -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.
      -- The txbody with the final fee and change output. This should work
      -- provided that the fee and change are less than 2^32-1, and so will
      -- fit within the encoding size we picked above when calculating the fee.
      -- Yes this could be an over-estimate by a few bytes if the fee or change
      -- would fit within 2^16-1. That's a possible optimisation.
      let finalTxBodyContent =
            txbodycontent1
              { txFee = TxFeeExplicit sbe fee
              , txOuts = finalTxOuts
              , txReturnCollateral = retColl
              , txTotalCollateral = reqCol
              }
      txbody3 <-
        first TxBodyError $ -- TODO: impossible to fail now. We need to implement a function
        -- that simply creates a transaction body because we have already
        -- validated the transaction body earlier within makeTransactionBodyAutoBalance
          createTransactionBody sbe finalTxBodyContent
      return
        ( BalancedTxBody
            finalTxBodyContent
            txbody3
            balanceTxOut
            fee
        )
   where
    era :: CardanoEra era
    era = toCardanoEra sbe

-- | In the event of spending the exact amount of lovelace and non-ada assets in
-- the specified input(s), this function excludes the change
-- output. Note that this does not save any fees because by default
-- the fee calculation includes a change address for simplicity and
-- we make no attempt to recalculate the tx fee without a change address.
checkAndIncludeChange
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxOut CtxTx era
  -> [TxOut CtxTx era]
  -> Either (TxBodyErrorAutoBalance era) [TxOut CtxTx era]
checkAndIncludeChange sbe pp change@(TxOut _ changeValue _ _) rest = do
  isChangeEmpty <- checkNonNegative sbe pp change
  if isChangeEmpty == Empty
    then pure rest
    else do
      let coin = txOutValueToLovelace changeValue
      first ((coin &) . uncurry TxBodyErrorAdaBalanceTooSmall) $
        checkMinUTxOValue sbe pp change
      -- We append change at the end so a client can predict the indexes of the outputs.
      pure $ rest <> [change]

checkMinUTxOValue
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxOut CtxTx era
  -> Either (TxOutInAnyEra, Coin) ()
  -- ^ @Left (offending txout, minimum required utxo)@ or @Right ()@ when txout is ok
checkMinUTxOValue sbe bpp txout@(TxOut _ v _ _) = do
  let minUTxO = calculateMinimumUTxO sbe bpp txout
  if txOutValueToLovelace v >= minUTxO
    then Right ()
    else Left (txOutInAnyEra (toCardanoEra sbe) txout, minUTxO)

data IsEmpty = Empty | NonEmpty
  deriving (Eq, Show)

checkNonNegative
  :: ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxOut CtxTx era
  -> Either (TxBodyErrorAutoBalance era) IsEmpty
  -- ^ result of check if txout is empty
checkNonNegative sbe bpparams txout@(TxOut _ balance _ _) = do
  let outValue@(L.MaryValue coin multiAsset) = toMaryValue $ txOutValueToValue balance
      isPositiveValue = L.pointwise (>) outValue mempty
  if
    | L.isZero outValue -> pure Empty -- empty TxOut - ok, it's removed at the end
    | L.isZero coin ->
        -- no ADA, just non-ADA assets: positive lovelace is required in such case
        Left $
          TxBodyErrorAdaBalanceTooSmall
            (TxOutInAnyEra (toCardanoEra sbe) txout)
            (calculateMinimumUTxO sbe bpparams txout)
            coin
    | not isPositiveValue -> Left $ TxBodyErrorBalanceNegative coin multiAsset
    | otherwise -> pure NonEmpty

-- Calculation taken from validateInsufficientCollateral:
-- https://github.com/input-output-hk/cardano-ledger/blob/389b266d6226dedf3d2aec7af640b3ca4984c5ea/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Rules/Utxo.hs#L335
-- TODO: Bug Jared to expose a function from the ledger that returns total and
-- return collateral.
calcReturnAndTotalCollateral
  :: ()
  => Ledger.AlonzoEraPParams (ShelleyLedgerEra era)
  => BabbageEraOnwards era
  -> L.Coin
  -- ^ Fee
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxInsCollateral era
  -- ^ From the initial TxBodyContent
  -> TxReturnCollateral CtxTx era
  -- ^ From the initial TxBodyContent
  -> TxTotalCollateral era
  -- ^ From the initial TxBodyContent
  -> AddressInEra era
  -- ^ Change address
  -> L.Value (ShelleyLedgerEra era)
  -- ^ Total available collateral (can include non-ada)
  -> (TxReturnCollateral CtxTx era, TxTotalCollateral era)
calcReturnAndTotalCollateral _ _ _ TxInsCollateralNone _ _ _ _ = (TxReturnCollateralNone, TxTotalCollateralNone)
calcReturnAndTotalCollateral w fee pp' TxInsCollateral{} txReturnCollateral txTotalCollateral cAddr totalAvailableCollateral = babbageEraOnwardsConstraints w $ do
  let sbe = convert w
      colPerc = pp' ^. Ledger.ppCollateralPercentageL
      -- We must first figure out how much lovelace we have committed
      -- as collateral and we must determine if we have enough lovelace at our
      -- collateral tx inputs to cover the tx
      totalCollateralLovelace = totalAvailableCollateral ^. A.adaAssetL sbe
      requiredCollateral@(L.Coin reqAmt) = fromIntegral colPerc * fee
      totalCollateral =
        TxTotalCollateral w . L.rationalToCoinViaCeiling $
          reqAmt % 100
      -- Why * 100? requiredCollateral is the product of the collateral percentage and the tx fee
      -- We choose to multiply 100 rather than divide by 100 to make the calculation
      -- easier to manage. At the end of the calculation we then use % 100 to perform our division
      -- and round the returnCollateral down which has the effect of potentially slightly
      -- overestimating the required collateral.
      L.Coin returnCollateralAmount = totalCollateralLovelace * 100 - requiredCollateral
      returnAdaCollateral = A.mkAdaValue sbe $ L.rationalToCoinViaFloor $ returnCollateralAmount % 100
      -- non-ada collateral is not used, so just return it as is in the return collateral output
      nonAdaCollateral = L.modifyCoin (const mempty) totalAvailableCollateral
      returnCollateral = returnAdaCollateral <> nonAdaCollateral
  case (txReturnCollateral, txTotalCollateral) of
    (rc@TxReturnCollateral{}, tc@TxTotalCollateral{}) ->
      (rc, tc)
    (rc@TxReturnCollateral{}, TxTotalCollateralNone) ->
      (rc, TxTotalCollateralNone)
    (TxReturnCollateralNone, tc@TxTotalCollateral{}) ->
      (TxReturnCollateralNone, tc)
    (TxReturnCollateralNone, TxTotalCollateralNone)
      | returnCollateralAmount < 0 ->
          (TxReturnCollateralNone, TxTotalCollateralNone)
      | otherwise ->
          ( TxReturnCollateral
              w
              ( TxOut
                  cAddr
                  (TxOutValueShelleyBased sbe returnCollateral)
                  TxOutDatumNone
                  ReferenceScriptNone
              )
          , totalCollateral
          )

-- | Calculate the partial change - this does not include certificates' deposits
calculatePartialChangeValue
  :: ShelleyBasedEra era
  -> Value
  -> TxBodyContent build era
  -> Value
calculatePartialChangeValue sbe incoming txbodycontent = do
  let outgoing = newUtxoValue
      mintedValue = txMintValueToValue $ txMintValue txbodycontent
  mconcat [incoming, mintedValue, negateValue outgoing]
 where
  newUtxoValue =
    mconcat [fromLedgerValue sbe v | (TxOut _ (TxOutValueShelleyBased _ v) _ _) <- txOuts txbodycontent]

-- | This is used in the balance calculation in the event where
-- the user does not supply the UTxO(s) they intend to spend
-- but they must supply their total balance of ADA.
-- evaluateTransactionBalance calls evalBalanceTxBody which requires a UTxO value.
-- This eventually calls getConsumedMaryValue which retrieves the balance
-- from the transaction itself. This necessitated a function to create a "fake" UTxO
-- to still use evalBalanceTxBody however this will fail for transactions
-- containing multi-assets, refunds and withdrawals.
-- TODO: Include multiassets
createFakeUTxO :: ShelleyBasedEra era -> TxBodyContent BuildTx era -> Coin -> UTxO era
createFakeUTxO sbe txbodycontent totalAdaInUTxO =
  let singleTxIn = maybe [] (return . fst) $ List.uncons [txin | (txin, _) <- txIns txbodycontent]
      singleTxOut =
        maybe [] (return . updateTxOut sbe totalAdaInUTxO . toCtxUTxOTxOut . fst) $
          List.uncons $
            txOuts txbodycontent
   in -- Take one txin and one txout. Replace the out value with totalAdaInUTxO
      -- Return an empty UTxO if there are no txins or txouts
      UTxO $ fromList $ zip singleTxIn singleTxOut

updateTxOut :: ShelleyBasedEra era -> Coin -> TxOut CtxUTxO era -> TxOut CtxUTxO era
updateTxOut sbe updatedValue txout =
  let ledgerout = shelleyBasedEraConstraints sbe $ toShelleyTxOut sbe txout & L.coinTxOutL .~ updatedValue
   in fromShelleyTxOut sbe ledgerout

-- Essentially we check for the existence of collateral inputs. If they exist we
-- create a fictitious collateral return output. Why? Because we need to put dummy values
-- to get a fee estimate (i.e we overestimate the fee). The required collateral depends
-- on the tx fee as per the Alonzo spec.
maybeDummyTotalCollAndCollReturnOutput
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> AddressInEra era
  -> (TxReturnCollateral CtxTx era, TxTotalCollateral era)
maybeDummyTotalCollAndCollReturnOutput sbe TxBodyContent{txInsCollateral, txReturnCollateral, txTotalCollateral} cAddr =
  case txInsCollateral of
    TxInsCollateralNone -> (TxReturnCollateralNone, TxTotalCollateralNone)
    TxInsCollateral{} ->
      forShelleyBasedEraInEon
        sbe
        (TxReturnCollateralNone, TxTotalCollateralNone)
        ( \w ->
            let dummyRetCol =
                  TxReturnCollateral
                    w
                    ( TxOut
                        cAddr
                        (lovelaceToTxOutValue sbe $ L.Coin (2 ^ (64 :: Integer)) - 1)
                        TxOutDatumNone
                        ReferenceScriptNone
                    )
                dummyTotCol = TxTotalCollateral w (L.Coin (2 ^ (32 :: Integer) - 1))
             in case (txReturnCollateral, txTotalCollateral) of
                  (rc@TxReturnCollateral{}, tc@TxTotalCollateral{}) -> (rc, tc)
                  (rc@TxReturnCollateral{}, TxTotalCollateralNone) -> (rc, dummyTotCol)
                  (TxReturnCollateralNone, tc@TxTotalCollateral{}) -> (dummyRetCol, tc)
                  (TxReturnCollateralNone, TxTotalCollateralNone) -> (dummyRetCol, dummyTotCol)
        )

substituteExecutionUnits
  :: forall era
   . Map ScriptWitnessIndex ExecutionUnits
  -> TxBodyContent BuildTx era
  -> Either (TxBodyErrorAutoBalance era) (TxBodyContent BuildTx era)
substituteExecutionUnits
  exUnitsMap
  txbodycontent@( TxBodyContent
                    txIns
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    txWithdrawals
                    txCertificates
                    _
                    txMintValue
                    _
                    txProposalProcedures
                    txVotingProcedures
                    _
                    _
                  ) = do
    mappedTxIns <- mapScriptWitnessesTxIns txIns
    mappedWithdrawals <- mapScriptWitnessesWithdrawals txWithdrawals
    mappedMintedVals <- mapScriptWitnessesMinting txMintValue
    mappedTxCertificates <- mapScriptWitnessesCertificates txCertificates
    mappedVotes <- mapScriptWitnessesVotes txVotingProcedures
    mappedProposals <- mapScriptWitnessesProposals txProposalProcedures

    Right $
      txbodycontent
        & setTxIns mappedTxIns
        & setTxMintValue mappedMintedVals
        & setTxCertificates mappedTxCertificates
        & setTxWithdrawals mappedWithdrawals
        & setTxVotingProcedures mappedVotes
        & setTxProposalProcedures mappedProposals
   where
    substituteExecUnits
      :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> Either (TxBodyErrorAutoBalance era) (ScriptWitness witctx era)
    substituteExecUnits _ wit@SimpleScriptWitness{} = Right wit
    substituteExecUnits idx (PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits ->
          Right $
            PlutusScriptWitness
              langInEra
              version
              script
              datum
              redeemer
              exunits

    adjustScriptWitness
      :: (ScriptWitness witctx era -> Either (TxBodyErrorAutoBalance era) (ScriptWitness witctx era))
      -> Witness witctx era
      -> Either (TxBodyErrorAutoBalance era) (Witness witctx era)
    adjustScriptWitness _ (KeyWitness ctx) = Right $ KeyWitness ctx
    adjustScriptWitness g (ScriptWitness ctx witness') = ScriptWitness ctx <$> g witness'

    mapScriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> Either (TxBodyErrorAutoBalance era) [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( TxIn
                 , Either (TxBodyErrorAutoBalance era) (BuildTxWith BuildTx (Witness WitCtxTxIn era))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, BuildTxWith <$> wit')
            | (ix, txin, wit) <- indexTxIns txins
            , let wit' = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in traverse
            (\(txIn, eWitness) -> (txIn,) <$> eWitness)
            mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxWithdrawals BuildTx era)
    mapScriptWitnessesWithdrawals TxWithdrawalsNone = Right TxWithdrawalsNone
    mapScriptWitnessesWithdrawals txWithdrawals'@(TxWithdrawals supported _) =
      let mappedWithdrawals
            :: [ ( StakeAddress
                 , L.Coin
                 , Either (TxBodyErrorAutoBalance era) (BuildTxWith BuildTx (Witness WitCtxStake era))
                 )
               ]
          mappedWithdrawals =
            [ (addr, withdrawal, BuildTxWith <$> mappedWitness)
            | (ix, addr, withdrawal, wit) <- indexTxWithdrawals txWithdrawals'
            , let mappedWitness = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in TxWithdrawals supported
            <$> traverse
              (\(sAddr, ll, eWitness) -> (sAddr,ll,) <$> eWitness)
              mappedWithdrawals

    mapScriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxCertificates BuildTx era)
    mapScriptWitnessesCertificates TxCertificatesNone = Right TxCertificatesNone
    mapScriptWitnessesCertificates txCertificates'@(TxCertificates supported _) = do
      let mappedScriptWitnesses
            :: [ ( Certificate era
                 , Either
                     (TxBodyErrorAutoBalance era)
                     ( BuildTxWith
                         BuildTx
                         ( Maybe
                             ( StakeCredential
                             , Witness WitCtxStake era
                             )
                         )
                     )
                 )
               ]
          mappedScriptWitnesses =
            [ (cert, BuildTxWith . Just . (stakeCred,) <$> eWitness')
            | (ix, cert, stakeCred, witness) <- indexTxCertificates txCertificates'
            , let eWitness' = adjustScriptWitness (substituteExecUnits ix) witness
            ]
      TxCertificates supported . fromList <$> traverseScriptWitnesses mappedScriptWitnesses

    mapScriptWitnessesVotes
      :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
      -> Either
           (TxBodyErrorAutoBalance era)
           (Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era)))
    mapScriptWitnessesVotes Nothing = return Nothing
    mapScriptWitnessesVotes (Just (Featured _ TxVotingProceduresNone)) = return Nothing
    mapScriptWitnessesVotes (Just (Featured _ (TxVotingProcedures _ ViewTx))) = return Nothing
    mapScriptWitnessesVotes (Just (Featured era txVotingProcedures'@(TxVotingProcedures vProcedures (BuildTxWith _)))) = do
      let eSubstitutedExecutionUnits =
            [ (vote, updatedWitness)
            | (ix, vote, witness) <- indexTxVotingProcedures txVotingProcedures'
            , let updatedWitness = substituteExecUnits ix witness
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return $
        Just
          (Featured era (TxVotingProcedures vProcedures (BuildTxWith $ fromList substitutedExecutionUnits)))

    mapScriptWitnessesProposals
      :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures BuildTx era))
      -> Either
           (TxBodyErrorAutoBalance era)
           (Maybe (Featured ConwayEraOnwards era (TxProposalProcedures BuildTx era)))
    mapScriptWitnessesProposals Nothing = return Nothing
    mapScriptWitnessesProposals (Just (Featured era proposals)) = do
      substitutedExecutionUnits <-
        traverse
          (bitraverse pure $ traverse $ uncurry substituteExecUnits)
          $ indexWitnessedTxProposalProcedures proposals
      pure $
        Just $
          Featured era $
            conwayEraOnwardsConstraints era $
              mkTxProposalProcedures substitutedExecutionUnits

    mapScriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxMintValue BuildTx era)
    mapScriptWitnessesMinting TxMintNone = Right TxMintNone
    mapScriptWitnessesMinting txMintValue'@(TxMintValue w _) = do
      let mappedScriptWitnesses =
            [ (policyId, (assets,) <$> substitutedWitness)
            | (ix, policyId, assets, BuildTxWith witness) <- indexTxMintValue txMintValue'
            , let substitutedWitness = BuildTxWith <$> substituteExecUnits ix witness
            ]
          -- merge map values, wit1 == wit2 will always hold
          mergeValues (assets1, wit1) (assets2, _wit2) = (assets1 <> assets2, wit1)
      final <- Map.fromListWith mergeValues <$> traverseScriptWitnesses mappedScriptWitnesses
      pure $ TxMintValue w final

traverseScriptWitnesses
  :: [(a, Either (TxBodyErrorAutoBalance era) b)]
  -> Either (TxBodyErrorAutoBalance era) [(a, b)]
traverseScriptWitnesses =
  traverse (\(item, eRes) -> eRes >>= (\res -> Right (item, res)))

calculateMinimumUTxO
  :: HasCallStack
  => ShelleyBasedEra era
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> TxOut CtxTx era
  -> L.Coin
calculateMinimumUTxO sbe pp txout =
  shelleyBasedEraConstraints sbe $
    let txOutWithMinCoin = L.setMinCoinTxOut pp (toShelleyTxOutAny sbe txout)
     in txOutWithMinCoin ^. L.coinTxOutL
