-- |
-- This module provides an experimental library interface that is intended
-- to replace the existing API. It is subject to dramatic changes, so use with caution.

module Cardano.Api.Experimental
  (
-- * Creating transactions using the new and the old API
-- |
-- Both the old and the new API can be used to create transactions, and
-- it is possible to transform a transaction created in one format to the other
-- since they have the same representation underneath. But we will be moving
-- towards using the new API and deprecating the old way, since the later is
-- simpler, closer to the ledger, and easier to maintain.
--
-- In both the new and the old API, in order to construct a transaction,
-- we need to construct a 'TxBodyContent', and we will need at least a
-- witness (for example, a 'ShelleyWitnessSigningKey'), to sign the transaction.
-- This hasn't changed.
-- 
-- In the following examples, we are using the following qualified modules:
--
-- @
-- import qualified Cardano.Api as Api                -- the general `cardano-api` exports (including the old API)
-- import qualified Cardano.Api.Script as Script      -- types related to scripts (Plutus and native)
-- import qualified Cardano.Api.Ledger as Ledger      -- cardano-ledger re-exports
-- import qualified Cardano.Api.Experimental as Exp   -- the experimental API
-- @

-- ** Creating a 'TxBodyContent'
-- |
-- 'TxBodyContent' datatype provides lots of fields, because transactions can serve multiple
-- purposes, but the function 'defaultTxBodyContent' exported from 'Cardano.Api' already provides
-- a base 'TxBodyContent' with all fields set to their default values that we can use as a starting
-- in order not to have to set all fields manually.
--
-- The 'defaultTxBodyContent' takes, as the only parameter, the 'ShelleyBasedEra' witness for the era
-- we are working with. For example, if we are working with the 'ConwayEra', we can use 'shelleyBasedEra'
-- available in 'Cardano.Api', as follows:
--
-- @
-- let sbe = Api.shelleyBasedEra :: Api.ShelleyBasedEra Api.ConwayEra
-- @
--
-- We can also derive it from 'ConwayEra' from 'Cardano.Api.Experimental' by using the 'convert' function:
-- 
-- @
-- let era = Exp.ConwayEra
-- let sbe = Api.convert era
-- @
--
-- Let's see what creating a simple transaction would look like.
--
-- First, we choose a transaction output to spend (a UTxO). We specify which UTxO to spend by
-- providing the transaction id and the index of the output in that transaction that we want
-- to spend.
--
-- To specify the transaction id, we can use the 'deserialiseFromRawBytesHex' function on the
-- hexadecimal representation of the hash of the transaction. For example:
--
-- @
-- let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
-- @
--
-- Of course, in real code, the failure case should be handled properly.
-- 
-- To specify the transaction index, we can use the 'TxIx' constructor. For example:
--
-- @
-- let srcTxIx = Api.TxIx 0
-- @
--
-- Now we combine both to create a 'TxIn' value and we pair it with a witness requirement using 'BuildTxWith' :
--
-- @
-- let txIn = ( Api.TxIn srcTxId srcTxIx
--            , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
--            )
-- @
--
-- Next, let's specify the address of the recipient of the transaction. If we have the Bech32 representation,
-- we can use the 'deserialiseAddress' function to get the 'AddressInEra' type. For example:
-- 
-- @
-- let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra eraAsType) "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
-- @
--
-- Now we can create a 'TxOut' value. For simplicity, we will assume the output is a simple payment output
-- of 10 ADA, with no datum or reference script attached to it:
--
-- @
-- let txOut = Api.TxOut
--               destAddress
--               (Api.TxOutValueShelleyBased sbe (Api.inject (Ledger.Coin 10_000_000)))
--               Api.TxOutDatumNone
--               Script.ReferenceScriptNone
-- @
-- 
-- We must also set the fee for the transaction. For example, let's set it to 2 ADA:
--
-- @
-- let txFee = Api.TxFeeExplicit sbe (Ledger.Coin 2_000_000)
-- @
--
-- Finally, we can create the 'TxBodyContent' by using the 'defaultTxBodyContent' function and 
-- putting everything together:
--
-- @
-- let txBodyContent = Api.defaultTxBodyContent sbe
--                      & Api.setTxIns [txIn]
--                      & Api.setTxOuts [txOut]
--                      & Api.setTxFee txFee
-- @
--
-- The 'txBodyContent' can now be used to create a transaction either using the old or the new API.

-- ** Creating a 'ShelleyWitnessSigningKey'
-- |
-- To sign the transaction, we need a witness. For example, a 'ShelleyWitnessSigningKey'.
--
-- There are several ways of doing this, and several ways of representing a signing key. But let us assume 
-- we have the bech32 representation of the signing key. In that case we can use the 'deserialiseFromBech32' function
-- as follows:
--
-- @
-- let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"
-- @
--
-- Then we simply wrap the signing key in a 'ShelleyWitnessSigningKey' constructor:
--
-- @
-- let witness = Api.WitnessPaymentKey signingKey
-- @
--
-- We could do it analogously if we wanted to use an extended key, for example, using 'AsPaymentExtendedKey' and 'WitnessPaymentExtendedKey'.

-- ** Creating a transaction using the old API
-- |
-- Now that we have a 'TxBodyContent' and a 'ShelleyWitnessSigningKey', we can create a transaction using the old API
-- easily. First, we create a transaction body using the 'createTransactionBody' function and the 'ShelleyBasedEra' witness
-- that we defined earlier.
--
-- We create the transaction body using the 'TransactionBodyContent' that we created earlier:
--
-- @
-- let (Right txBody) = Api.createTransactionBody sbe txBodyContent
-- @
--
-- Then, we sign the transaction using the 'signShelleyTransaction' function and the witness:
--
-- @
-- let oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [witness]
-- @
--
-- And that is it. We have a signed transaction.

-- ** Creating a transaction using the new API
-- |
-- Now, let's see how we can create a transaction using the new API. First, we create an 'UnsignedTx' using the 'makeUnsignedTx'
-- function and the 'Era' and 'TxBodyContent' that we defined earlier:
--
-- @
-- let (Right unsignedTx) = Exp.makeUnsignedTx era txBodyContent
-- @
--
-- Then we use the key witness to witness the current unsigned transaction using the 'makeKeyWitness' function:
--
-- @
-- let transactionWitness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)
-- @
--
-- Finally, we sign the transaction using the 'signTx' function:
--
-- @
-- let newApiSignedTx :: Ledger.Tx (Exp.LedgerEra Exp.ConwayEra) = Exp.signTx era [] [transactionWitness] unsignedTx
-- @
--
-- Where the empty list is for the bootstrap witnesses, which, in this case, we don't have any.
--
-- And that is it. We have a signed transaction.

-- ** Converting a transaction from the new API to the old API
-- |
-- If we have a transaction created using the new API, we can convert it to the old API very easily by
-- just wrapping it using the 'ShelleyTx' constructor:
--
-- @
-- let oldStyleTx :: Api.Tx Api.ConwayEra = ShelleyTx sbe newApiSignedTx
-- @
--

-- * Contents

-- ** Tx related
    UnsignedTx (..)
  , UnsignedTxError (..)
  , makeUnsignedTx
  , makeKeyWitness
  , signTx
  , convertTxBodyToUnsignedTx
  , EraCommonConstraints
  , obtainCommonConstraints
  , hashTxBody
  , evaluateTransactionExecutionUnitsShelley

-- ** Era related
  , BabbageEra
  , ConwayEra
  , Era (..)
  , IsEra (..)
  , Some (..)
  , LedgerEra
  , DeprecatedEra (..)
  , eraToSbe
  , babbageEraOnwardsToEra
  , eraToBabbageEraOnwards
  , sbeToEra
  )
where

import           Cardano.Api.Experimental.Eras
import           Cardano.Api.Experimental.Tx
import           Cardano.Api.Fees (evaluateTransactionExecutionUnitsShelley)
