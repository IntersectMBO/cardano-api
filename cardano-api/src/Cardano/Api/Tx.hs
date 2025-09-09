module Cardano.Api.Tx
  ( -- * Creating transactions using the old API

    -- |
    -- Both the old and new APIs support transaction creation. Transactions can be
    -- converted between formats, as they share the same underlying representation.
    -- @cardano-api@ will be moving towards using the new API and deprecating
    -- the old way to ensure simplicity, closer alignment with the ledger, and
    -- easier maintenance.
    --
    -- In both the new and old APIs, to construct a transaction, you need
    -- to construct a 'TxBodyContent', and you will need at least a
    -- witness (for example, a 'ShelleyWitnessSigningKey'), to sign the transaction.
    -- This process remains unchanged.
    --
    -- To learn how to create a transaction using the new API, refer to
    -- "Cardano.Api.Experimental.Tx" documentation.
    --
    -- The next examples use the following qualified modules:
    --
    -- @
    -- import qualified Cardano.Api as Api                -- the general `cardano-api` exports (including the old API)
    -- import qualified Cardano.Api.Script as Script      -- types related to scripts (Plutus and native)
    -- import qualified Cardano.Api.Ledger as Ledger      -- cardano-ledger re-exports
    -- @

    -- ** Creating a 'TxBodyContent'

    -- |
    --
    -- To create a transaction, you first need to define the contents of its body. This section
    -- will show how to use the API to create a 'TxBodyContent' for a simple transaction.
    --
    -- 'TxBodyContent' datatype provides several fields because transactions can serve multiple
    -- purposes, but the function 'defaultTxBodyContent' (exported from "Cardano.Api") already provides
    -- a base 'TxBodyContent' with all fields set to their default values that you can use as a starting point
    -- so as not to have to set all fields manually.
    --
    -- The 'defaultTxBodyContent' takes, as the only parameter, the 'ShelleyBasedEra' witness for the era
    -- you are working with. For example, if you are working with the 'ConwayEra', use 'shelleyBasedEra'
    -- available in "Cardano.Api", as follows:
    --
    -- @
    -- let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    -- @
    --
    -- This is what creating a simple 'TxBodyContent' would look like.
    --
    -- First, choose a transaction output to spend (a UTXO). Specify which UTXO to spend by
    -- providing the transaction ID and the index of the output in that transaction that you want
    -- to spend.
    --
    -- To specify the transaction ID, you can use the 'deserialiseFromRawBytesHex' function on the
    -- hexadecimal representation of the transaction hash. For example:
    --
    -- @
    -- let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
    -- @
    --
    -- In real implementations, failure cases should be handled appropriately.
    --
    -- To specify the transaction index, use the 'TxIx' constructor. For example:
    --
    -- @
    -- let srcTxIx = Api.TxIx 0
    -- @
    --
    -- Now, combine both to create a 'TxIn' value and pair it with a witness requirement using 'BuildTxWith' :
    --
    -- @
    -- let txIn = ( Api.TxIn srcTxId srcTxIx
    --            , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
    --            )
    -- @
    --
    -- Next, specify the address of the recipient of the transaction. If you have the bech32 representation,
    -- you can use the 'deserialiseAddress' function to get the 'AddressInEra' type. For example:
    --
    -- @
    -- let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra eraAsType) "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
    -- @
    --
    -- Now, you can create a 'TxOut' value. For simplicity, assume the output is a simple payment output
    -- of 10 ada, with no datum or reference script attached to it:
    --
    -- @
    -- let txOut = Api.TxOut
    --               destAddress
    --               (Api.lovelaceToTxOutValue sbe 10_000_000)
    --               Api.TxOutDatumNone
    --               Script.ReferenceScriptNone
    -- @
    --
    -- Note to set the transaction fee. For example, set it to 2 ada:
    --
    -- @
    -- let txFee = Api.TxFeeExplicit sbe 2_000_000
    -- @
    --
    -- Finally, you can create the 'TxBodyContent' by using the 'defaultTxBodyContent' function and
    -- putting everything together:
    --
    -- @
    -- let txBodyContent = Api.defaultTxBodyContent sbe
    --                      & Api.setTxIns [txIn]
    --                      & Api.setTxOuts [txOut]
    --                      & Api.setTxFee txFee
    -- @
    --
    -- The 'txBodyContent' can now be used to create a transaction using the old or the new API.

    -- ** Balancing a transaction

    -- |
    -- If you have a UTXO with exactly 12 ada, you could just construct the transaction as in the
    -- previous section directly, and it would be a valid transaction, but:
    --
    --   * It is probably wasting ADA
    --   * There may not be exactly one UTXO of 12 ada
    --   * The transaciton may not be this simple.
    --
    -- For these reasons, it is recommended that you balance the transaction before proceeding with
    -- signing and submitting.
    --
    -- See how to balance a transaction in the "Cardano.Api.Tx.Internal.Fee" documentation.

    -- ** Creating a 'ShelleyWitnessSigningKey'

    -- |
    -- Signing a transaction requires a witness, for example, a 'ShelleyWitnessSigningKey'.
    --
    -- Learn how to create a 'ShelleyWitnessSigningKey' in the "Cardano.Api.Tx.Internal.Sign" documentation.

    -- ** Creating a transaction using the old API

    -- |
    -- Now that you have a 'TxBodyContent' and a 'ShelleyWitnessSigningKey', you can easily create a transaction using the old API.
    -- First, create a transaction body using the 'createTransactionBody' function and the 'ShelleyBasedEra' witness
    -- defined earlier.
    --
    -- Create the transaction body using the 'TransactionBodyContent' created earlier:
    --
    -- @
    -- let (Right txBody) = Api.createTransactionBody sbe txBodyContent
    -- @
    --
    -- Then, sign the transaction using the 'signShelleyTransaction' function and the witness:
    --
    -- @
    -- let oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [witness]
    -- @
    --
    -- We now have a signed transaction. Learn how to submit it to the node by using the IPC protocol in
    -- the "Cardano.Api.Network.IPC.Internal".

    -- ** Inspecting transactions

    -- |
    -- To deconstruct an old-style 'TxBody' into a 'TxBodyContent', you can also use the
    -- 'TxBody' pattern. Note that this cannot be used for constructing. For that, use 'ShelleyTxBody'
    -- or 'createTransactionBody', as in the example.
    --
    -- To extract the 'TxBody' and the 'KeyWitness'es from an old-style 'Tx', use
    -- the functions 'getTxBody' and 'getTxWitnesses' respectively, from "Cardano.Api".

    -- ** Appendix: Getting Shelley-based era witness from the new API

    -- |
    -- If you are using the new API, you can also derive the 'ShelleyBasedEra' of it from 'ConwayEra'
    -- from "Cardano.Api.Internal.Experimental" using the 'convert' function:
    --
    -- @
    -- let era = Exp.ConwayEra
    -- let sbe = Api.convert era
    -- @

    -- ** Transaction body
    TxBody (..)
  , createTransactionBody
  , createAndValidateTransactionBody
  , TxBodyContent (..)

    -- ** Byron only
  , makeByronTransactionBody

    -- *** Transaction body builders
  , defaultTxBodyContent
  , defaultTxFee
  , defaultTxValidityUpperBound
  , setTxIns
  , modTxIns
  , addTxIn
  , addTxIns
  , setTxInsCollateral
  , modTxInsCollateral
  , addTxInsCollateral
  , addTxInCollateral
  , setTxInsReference
  , modTxInsReference
  , addTxInsReference
  , addTxInReference
  , setTxOuts
  , modTxOuts
  , addTxOut
  , addTxOuts
  , setTxTotalCollateral
  , modTxTotalCollateral
  , setTxReturnCollateral
  , modTxReturnCollateral
  , setTxFee
  , modTxFee
  , setTxValidityLowerBound
  , modTxValidityLowerBound
  , setTxValidityUpperBound
  , modTxValidityUpperBound
  , setTxMetadata
  , modTxMetadata
  , setTxAuxScripts
  , modTxAuxScripts
  , setTxExtraKeyWits
  , modTxExtraKeyWits
  , addTxExtraKeyWits
  , setTxProtocolParams
  , setTxWithdrawals
  , modTxWithdrawals
  , setTxCertificates
  , modTxCertificates
  , setTxUpdateProposal
  , modTxUpdateProposal
  , setTxProposalProcedures
  , setTxVotingProcedures
  , setTxMintValue
  , modTxMintValue
  , addTxMintValue
  , subtractTxMintValue
  , setTxScriptValidity
  , modTxScriptValidity
  , setTxCurrentTreasuryValue
  , setTxTreasuryDonation
  , TxBodyError (..)
  , TxOutputError (..)
  , TxBodyScriptData (..)
  , selectTxDatums
  , TxScriptValidity (..)
  , ScriptValidity (..)
  , scriptValidityToIsValid
  , isValidToScriptValidity
  , txScriptValidityToIsValid
  , txScriptValidityToScriptValidity
  , UTxO (..)

    -- ** Transaction Ids
  , TxId (..)
  , parseTxId
  , getTxId
  , getTxIdByron
  , getTxIdShelley

    -- ** Transaction inputs
  , TxIn (..)
  , parseTxIn
  , TxIns
  , indexTxIns
  , TxIx (..)
  , parseTxIx
  , genesisUTxOPseudoTxIn
  , getReferenceInputsSizeForTxIds

    -- ** Transaction outputs
  , CtxTx
  , CtxUTxO
  , TxOut (..)
  , TxOutValue (..)
  , TxOutDatum (TxOutDatumNone, TxOutDatumHash, TxOutSupplementalDatum, TxOutDatumInline)
  , toCtxUTxOTxOut
  , fromCtxUTxOTxOut
  , lovelaceToTxOutValue
  , prettyRenderTxOut
  , txOutValueToLovelace
  , txOutValueToValue
  , TxOutInAnyEra (..)
  , txOutInAnyEra

    -- ** Other transaction body types
  , TxInsCollateral (..)
  , TxInsReference (..)
  , TxInsReferenceDatums
  , getReferenceInputDatumMap
  , TxReturnCollateral (..)
  , TxTotalCollateral (..)
  , TxFee (..)
  , TxValidityLowerBound (..)
  , TxValidityUpperBound (..)
  , TxMetadataInEra (..)
  , TxAuxScripts (..)
  , TxExtraKeyWitnesses (..)
  , TxWithdrawals (..)
  , indexTxWithdrawals
  , TxCertificates (..)
  , mkTxCertificates
  , indexTxCertificates
  , TxUpdateProposal (..)
  , TxMintValue (..)
  , mkTxMintValue
  , txMintValueToValue
  , indexTxMintValue
  , TxVotingProcedures (..)
  , mkTxVotingProcedures
  , indexTxVotingProcedures
  , TxProposalProcedures (..)
  , mkTxProposalProcedures
  , indexTxProposalProcedures
  , indexWitnessedTxProposalProcedures
  , convProposalProcedures

    -- *** Building vs viewing transactions
  , BuildTxWith (..)
  , BuildTx
  , ViewTx
  , buildTxWithToMaybe

    -- ** Inspecting 'ScriptWitness'es
  , AnyScriptWitness (..)
  , ScriptWitnessIndex (..)
  , renderScriptWitnessIndex
  , collectTxBodyScriptWitnesses
  , collectTxBodyScriptWitnessRequirements
  , toScriptIndex

    -- ** Conversion to inline data
  , scriptDataToInlineDatum

    -- ** Internal conversion functions & types
  , convCertificates
  , convCollateralTxIns
  , convExtraKeyWitnesses
  , convLanguages
  , convMintValue
  , convPParamsToScriptIntegrityHash
  , convReferenceInputs
  , convReturnCollateral
  , convScripts
  , convScriptData
  , convTotalCollateral
  , convTransactionFee
  , convTxIns
  , convTxOuts
  , convTxUpdateProposal
  , convValidityLowerBound
  , convValidityUpperBound
  , convVotingProcedures
  , convWithdrawals
  , getScriptIntegrityHash
  , mkCommonTxBody
  , toAuxiliaryData
  , toByronTxId
  , toShelleyTxId
  , toShelleyTxIn
  , toShelleyTxOut
  , toShelleyTxOutAny
  , fromShelleyTxId
  , fromShelleyTxIn
  , fromShelleyTxOut
  , fromByronTxIn
  , fromLedgerTxOuts
  , renderTxIn

    -- ** Misc helpers
  , calculateExecutionUnitsLovelace

    -- ** Data family instances
  , AsType (AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody)
  , getTxBodyContent
  -- Temp
  , validateTxIns
  , guardShelleyTxInsOverflow
  , validateTxOuts
  , validateMetadata
  , validateTxInsCollateral
  , validateProtocolParameters

    -- * Convenience construction functions
  , constructBalancedTx

    -- ** Misc
  , TxInsExistError (..)
  , ScriptLockedTxInsError (..)
  , notScriptLockedTxIns
  , renderNotScriptLockedTxInsError
  , renderTxInsExistError
  , txInsExistInUTxO

    -- ** Ledger TxBody wrapper with useful lens
  , LedgerTxBody (..)

    -- ** Constructors
  , mkAdaOnlyTxOut
  , mkAdaValue

    -- ** Lenses
  , strictMaybeL
  , invalidBeforeL
  , invalidHereAfterL
  , invalidBeforeStrictL
  , invalidHereAfterStrictL
  , invalidBeforeTxBodyL
  , invalidHereAfterTxBodyL
  , ttlAsInvalidHereAfterTxBodyL
  , updateTxBodyL
  , txBodyL
  , mintTxBodyL
  , scriptIntegrityHashTxBodyL
  , collateralInputsTxBodyL
  , reqSignerHashesTxBodyL
  , referenceInputsTxBodyL
  , collateralReturnTxBodyL
  , totalCollateralTxBodyL
  , certsTxBodyL
  , votingProceduresTxBodyL
  , proposalProceduresTxBodyL
  , currentTreasuryValueTxBodyL
  , treasuryDonationTxBodyL
  , adaAssetL
  , multiAssetL
  , valueTxOutL
  , valueTxOutAdaAssetL

    -- * Fees calculation

    -- |
    -- The "Cardano.Api.Tx.Internal.Body" documentation demonstrates how to create a 'TxBodyContent' for a
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
    -- query defined in "Cardano.Api.Query.Internal.Type.QueryInMode". "Cardano.Api.Network.IPC.Internal" documentation illustrates how
    -- to make a query using IPC protocol.
    -- Let's assume they are stored in the @exampleProtocolParams@ variable.
    --
    -- 3. The draft transaction body, which can be created using 'createTransactionBody' defined in "Cardano.Api.Tx.Internal.Body":
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
    -- the transaction as demonstrated in "Cardano.Api.Experimental.Tx".

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
    -- in "Cardano.Api.Tx.Internal.Body". It is assumed to be stored in the @txBodyContent@ variable.
    --
    -- 3. The protocol parameters for the current era, which can be obtained using the 'QueryProtocolParameters'
    -- query defined in "Cardano.Api.Query.Internal.Type.QueryInMode". "Cardano.Api.Network.IPC.Internal" documentation illustrates how
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
    -- using the 'deserialiseAddress' function defined in "Cardano.Api.Address":
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
    -- "Cardano.Api.Query.Internal.Type.QueryInMode". "Cardano.Api.Network.IPC.Internal" documentation illustrates how
    -- to make a query using IPC protocol. Assume we have it in the @exampleSystemStart@ variable.
    --
    -- 3. Ledger epoch information, derivable by applying 'toLedgerEpochInfo' to the
    -- 'EraHistory', which can be retrieved using the 'QueryEraHistory' query defined in
    -- "Cardano.Api.Query.Internal.Type.QueryInMode". Assume this is stored in the @exampleLedgerEpochInfo@ variable.
    --
    -- 4. Protocol parameters for the current era, accessible through the 'QueryProtocolParameters'
    -- query defined in "Cardano.Api.Query.Internal.Type.QueryInMode". Assume this is stored in the @exampleProtocolParams@ variable.
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
    -- from "Cardano.Api.Query.Internal.Type.QueryInMode". Assume this is stored in the @utxoToUse@ variable.
    --
    -- 7. Draft transaction body content -- the 'TxBodyContent' for the transaction that requires balancing.
    -- The process for creating this structure is demonstrated in "Cardano.Api.Tx.Internal.Body".
    --
    -- 8. Change address -- the address where any remaining balance should be returned. This can be
    -- deserialized from its Bech32 representation using the 'deserialiseAddress' function from
    -- "Cardano.Api.Address":
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

    -- ** Transaction fees
  , evaluateTransactionFee
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

    -- * Signing a transaction

    -- | Example: Creating a 'ShelleyWitnessSigningKey'
    -- Signing a transaction requires a witness, for example, a 'ShelleyWitnessSigningKey'.
    --
    -- This example uses the following qualified module:
    --
    -- @
    -- import qualified Cardano.Api as Api                -- the general `cardano-api` exports (including the old API)
    -- @
    --
    -- There are several ways of signing a transaction and representing a signing key. If the
    -- bech32 representation of the signing key is available, it is possible to use the
    -- 'deserialiseFromBech32' function as follows:
    --
    -- @
    -- let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"
    -- @
    --
    -- Then, simply wrap the signing key in a 'ShelleyWitnessSigningKey' value:
    --
    -- @
    -- let witness = Api.WitnessPaymentKey signingKey
    -- @
    --
    -- This could also be done using an extended key, such as 'AsPaymentExtendedKey' and 'WitnessPaymentExtendedKey'.

    -- ** Signing transactions

    -- | Creating transaction witnesses one by one, or all in one go.
  , Tx (..)
  , ATxAux (..)
  , getTxBody
  , getByronTxBody
  , getTxWitnesses
  , getTxWitnessesByron

    -- *** Signing in one go
  , ShelleySigningKey (..)
  , toShelleySigningKey
  , signByronTransaction
  , signShelleyTransaction

    -- *** Incremental signing and separate witnesses
  , makeSignedByronTransaction
  , makeSignedTransaction
  , makeSignedTransaction'
  , KeyWitness (..)
  , makeByronKeyWitness
  , ShelleyWitnessSigningKey (..)
  , makeShelleyKeyWitness
  , makeShelleyKeyWitness'
  , WitnessNetworkIdOrByronAddress (..)
  , makeShelleyBootstrapWitness
  , makeShelleyBasedBootstrapWitness
  , makeShelleySignature
  , getShelleyKeyWitnessVerificationKey
  , getTxBodyAndWitnesses

    -- * TxMetadata
  , TxMetadata (..)

    -- ** Constructing metadata
  , AsTxMetadata (..)
  , TxMetadataValue (..)
  , makeTransactionMetadata
  , mergeTransactionMetadata
  , metaTextChunks
  , metaBytesChunks

    -- ** Validating metadata
  , validateTxMetadata
  , TxMetadataRangeError (..)

    -- ** Conversion to\/from JSON
  , TxMetadataJsonSchema (..)
  , metadataFromJson
  , metadataToJson
  , metadataValueFromJsonNoSchema
  , metadataValueToJsonNoSchema
  , TxMetadataJsonError (..)
  , TxMetadataJsonSchemaError (..)

    -- ** Internal conversion functions
  , toShelleyMetadata
  , fromShelleyMetadata
  , toShelleyMetadatum
  , fromShelleyMetadatum
  -- Exported for testing
  , extractWitnessableCertificates
  , extractWitnessableMints
  , extractWitnessableProposals
  , extractWitnessableTxIns
  , extractWitnessableVotes
  , extractWitnessableWithdrawals
  -- Exporting for testing. Deprecate in the future.
  , legacyKeyWitnessEncode

    -- ** Shared parsing utils
  , parseAll
  , pUnsigned
  , pSigned
  , pBytes
  )
where

import Cardano.Api.Tx.Internal.Body
import Cardano.Api.Tx.Internal.Body.Lens
import Cardano.Api.Tx.Internal.Convenience
import Cardano.Api.Tx.Internal.Fee
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.UTxO
