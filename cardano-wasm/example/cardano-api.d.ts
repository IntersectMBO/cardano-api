// cardano-api.d.ts

export default initialize;

/**
 * Initializes the Cardano API.
 * @returns A promise that resolves to the main `CardanoAPI` object.
 */
declare function initialize(): Promise<CardanoAPI>;

/**
 * Represents an unsigned transaction.
 */
declare interface UnsignedTx {
    /**
     * The type of the object, used for identification (the "UnsignedTx" string).
     */
    objectType: string;

    /**
     * Adds a simple transaction input to the transaction.
     * @param txId The transaction ID of the input UTxO.
     * @param txIx The index of the input within the UTxO.
     * @returns The `UnsignedTx` object with the added input.
     */
    addTxInput(txId: string, txIx: number): UnsignedTx;

    /**
     * Adds a simple transaction output to the transaction.
     * @param destAddr The destination address.
     * @param lovelaceAmount The amount in lovelace to output.
     * @returns The `UnsignedTx` object with the added output.
     */
    addSimpleTxOut(destAddr: string, lovelaceAmount: bigint): UnsignedTx;

    /**
     * Sets the fee for the transaction.
     * @param lovelaceAmount The fee amount in lovelace.
     * @returns The `UnsignedTx` object with the set fee.
     */
    setFee(lovelaceAmount: bigint): UnsignedTx;

    /**
     * Estimates the minimum fee for the transaction.
     * @param protocolParams The protocol parameters.
     * @param numKeyWitnesses The number of key witnesses.
     * @param numByronKeyWitnesses The number of Byron key witnesses.
     * @param totalRefScriptSize The total size of reference scripts in bytes.
     * @returns A promise that resolves to the estimated minimum fee in lovelace.
     */
    estimateMinFee(protocolParams: any, numKeyWitnesses: number, numByronKeyWitnesses: number, totalRefScriptSize: number): Promise<BigInt>;

    /**
     * Signs the transaction with a payment key.
     * @param signingKey The signing key to witness the transaction.
     * @returns A promise that resolves to a `SignedTx` object.
     */
    signWithPaymentKey(signingKey: string): Promise<SignedTx>;
}

/**
 * Represents a signed transaction.
 */
declare interface SignedTx {
    /**
     * The type of the object, used for identification (the "SignedTx" string).
     */
    objectType: string;

    /**
     * Adds an extra signature to the transaction with a payment key.
     * @param signingKey The signing key to witness the transaction.
     * @returns The `SignedTx` object with the additional signature.
     */
    alsoSignWithPaymentKey(signingKey: string): SignedTx;

    /**
     * Converts the signed transaction to its CBOR representation.
     * @returns A promise that resolves to the CBOR representation of the transaction as a hex string.
     */
    txToCbor(): Promise<string>;
}

/**
 * The main Cardano API object with static methods.
 */
declare interface CardanoAPI {
    /**
     * The type of the object, used for identification (the "CardanoAPI" string).
     */
    objectType: string;

    /**
     * Creates a new Conway-era transaction.
     * @returns A promise that resolves to a new `UnsignedTx` object.
     */
    newConwayTx(): Promise<UnsignedTx>;
}

