// unsigned-tx.d.ts

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
     * @param lovelaceAmount The amount in lovelaces to output.
     * @returns The `UnsignedTx` object with the added output.
     */
    addSimpleTxOut(destAddr: string, lovelaceAmount: bigint): UnsignedTx;

    /**
     * Sets the fee for the transaction.
     * @param lovelaceAmount The fee amount in lovelaces.
     * @returns The `UnsignedTx` object with the set fee.
     */
    setFee(lovelaceAmount: bigint): UnsignedTx;

    /**
     * Estimates the minimum fee for the transaction.
     * @param protocolParams The protocol parameters.
     * @param numKeyWitnesses The number of key witnesses.
     * @param numByronKeyWitnesses The number of Byron key witnesses.
     * @param totalRefScriptSize The total size of reference scripts in bytes.
     * @returns A promise that resolves to the estimated minimum fee in lovelaces.
     */
    estimateMinFee(protocolParams: any, numKeyWitnesses: number, numByronKeyWitnesses: number, totalRefScriptSize: number): Promise<bigint>;

    /**
     * Signs the transaction with a payment key.
     * @param signingKey The signing key to witness the transaction.
     * @returns A promise that resolves to a `SignedTx` object.
     */
    signWithPaymentKey(signingKey: string): Promise<SignedTx>;
}
