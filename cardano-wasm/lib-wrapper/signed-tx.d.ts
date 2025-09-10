// signed-tx.d.ts

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

export default SignedTx;
