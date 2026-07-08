// signed-tx.d.ts

/**
 * Represents a signed transaction.
 */
declare interface SignedTx {
    /**
     * The type of the object, used for identification (the "SignedTx" string).
     * Other types of objects would be:
     * "CardanoApi", "GrpcConnection", "UnsignedTx", and "Wallet"
     */
    objectType: string;

    /**
     * Adds an extra signature to the transaction with a payment key.
     * @param signingKey The signing key to witness the transaction.
     * @returns The `SignedTx` object with the additional signature.
     */
    alsoSignWithPaymentKey(signingKey: string): SignedTx;

    /**
     * Adds an extra signature to the transaction with a stake key.
     * @param signingKey The signing key to witness the transaction.
     * @returns The `SignedTx` object with the additional signature.
     */
    alsoSignWithStakeKey(signingKey: string): SignedTx;

    /**
     * Converts the signed transaction to its CBOR representation.
     * @returns A promise that resolves to the CBOR representation of the transaction as a hex string.
     */
    txToCbor(): Promise<string>;

    /**
     * Gets the transaction id (the hash of the transaction body). It is not affected by signing, so it matches the id of the unsigned transaction just before signing.
     * @returns A promise that resolves to the transaction id as a hex string.
     */
    getTxId(): Promise<string>;
}

export default SignedTx;
