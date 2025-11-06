// wallet.d.ts

/**
 * Represents a wallet.
 */
declare interface Wallet {
    /**
     * The type of the object, used for identification (the "Wallet" string).
     */
    objectType: string;

    /**
     * Get the Bech32 representation of the address. (Can be shared for receiving funds.)
     * @returns The Bech32 representation of the address.
     */
    getAddressBech32(): Promise<string>;

    /**
     * Get the Bech32 representation of the payment verification key of the wallet. (Can be shared for verification.)
     * @returns The Bech32 representation of the payment verification key.
     */
    getBech32ForPaymentVerificationKey(): Promise<string>;

    /**
     * Get the Bech32 representation of the payment signing key of the wallet. (Must be kept secret.)
     * @returns The Bech32 representation of the payment signing key.
     */
    getBech32ForPaymentSigningKey(): Promise<string>;

    /**
     * Get the Bech32 representation of the stake verification key of the wallet. (Can be shared for verification.)
     * @returns The Bech32 representation of the stake verification key.
     */
    getBech32ForStakeVerificationKey(): Promise<string>;

    /**
     * Get the Bech32 representation of the stake signing key of the wallet. (Must be kept secret.)
     * @returns The Bech32 representation of the stake signing key.
     */
    getBech32ForStakeSigningKey(): Promise<string>;

    /**
     * Get the base16 representation of the hash of the payment verification key of the wallet.
     * @returns The base16 representation of the payment verification key hash.
     */
    getBase16ForPaymentVerificationKeyHash(): Promise<string>;

    /**
     * Get the base16 representation of the hash of the stake verification key of the wallet.
     * @returns The base16 representation of the stake verification key hash.
     */
    getBase16ForStakeVerificationKeyHash(): Promise<string>;
}

export default Wallet;
