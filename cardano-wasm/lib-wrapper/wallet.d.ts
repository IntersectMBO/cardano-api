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
     * Get the Bech32 representation of the verification key of the wallet. (Can be shared for verification.)
     * @returns The Bech32 representation of the verification key.
     */
    getBech32ForVerificationKey(): Promise<string>;

    /**
     * Get the Bech32 representation of the signing key of the wallet. (Must be kept secret.)
     * @returns The Bech32 representation of the signing key.
     */
    getBech32ForSigningKey(): Promise<string>;

    /**
     * Get the base16 representation of the hash of the verification key of the wallet.
     * @returns The base16 representation of the verification key hash.
     */
    getBase16ForVerificationKeyHash(): Promise<string>;
}

export default Wallet;
