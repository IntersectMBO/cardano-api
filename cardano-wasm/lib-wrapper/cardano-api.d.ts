// cardano-api.d.ts

export default initialise;

/**
 * Initialises the Cardano API.
 * @returns A promise that resolves to the main `CardanoAPI` object.
 */
declare function initialise(): Promise<CardanoAPI>;

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
 * Represents a gRPC-web client connection to a Cardano node.
 */
declare interface GrpcConnection {
    /**
     * The type of the object, used for identification (the "GrpcConnection" string).
     */
    objectType: string;

    /**
     * Get the era from the Cardano Node using a GRPC-web client.
     * @returns A promise that resolves to the current era number.
     */
    getEra(): Promise<number>;

    /**
     * Submit a signed and CBOR-encoded transaction to the Cardano node.
     * @param txCbor The CBOR-encoded transaction as a hex string.
     * @returns A promise that resolves to the transaction ID.
     */
    submitTx(txCbor: string): Promise<string>;

    /**
     * Get the protocol parameters in the cardano-ledger format from the Cardano Node using a GRPC-web client.
     * @returns A promise that resolves to the current protocol parameters.
     */
    getProtocolParams(): Promise<any>;

    /**
     * Get UTXOs from the node using a GRPC-web client.
     * @param utxoFilter UTXO filter: a list of addresses to query
     * @returns A promise that resolves to the current UTXO set.
     */
    getUtxos(utxoFilter: { addresses: string[] } | null): Promise<any>;
}

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

    /**
     * Create a new client connection for communicating with a Cardano node through gRPC-web.
     * @param webGrpcUrl The URL of the gRPC-web server.
     * @returns A promise that resolves to a new `GrpcConnection`.
     */
    newGrpcConnection(webGrpcUrl: string): Promise<GrpcConnection>;

    /**
     * Generate a simple payment wallet for mainnet.
     * @returns A promise that resolves to a new `Wallet` object.
     */
    generateMainnetPaymentWallet(): Promise<Wallet>;

    /**
     * Restore a mainnet payment wallet from a Bech32 encoded signing key.
     * @param signingKeyBech32 The Bech32 encoded signing key.
     * @returns A promise that resolves to a new `Wallet` object.
     */
    restoreMainnetPaymentWalletFromSigningKeyBech32(signingKeyBech32: string): Promise<Wallet>;

    /**
     * Generate a simple payment wallet for testnet, given the testnet's network magic.
     * @param networkMagic The network magic for the testnet.
     * @returns A promise that resolves to a new `Wallet` object.
     */
    generateTestnetPaymentWallet(networkMagic: number): Promise<Wallet>;

    /**
     * Restore a testnet payment wallet from a Bech32 encoded signing key.
     * @param networkMagic The network magic for the testnet.
     * @param signingKeyBech32 The Bech32 encoded signing key.
     * @returns A promise that resolves to a new `Wallet` object.
     */
    restoreTestnetPaymentWalletFromSigningKeyBech32(networkMagic: number, signingKeyBech32: string): Promise<Wallet>;
}

