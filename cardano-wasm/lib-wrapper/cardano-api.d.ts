// cardano-api.d.ts

import UnsignedTx from './unsigned-tx';

import GrpcConnection from './grpc-connection';

import Wallet from './wallet';

/**
 * The main Cardano API object with static methods.
 */
declare interface CardanoApi {
    /**
     * The type of the object, used for identification (the "CardanoApi" string).
     */
    objectType: string;

    /**
     * Create a new unsigned transaction in the current era.
     * @returns A promise that resolves to a new `UnsignedTx` object.
     */
    newTx(): Promise<UnsignedTx>;

    /**
     * Create a new unsigned transaction in the current experimental era.
     * @returns A promise that resolves to a new `UnsignedTx` object.
     */
    newExperimentalEraTx(): Promise<UnsignedTx>;

    /**
     * Create a new unsigned transaction in the Conway era.
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
    generatePaymentWallet(): Promise<Wallet>;

    /**
     * Restore a mainnet payment wallet from a Bech32 encoded signing key.
     * @param signingKeyBech32 The Bech32 encoded signing key.
     * @returns A promise that resolves to a new `Wallet` object.
     */
    restorePaymentWalletFromSigningKeyBech32(signingKeyBech32: string): Promise<Wallet>;

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

/**
 * Initialises the Cardano API.
 * @returns A promise that resolves to the main `CardanoApi` object.
 */
declare function initialise(): Promise<CardanoApi>;

export default initialise;
