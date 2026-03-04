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
     * Methods for creating unsigned transactions.
     */
    tx: {
        /**
         * Create a new unsigned transaction in the current era (currently Conway).
         * @returns A promise that resolves to a new `UnsignedTx` object.
         */
        newTx(): Promise<UnsignedTx>;

        /**
         * Create a new unsigned transaction in the current upcoming era (currently Dijkstra).
         * @returns A promise that resolves to a new `UnsignedTx` object.
         */
        newUpcomingEraTx(): Promise<UnsignedTx>;
    }

    /**
     * Create a new client connection for communicating with a Cardano node through gRPC-web.
     * @param webGrpcUrl The URL of the gRPC-web server.
     * @returns A promise that resolves to a new `GrpcConnection`.
     */
    newGrpcConnection(webGrpcUrl: string): Promise<GrpcConnection>;

    /**
     * Methods for creating certificates.
     */
    certificate: {
        /**
         * Methods for creating certificates in the current era (currently Conway).
         */
        currentEra: {
            /**
             * Make a certificate that delegates a stake address to a stake pool in the current era (currently Conway).
             * @param stakeKeyHash The stake key hash in base16 format.
             * @param poolId The pool ID in base16 format.
             * @returns A promise that resolves to the CBOR-encoded certificate as a hex string.
             */
            makeStakeAddressStakeDelegationCertificate(stakeKeyHash: string, poolId: string): Promise<string>;

            /**
             * Make a stake address registration certificate in the current era (currently Conway).
             * @param stakeKeyHash The stake key hash in base16 format.
             * @param deposit The deposit amount in lovelaces.
             * @returns A promise that resolves to the CBOR-encoded certificate as a hex string.
             */
            makeStakeAddressRegistrationCertificate(stakeKeyHash: string, deposit: bigint): Promise<string>;

            /**
             * Make a stake address unregistration certificate in the current era (currently Conway).
             * @param stakeKeyHash The stake key hash in base16 format.
             * @param deposit The deposit amount in lovelaces.
             * @returns A promise that resolves to the CBOR-encoded certificate as a hex string.
             */
            makeStakeAddressUnregistrationCertificate(stakeKeyHash: string, deposit: bigint): Promise<string>;
        }

        /**
         * Methods for creating certificates in the current upcoming era (currently Dijkstra).
         */
        upcomingEra: {
            /**
             * Make a certificate that delegates a stake address to a stake pool in the current upcoming era (currently Dijkstra).
             * @param stakeKeyHash The stake key hash in base16 format.
             * @param poolId The pool ID in base16 format.
             * @returns A promise that resolves to the CBOR-encoded certificate as a hex string.
             */
            makeStakeAddressStakeDelegationCertificateUpcomingEra(stakeKeyHash: string, poolId: string): Promise<string>;

            /**
             * Make a stake address registration certificate in the current upcoming era (currently Dijkstra).
             * @param stakeKeyHash The stake key hash in base16 format.
             * @param deposit The deposit amount in lovelaces.
             * @returns A promise that resolves to the CBOR-encoded certificate as a hex string.
             */
            makeStakeAddressRegistrationCertificateUpcomingEra(stakeKeyHash: string, deposit: bigint): Promise<string>;

            /**
             * Make a stake address unregistration certificate in the current upcoming era (currently Dijkstra).
             * @param stakeKeyHash The stake key hash in base16 format.
             * @param deposit The deposit amount in lovelaces.
             * @returns A promise that resolves to the CBOR-encoded certificate as a hex string.
             */
            makeStakeAddressUnregistrationCertificateUpcomingEra(stakeKeyHash: string, deposit: bigint): Promise<string>;
        }
    }

    /**
     * Methods for generating and restoring wallets.
     */
    wallet: {
        /**
         * Methods for mainnet wallets.
         */
        mainnet: {
            /**
             * Generate a simple payment wallet for mainnet.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            generatePaymentWallet(): Promise<Wallet>;

            /**
             * Generate a stake wallet for mainnet.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            generateStakeWallet(): Promise<Wallet>;

            /**
             * Restore a mainnet payment wallet from a Bech32 encoded signing key.
             * @param signingKeyBech32 The Bech32 encoded signing key.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            restorePaymentWalletFromSigningKeyBech32(signingKeyBech32: string): Promise<Wallet>;

            /**
             * Restore a mainnet stake wallet from Bech32 encoded signing keys.
             * @param paymentSigningKeyBech32 The Bech32 encoded payment signing key.
             * @param stakeSigningKeyBech32 The Bech32 encoded stake signing key.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            restoreStakeWalletFromSigningKeyBech32(paymentSigningKeyBech32: string, stakeSigningKeyBech32: string): Promise<Wallet>;
        }

        /**
         * Methods for wallets in other networks.
         */
        testnet: {
            /**
             * Generate a simple payment wallet for testnet, given the testnet's network magic.
             * @param networkMagic The network magic for the testnet.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            generateTestnetPaymentWallet(networkMagic: number): Promise<Wallet>;

            /**
             * Generate a stake wallet for testnet, given the testnet's network magic.
             * @param networkMagic The network magic for the testnet.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            generateTestnetStakeWallet(networkMagic: number): Promise<Wallet>;

            /**
             * Restore a testnet payment wallet from a Bech32 encoded signing key.
             * @param networkMagic The network magic for the testnet.
             * @param signingKeyBech32 The Bech32 encoded signing key.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            restoreTestnetPaymentWalletFromSigningKeyBech32(networkMagic: number, signingKeyBech32: string): Promise<Wallet>;

            /**
             * Restore a testnet stake wallet from Bech32 encoded signing keys.
             * @param networkMagic The network magic for the testnet.
             * @param paymentSigningKeyBech32 The Bech32 encoded payment signing key.
             * @param stakeSigningKeyBech32 The Bech32 encoded stake signing key.
             * @returns A promise that resolves to a new `Wallet` object.
             */
            restoreTestnetStakeWalletFromSigningKeyBech32(networkMagic: number, paymentSigningKeyBech32: string, stakeSigningKeyBech32: string): Promise<Wallet>;
        }
    }
}

/**
 * Initialises the Cardano API.
 * @returns A promise that resolves to the main `CardanoApi` object.
 */
declare function initialise(): Promise<CardanoApi>;

export default initialise;
