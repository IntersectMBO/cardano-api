// grpc-connection.d.ts

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
     * Get all UTXOs from the node using a GRPC-web client.
     * @returns A promise that resolves to the current UTXO set.
     */
    getAllUtxos(): Promise<{ address: string, txId: string, txIndex: number, lovelace: bigint, assets: any[], datum?: any, script?: any }[]>;

    /**
     * Get UTXOs for a given address using a GRPC-web client.
     * @param address The address to get UTXOs for.
     * @returns A promise that resolves to the UTXOs for the given address.
     */
    getUtxosForAddress(address: string): Promise<{ txId: string, txIndex: number, lovelace: bigint, assets: any[], datum?: any, script?: any }[]>;
}
