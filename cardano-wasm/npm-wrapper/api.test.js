/**
 * @jest-environment node
 */
import initialise from "./dist/node.cjs";

// Main test suite for the Cardano API
describe('Cardano API', () => {

    let api;

    beforeAll(async () => {
        api = await initialise();
    });

    // Test case adapted from your browser script
    it('should build, sign, and serialize a simple transaction', async () => {
        // Test constants
        const PREVIEW_MAGIC_NUMBER = 2;
        const secretKey = "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms";

        const expectedAddress = "addr_test1vp93p9em3regvgylxuvet6fgr3e9sn259pcejgrk4ykystcs7v8j6";
        const txInputHash = "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978";
        const outputAddress = "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v";

        // Restore wallet and verify the address
        const wallet = await api.restoreTestnetPaymentWalletFromSigningKeyBech32(PREVIEW_MAGIC_NUMBER, secretKey);
        const bech32Address = await wallet.getAddressBech32();
        expect(bech32Address).toBe(expectedAddress);

        // Create a new transaction
        const emptyTx = await api.tx.newTx();
        expect(emptyTx).toBeDefined();

        // Add inputs and outputs
        const tx = await emptyTx
            .addTxInput(txInputHash, 0)
            .addSimpleTxOut(outputAddress, 10_000_000n);

        // Set the fee and sign the transaction
        const signedTx = await tx.setFee(10_000n).signWithPaymentKey(secretKey);
        expect(signedTx).toBeDefined();

        // Serialize the transaction to CBOR format
        const txCbor = await signedTx.txToCbor();
        expect(txCbor).toBe("84a300d9010281825820be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd97800018182581d6082935e44937e8b530f32ce672b5d600d0a286b4e8a52c6555f659b871a0098968002192710a100d9010281825820adfc1c30385916da87db1ba3328f0690a57ebb2a6ac9f6f86b2d97f943adae0058400b19a00593e659ad0f10951f0f7d1e8a8b93112c60f67277529f91340581639e92ed4d0042ff92a0076cd69deb7e708acfdb73bb4ae79cf4bc06fd6d15efa208f5f6");
    });

});
