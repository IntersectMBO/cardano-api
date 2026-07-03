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
        const wallet = await api.wallet.testnet.restorePaymentWalletFromSigningKeyBech32(PREVIEW_MAGIC_NUMBER, secretKey);

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

    // Delegation requires a witness from the stake credential, added with the stake key.
    it('should build a delegation transaction signed with a payment key and a stake key', async () => {
        // Test constants
        const PREVIEW_MAGIC_NUMBER = 2;
        const paymentSecretKey = "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms";
        const stakeSecretKey = "stake_sk10pu8s7rc0pu8s7rc0pu8s7rc0pu8s7rc0pu8s7rc0pu8s7rc0puqawrffl";
        const poolId = "b".repeat(56);
        const txInputHash = "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978";
        const outputAddress = "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v";

        // Restore a stake wallet (holds both a payment and a stake key)
        const wallet = await api.wallet.testnet.restoreStakeWalletFromSigningKeyBech32(
            PREVIEW_MAGIC_NUMBER, paymentSecretKey, stakeSecretKey);
        const stakeKeyHash = await wallet.getBase16ForStakeVerificationKeyHash();

        // Delegate the stake credential to a pool
        const delegationCert = await api.certificate.mainnetEra
            .makeStakeAddressStakeDelegationCertificate(stakeKeyHash, poolId);

        // Build a transaction that includes the delegation certificate
        const tx = (await api.tx.newTx())
            .addTxInput(txInputHash, 0)
            .addSimpleTxOut(outputAddress, 5_000_000n)
            .appendCertificateToTx(delegationCert)
            .setFee(10_000n);

        // Sign with the payment key, then add a witness from the stake key
        const signedTx = (await tx.signWithPaymentKey(paymentSecretKey))
            .alsoSignWithStakeKey(stakeSecretKey);
        expect(signedTx).toBeDefined();

        const txCbor = await signedTx.txToCbor();

        expect(txCbor).toBe("84a400d9010281825820be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd97800018182581d6082935e44937e8b530f32ce672b5d600d0a286b4e8a52c6555f659b871a004c4b400219271004d901028183028200581ca9461e687627cddc5f54ffc988bc44321189538c601f1ad1b7979d9b581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba100d9010282825820adfc1c30385916da87db1ba3328f0690a57ebb2a6ac9f6f86b2d97f943adae005840b5df44de4b1302d1b031363cfb9636be2f395561dcf083d1192656677506bfa401d988d745f2ee2d11ed9e563f35cc2dfbb605c7cd58a613e42377e5e2a8da01825820ee31f83c88a71219a6fcf9bee0da9bc22620588f5a15a6145553504df9649e5c58402a2a37f2391bdf3a1ff52a7ed2103473f35b43022ed101bddcb18a557f7e9f9eac75c3562b142cc15dc9ebd006da34f4076ff413914ec90e4359d757d73bfc06f5f6");
    });

    // The stake key can also be the first signer, with the payment witness added afterwards.
    // Witnesses live in a set, so the resulting transaction is identical regardless of signing order.
    it('should build a delegation transaction signed with a stake key and then a payment key', async () => {
        // Test constants
        const PREVIEW_MAGIC_NUMBER = 2;
        const paymentSecretKey = "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms";
        const stakeSecretKey = "stake_sk10pu8s7rc0pu8s7rc0pu8s7rc0pu8s7rc0pu8s7rc0pu8s7rc0puqawrffl";
        const poolId = "b".repeat(56);
        const txInputHash = "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978";
        const outputAddress = "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v";

        // Restore a stake wallet (holds both a payment and a stake key)
        const wallet = await api.wallet.testnet.restoreStakeWalletFromSigningKeyBech32(
            PREVIEW_MAGIC_NUMBER, paymentSecretKey, stakeSecretKey);
        const stakeKeyHash = await wallet.getBase16ForStakeVerificationKeyHash();

        // Delegate the stake credential to a pool
        const delegationCert = await api.certificate.mainnetEra
            .makeStakeAddressStakeDelegationCertificate(stakeKeyHash, poolId);

        // Build a transaction that includes the delegation certificate
        const tx = (await api.tx.newTx())
            .addTxInput(txInputHash, 0)
            .addSimpleTxOut(outputAddress, 5_000_000n)
            .appendCertificateToTx(delegationCert)
            .setFee(10_000n);

        // Sign with the stake key first, then add a witness from the payment key
        const signedTx = (await tx.signWithStakeKey(stakeSecretKey))
            .alsoSignWithPaymentKey(paymentSecretKey);
        expect(signedTx).toBeDefined();

        const txCbor = await signedTx.txToCbor();

        // Identical to the payment-key-first delegation transaction above
        expect(txCbor).toBe("84a400d9010281825820be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd97800018182581d6082935e44937e8b530f32ce672b5d600d0a286b4e8a52c6555f659b871a004c4b400219271004d901028183028200581ca9461e687627cddc5f54ffc988bc44321189538c601f1ad1b7979d9b581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba100d9010282825820adfc1c30385916da87db1ba3328f0690a57ebb2a6ac9f6f86b2d97f943adae005840b5df44de4b1302d1b031363cfb9636be2f395561dcf083d1192656677506bfa401d988d745f2ee2d11ed9e563f35cc2dfbb605c7cd58a613e42377e5e2a8da01825820ee31f83c88a71219a6fcf9bee0da9bc22620588f5a15a6145553504df9649e5c58402a2a37f2391bdf3a1ff52a7ed2103473f35b43022ed101bddcb18a557f7e9f9eac75c3562b142cc15dc9ebd006da34f4076ff413914ec90e4359d757d73bfc06f5f6");
    });

});
