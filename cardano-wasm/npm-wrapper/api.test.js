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

    // The transaction id is the blake2b-256 hash of the transaction body, so it
    // is already available before signing and is not changed by adding witnesses.
    it('should expose the transaction id on both unsigned and signed transactions', async () => {
        // Test constants
        const secretKey = "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms";
        const txInputHash = "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978";
        const outputAddress = "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v";

        // Same transaction as in the simple transaction test above. The expected id
        // is the blake2b-256 hash of the transaction body bytes in its expected CBOR
        // (computed independently of this library).
        const expectedTxId = "4eebc97a1c96eb5537d0ec2172fdb3f0c516dc209d9ab93de585956e0be99ff1";

        const tx = (await api.tx.newTx())
            .addTxInput(txInputHash, 0)
            .addSimpleTxOut(outputAddress, 10_000_000n)
            .setFee(10_000n);

        // The id is available as soon as the body is final (before signing)
        const unsignedTxId = await tx.getTxId();
        expect(unsignedTxId).toBe(expectedTxId);

        // Signing does not change the transaction id
        const signedTx = await tx.signWithPaymentKey(secretKey);
        expect(await signedTx.getTxId()).toBe(expectedTxId);

        // Nor does adding more witnesses
        const multiSignedTx = await signedTx.alsoSignWithPaymentKey(secretKey);
        expect(await multiSignedTx.getTxId()).toBe(expectedTxId);
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

        // The transaction id only depends on the body, so the stake witness does not change it
        expect(await signedTx.getTxId()).toBe(await tx.getTxId());
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

    // An address only encodes mainnet vs testnet (network id 1 vs 0). Different
    // testnets (like preprod and preview) only differ by network magic, which is
    // not part of the address, so they cannot be told apart here.
    it('should validate addresses and detect their network', async () => {
        // Payment-only (enterprise) and base (payment + stake) addresses, built
        // from the payment and stake key hashes used in the other tests
        const testnetAddress = "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v";
        const mainnetAddress = "addr1vxpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpc83ps9f";
        const testnetBaseAddress = "addr_test1qzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpafgc0xsa38ehw9748lexytc3pjzxy48rrqruddrduhnkdssekpmc";
        const mainnetBaseAddress = "addr1qxpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpafgc0xsa38ehw9748lexytc3pjzxy48rrqruddrduhnkdsn0tph8";

        expect(await api.inspectAddress(testnetAddress)).toEqual({ network: "testnet" });
        expect(await api.inspectAddress(testnetBaseAddress)).toEqual({ network: "testnet" });
        expect(await api.inspectAddress(mainnetAddress)).toEqual({ network: "mainnet" });
        expect(await api.inspectAddress(mainnetBaseAddress)).toEqual({ network: "mainnet" });

        // Invalid inputs do not throw, they resolve to null instead
        const invalidAddresses = [
            "",                                // empty
            testnetAddress.slice(0, -1),       // truncated
            testnetAddress.slice(0, -1) + "w", // bad checksum
            "this is not an address",          // not an address at all
        ];
        for (const address of invalidAddresses) {
            expect(await api.inspectAddress(address)).toBeNull();
        }
    });

});
