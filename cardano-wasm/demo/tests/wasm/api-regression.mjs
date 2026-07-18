// Regression test suite for the built cardano-wasm library (../../../lib-wrapper).
// Run: nix-shell -p nodejs --run 'node api-regression.mjs'  (from this directory)

import { readFile } from "node:fs/promises";
import { createInitializer } from "../../../lib-wrapper/main.js";
import { WASI } from "@bjorn3/browser_wasi_shim";

const DUMMY_TXID =
  "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978";
const DUMMY_POOL = "0".repeat(56);
const HEX64 = /^[0-9a-f]{64}$/;

// ---------------------------------------------------------------- harness

const results = [];

async function test(name, fn) {
  try {
    await fn();
    results.push({ name, ok: true });
    console.log(`PASS ${name}`);
  } catch (e) {
    results.push({ name, ok: false });
    console.log(`FAIL ${name}: ${e && e.message ? e.message : e}`);
  }
}

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

// ------------------------------------------------------------------ setup

const load = async (imp) =>
  WebAssembly.instantiate(
    await readFile(
      new URL("../../../lib-wrapper/cardano-wasm.wasm", import.meta.url)
    ),
    imp
  );

const api = await createInitializer(
  async () => new WASI([], [], []),
  load,
  () => ({})
)();

const { default: pparams } = await import("../../web/pparams.js");

// Builds a minimal tx (one input, one output to `addr`) ready for fee work.
async function baseTx(addr) {
  const tx = await api.tx.newTx();
  tx.addTxInput(DUMMY_TXID, 0).addSimpleTxOut(addr, 5_000_000n);
  return tx;
}

// ------------------------------------------------------------ 1. generate

let testnetWallet, mainnetWallet, testnetAddr, mainnetAddr;

await test("generate", async () => {
  testnetWallet = await api.wallet.testnet.generateStakeWallet(2);
  testnetAddr = await testnetWallet.getAddressBech32();
  assert(
    testnetAddr.startsWith("addr_test1"),
    `testnet address should start with addr_test1, got: ${testnetAddr}`
  );
  mainnetWallet = await api.wallet.mainnet.generateStakeWallet();
  mainnetAddr = await mainnetWallet.getAddressBech32();
  assert(
    mainnetAddr.startsWith("addr1"),
    `mainnet address should start with addr1, got: ${mainnetAddr}`
  );
});

// --------------------------------------------------- 2. restore round-trip

await test("restore round-trip", async () => {
  assert(testnetWallet, "prerequisite: generate must have succeeded");
  const paySkey = await testnetWallet.getBech32ForPaymentSigningKey();
  const stakeSkey = await testnetWallet.getBech32ForStakeSigningKey();
  const restored = await api.wallet.testnet.restoreStakeWalletFromSigningKeyBech32(
    2,
    paySkey,
    stakeSkey
  );
  const restoredAddr = await restored.getAddressBech32();
  assert(
    restoredAddr === testnetAddr,
    `restored address differs: ${restoredAddr} !== ${testnetAddr}`
  );
});

// ----------------------------------------------------------- 3. getTxId

await test("getTxId", async () => {
  assert(testnetWallet, "prerequisite: generate must have succeeded");
  const paySkey = await testnetWallet.getBech32ForPaymentSigningKey();
  const stakeSkey = await testnetWallet.getBech32ForStakeSigningKey();

  const tx = await baseTx(testnetAddr);
  const fee = await tx.estimateMinFee(pparams, 1, 0, 0);
  assert(typeof fee === "bigint" && fee > 0n, `bad fee: ${fee}`);
  tx.setFee(fee);

  const unsignedId = await tx.getTxId();
  assert(
    HEX64.test(unsignedId),
    `unsigned txid not 64 lowercase hex: ${unsignedId}`
  );

  const signed = await tx.signWithPaymentKey(paySkey);
  const signedId = await signed.getTxId();
  assert(
    signedId === unsignedId,
    `signed txid differs from unsigned: ${signedId} !== ${unsignedId}`
  );

  signed.alsoSignWithStakeKey(stakeSkey);
  const twiceSignedId = await signed.getTxId();
  assert(
    twiceSignedId === unsignedId,
    `txid changed after stake signature: ${twiceSignedId} !== ${unsignedId}`
  );
});

// ------------------------------------------------------ 4. cert round-trips

async function certRoundTrip(makeCert, { stakeSigns }) {
  assert(mainnetWallet, "prerequisite: generate must have succeeded");
  const stakeKeyHash =
    await mainnetWallet.getBase16ForStakeVerificationKeyHash();
  const certHex = await makeCert(stakeKeyHash);
  assert(
    typeof certHex === "string" && certHex.length > 0,
    `certificate hex empty or not a string: ${certHex}`
  );

  const tx = await baseTx(mainnetAddr);
  tx.appendCertificateToTx(certHex);
  const numWits = stakeSigns ? 2 : 1;
  const fee = await tx.estimateMinFee(pparams, numWits, 0, 0);
  assert(typeof fee === "bigint" && fee > 0n, `bad fee: ${fee}`);
  tx.setFee(fee);

  const paySkey = await mainnetWallet.getBech32ForPaymentSigningKey();
  const signed = await tx.signWithPaymentKey(paySkey);
  if (stakeSigns) {
    const stakeSkey = await mainnetWallet.getBech32ForStakeSigningKey();
    signed.alsoSignWithStakeKey(stakeSkey);
  }
  const cbor = await signed.txToCbor();
  assert(
    typeof cbor === "string" && cbor.startsWith("84"),
    `tx CBOR should start with 84, got: ${String(cbor).slice(0, 8)}...`
  );
}

await test("cert round-trip: registration", () =>
  certRoundTrip(
    (hash) =>
      api.certificate.mainnetEra.makeStakeAddressRegistrationCertificate(
        hash,
        2_000_000n
      ),
    { stakeSigns: false }
  ));

await test("cert round-trip: delegation", () =>
  certRoundTrip(
    (hash) =>
      api.certificate.mainnetEra.makeStakeAddressStakeDelegationCertificate(
        hash,
        DUMMY_POOL
      ),
    { stakeSigns: true }
  ));

await test("cert round-trip: unregistration", () =>
  certRoundTrip(
    (hash) =>
      api.certificate.mainnetEra.makeStakeAddressUnregistrationCertificate(
        hash,
        2_000_000n
      ),
    { stakeSigns: true }
  ));

// ------------------------------------------------------- 5. inspectAddress

await test("inspectAddress", async () => {
  assert(testnetAddr && mainnetAddr, "prerequisite: generate must have succeeded");

  const t = await api.inspectAddress(testnetAddr);
  assert(
    t && t.network === "testnet",
    `expected {network:"testnet"}, got: ${JSON.stringify(t)}`
  );

  const m = await api.inspectAddress(mainnetAddr);
  assert(
    m && m.network === "mainnet",
    `expected {network:"mainnet"}, got: ${JSON.stringify(m)}`
  );

  const garbage = await api.inspectAddress("hello world");
  assert(garbage === null, `garbage should be null, got: ${JSON.stringify(garbage)}`);

  const truncated = await api.inspectAddress(
    testnetAddr.slice(0, testnetAddr.length - 10)
  );
  assert(
    truncated === null,
    `truncated should be null, got: ${JSON.stringify(truncated)}`
  );

  const empty = await api.inspectAddress("");
  assert(empty === null, `empty string should be null, got: ${JSON.stringify(empty)}`);
});

// ------------------------------------- 6. newConwayTx known-broken probe

await test("newConwayTx known-broken probe", async () => {
  assert(
    typeof api.tx.newConwayTx === "function",
    "api.tx.newConwayTx is no longer advertised on the API"
  );
  try {
    await api.tx.newConwayTx();
    // Upstream fixed it: don't fail the suite, but make it visible.
    console.log(
      "NOTE newConwayTx now works upstream — the wasm export was added; " +
        "consider testing it properly and removing this probe."
    );
  } catch (e) {
    const msg = e && e.message ? e.message : String(e);
    assert(
      msg.includes("is not a function"),
      `expected the known missing-export error, got: ${msg}`
    );
  }
});

// ---------------------------------------------------------------- summary

const failed = results.filter((r) => !r.ok);
console.log(
  `\n${results.length - failed.length}/${results.length} tests passed`
);
process.exit(failed.length === 0 ? 0 : 1);
