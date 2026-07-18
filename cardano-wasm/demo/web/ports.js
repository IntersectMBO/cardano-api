// Port glue: Elm ⇄ cardano-wasm. The wrapper does the Cardano work here —
// key generation and restoration, address encoding and inspection, transaction
// building, fee estimation and signing.
// Protocol parameters come from a pinned module (pparams.js): estimateMinFee
// requires the full cardano-ledger JSON format, which no CORS-friendly provider
// serves directly, and the fee-relevant fields are stable across networks. Refresh
// the file if a protocol update changes them.
import initialise from "./cardano-api.js";
import pparams from "./pparams.js";

const magic = { mainnet: undefined, preprod: 1, preview: 2 };

async function walletToJson(w) {
  return {
    address: await w.getAddressBech32(),
    keys: {
      paymentVKey: await w.getBech32ForPaymentVerificationKey(),
      paymentSKey: await w.getBech32ForPaymentSigningKey(),
      stakeVKey: await w.getBech32ForStakeVerificationKey(),
      stakeSKey: await w.getBech32ForStakeSigningKey(),
      paymentKeyHash: await w.getBase16ForPaymentVerificationKeyHash(),
      stakeKeyHash: await w.getBase16ForStakeVerificationKeyHash(),
    },
  };
}

async function restoreWallet(api, network, paymentSkey, stakeSkey) {
  return network === "mainnet"
    ? api.wallet.mainnet.restoreStakeWalletFromSigningKeyBech32(paymentSkey, stakeSkey)
    : api.wallet.testnet.restoreStakeWalletFromSigningKeyBech32(magic[network], paymentSkey, stakeSkey);
}

async function buildUnsigned(api, spec) {
  // Constructors are async (Promise<UnsignedTx>) — must await.
  // NOTE: newConwayTx is advertised by getApiInfo but NOT exported by the current wasm build,
  // so use newTx() for the current era (= Conway). newUpcomingEraTx() works for Dijkstra.
  let tx = await (spec.era === "dijkstra" ? api.tx.newUpcomingEraTx() : api.tx.newTx());
  // Fluent builders (addTxInput/addSimpleTxOut) are synchronous chainers.
  for (const i of spec.inputs) tx = tx.addTxInput(i.txId, i.txIx);
  for (const o of spec.outputs) tx = tx.addSimpleTxOut(o.address, BigInt(o.lovelace));
  return tx;
}

function wire(app, api) {
  app.ports.wasmGenerateWallet.subscribe(async ({ network }) => {
    try {
      const w = network === "mainnet"
        ? await api.wallet.mainnet.generateStakeWallet()
        : await api.wallet.testnet.generateStakeWallet(magic[network]);
      app.ports.wasmWalletGenerated.send(await walletToJson(w));
    } catch (e) { app.ports.wasmWalletGenerated.send({ error: String(e) }); }
  });

  app.ports.wasmRestoreWallet.subscribe(async ({ network, paymentSkey, stakeSkey }) => {
    try {
      const w = await restoreWallet(api, network, paymentSkey, stakeSkey);
      app.ports.wasmWalletRestored.send(await walletToJson(w));
    } catch (e) { app.ports.wasmWalletRestored.send({ error: String(e) }); }
  });

  app.ports.wasmDeriveAddresses.subscribe(async ({ network, wallets }) => {
    try {
      const out = [];
      for (const wk of wallets) {
        const w = await restoreWallet(api, network, wk.paymentSkey, wk.stakeSkey);
        out.push({ id: wk.id, address: await w.getAddressBech32() });
      }
      app.ports.wasmAddressesDerived.send(out);
    } catch (e) { app.ports.wasmAddressesDerived.send({ error: String(e) }); }
  });

  app.ports.wasmEstimateFee.subscribe(async ({ spec, paymentWits, stakeWits }) => {
    try {
      const tx = await buildUnsigned(api, spec);
      const fee = await tx.estimateMinFee(pparams, paymentWits + stakeWits, 0, 0);
      app.ports.wasmFeeEstimated.send({ fee: Number(fee) });
    } catch (e) { app.ports.wasmFeeEstimated.send({ error: String(e) }); }
  });

  app.ports.wasmSignTx.subscribe(async ({ spec, paymentKeys, stakeKeys }) => {
    try {
      let tx = await buildUnsigned(api, spec);
      tx = tx.setFee(BigInt(spec.fee)); // fluent, synchronous
      // Payment witnesses (for the spent inputs) — signWithPaymentKey / alsoSignWithPaymentKey.
      let signed = await tx.signWithPaymentKey(paymentKeys[0]); // async → SignedTx
      for (const k of paymentKeys.slice(1)) signed = signed.alsoSignWithPaymentKey(k); // fluent, sync
      // Stake witnesses — alsoSignWithStakeKey (used once certificates arrive).
      for (const k of stakeKeys) signed = signed.alsoSignWithStakeKey(k); // fluent, sync
      const cbor = await signed.txToCbor(); // async → hex
      const txId = await signed.getTxId(); // async → 64-char hex (body hash; witness-independent)
      app.ports.wasmTxSigned.send({ cbor, txId });
    } catch (e) { app.ports.wasmTxSigned.send({ error: String(e) }); }
  });

  app.ports.wasmInspectAddress.subscribe(async ({ address }) => {
    try {
      const r = await api.inspectAddress(address); // { network } | null — never throws
      app.ports.wasmAddressInspected.send({ address, network: r ? r.network : "invalid" });
    } catch (e) {
      // Belt and braces: treat an unexpected throw as invalid rather than leaving the check pending.
      app.ports.wasmAddressInspected.send({ address, network: "invalid" });
    }
  });

  app.ports.clipboardWrite.subscribe((t) => { if (navigator.clipboard) navigator.clipboard.writeText(t); });
}

async function boot() {
  const api = await initialise();
  const app = window.Elm.Main.init({
    node: document.getElementById("app"),
    // The two numbers Elm needs for its balance arithmetic come from the same
    // pinned object, so there is a single source of truth.
    flags: { keyDeposit: pparams.stakeAddressDeposit, coinsPerUtxoByte: pparams.utxoCostPerByte },
  });
  wire(app, api);
}

boot().catch((e) => {
  document.getElementById("app").innerHTML =
    '<div style="color:#e6edff;font-family:sans-serif;padding:30px">Failed to load cardano-wasm:<br><pre>' +
    String(e) + "</pre>Make sure the page is served over http(s), not opened as a file://</div>";
});
