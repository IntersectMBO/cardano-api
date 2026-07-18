// Port glue: Elm ⇄ cardano-wasm. The wrapper does the Cardano work here —
// key generation and restoration, address encoding and inspection.
import initialise from "./cardano-api.js";

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
  const app = window.Elm.Main.init({ node: document.getElementById("app") });
  wire(app, api);
}

boot().catch((e) => {
  document.getElementById("app").innerHTML =
    '<div style="color:#e6edff;font-family:sans-serif;padding:30px">Failed to load cardano-wasm:<br><pre>' +
    String(e) + "</pre>Make sure the page is served over http(s), not opened as a file://</div>";
});
