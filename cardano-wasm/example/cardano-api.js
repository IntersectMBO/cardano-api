import { WASI } from "https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js";
import ghc_wasm_jsffi from "./cardano-wasm.js";
const __exports = {};
const wasi = new WASI([], [], []);
async function initialize() {
  let {instance} = await WebAssembly.instantiateStreaming(fetch("./cardano-wasm.wasm"), {
    ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  Object.assign(__exports, instance.exports);
  wasi.initialize(instance);
  var makeSignedTxObject = async function (asyncValue) {
    let value = await asyncValue;
    return { objectType: "SignedTx"
           , txToCbor: async function () { return instance.exports.txToCbor(value); }
           };
  }
  var makeUnsignedTxObject = async function (asyncValue) {
    let value = await asyncValue;
    return { objectType: "UnsignedTx"
           , addTxInput: function (txId, txIx) { return makeUnsignedTxObject(instance.exports.addTxInput(value, txId, txIx)); }
           , addSimpleTxOut: function (destAddr, amount) { return makeUnsignedTxObject(instance.exports.addSimpleTxOut(value, destAddr, amount)); }
           , setFee: function (amount) { return makeUnsignedTxObject(instance.exports.setFee(value, amount)); }
           , addSigningKey: function (signingKey) { return makeUnsignedTxObject(instance.exports.addSigningKey(value, signingKey)); }
           , signTx: function () { return makeSignedTxObject(instance.exports.signTx(value)); }
           };
  };
  return { objectType: "cardano-api"
         , newConwayTx: async function () { return makeUnsignedTxObject(instance.exports.newConwayTx()); }
         };

}
export default initialize;
