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
  return { mkTransaction: instance.exports.mkTransaction
         , signTransaction: instance.exports.signTransaction
         , mkTxIn: instance.exports.mkTxIn
         };
}
export default initialize;
