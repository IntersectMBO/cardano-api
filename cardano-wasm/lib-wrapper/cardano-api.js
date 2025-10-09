import { createInitializer } from './main.js';

const wasmUrl = './cardano-wasm.wasm';

const getWasi = async () => {
    return (await import("https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js")).WASI;
};

const loadWasmModule = async (importObject) => {
  const response = await fetch("./cardano-wasm.wasm");
  return await WebAssembly.instantiateStreaming(response, importObject);
};

const initialise = createInitializer(getWasi, loadWasmModule);
export default initialise;

