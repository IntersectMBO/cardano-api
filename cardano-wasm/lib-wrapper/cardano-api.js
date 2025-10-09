/**
 * This is an entry point for the cardano-wasm API for
 * use from vanilla JS, from the browser without bundling
 * nor using NPM. See the README.md and examples folder
 * for more info.
 **/

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

