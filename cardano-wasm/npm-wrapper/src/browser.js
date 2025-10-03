import { createInitializer } from './main.js';
import { WASI } from '@bjorn3/browser_wasi_shim';
import wasmUrl from './cardano-wasm.wasm';

const getWasiInstance = async () => {
    return new WASI([], [], []);
};

const loadWasmModule = async (importObject) => {
    const response = await fetch(wasmUrl);
    return await WebAssembly.instantiateStreaming(response, importObject);
};

const initialise = createInitializer(getWasiInstance, loadWasmModule);

export default initialise;

