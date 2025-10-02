import { createInitializer } from './main.js';

export default function(wasmUrl) {
    const getWasi = async () => {
        const { WASI } = await import("https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js");
        return new WASI([], [], []);
    };

    const loadWasmModule = async (importObject) => {
        const response = await fetch(wasmUrl);
        return await WebAssembly.instantiateStreaming(response, importObject);
    };

    const initialise = createInitializer(getWasi, loadWasmModule);
    return initialise;
}

