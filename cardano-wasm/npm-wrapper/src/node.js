import { createInitializer } from './main.js';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import path from 'path';
import { WASI } from '@bjorn3/browser_wasi_shim';

const getWasi = async () => {
    return new WASI([], [], []);
};

const loadWasmModule = async (importObject) => {
  const __dirname = path.dirname(fileURLToPath(import.meta.url));
  const wasmPath = path.resolve(__dirname, 'cardano-wasm.wasm');
  const wasmBuffer = await readFile(wasmPath);
  const { module } = await WebAssembly.instantiate(wasmBuffer, importObject);
  return { instance: new WebAssembly.Instance(module, importObject), module };
};

const initialise = createInitializer(getWasi, loadWasmModule);
export default initialise;

