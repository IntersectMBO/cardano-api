import { createInitializer } from './main.js';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import path from 'path';
import { WASI } from '@bjorn3/browser_wasi_shim';
import * as grpc_js from '@grpc/grpc-js';

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

const createClient = (address) => {
  const client = new grpc_js.Client(address, grpc_js.credentials.createInsecure());
  return {
    // Send a unary gRPC request, taking and returning the protobuf messages in base64
    unary: (path, request) => new Promise((resolve, reject) => {
      client.makeUnaryRequest(path, (bytes) => bytes, (bytes) => bytes, Buffer.from(request, 'base64'),
        (error, response) => error ? reject(error) : resolve(response.toString('base64')));
    })
  };
};

const initialise = createInitializer(getWasi, loadWasmModule, createClient);
export default initialise;

