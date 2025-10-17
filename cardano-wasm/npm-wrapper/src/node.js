import { createInitializer } from './main.js';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import path from 'path';
import { WASI } from '@bjorn3/browser_wasi_shim';
import * as grpc_js from '@grpc/grpc-js';
import * as Clients from './node/node-index.js';
import { promisify } from 'node:util';

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

function promisifyClient(client) {
  const promisedClient = {};

  for (const key of Object.keys(Object.getPrototypeOf(client))) {
    // Ensure it is an API method
    if (key[0] !== '_' && typeof client[key] === 'function') {
      // We use .bind(client) to ensure the method is called
      // in the correct context of the client instance.
      promisedClient[key] = promisify(client[key].bind(client));
    }
  }
  return promisedClient;
}

const createClient = (address) => {
  const credentials = grpc_js.credentials.createInsecure();
  globalThis.cardano_node = {
    node: Clients.node_messages,
    query: Clients.query_messages,
    submit: Clients.submit_messages
  };
  return {
    node: promisifyClient(new Clients.default.node.NodeClient(address, credentials)),
    query: promisifyClient(new Clients.default.query.QueryServiceClient(address, credentials)),
    submit: promisifyClient(new Clients.default.submit.SubmitServiceClient(address, credentials))
  };
};

const initialise = createInitializer(getWasi, loadWasmModule, createClient);
export default initialise;

