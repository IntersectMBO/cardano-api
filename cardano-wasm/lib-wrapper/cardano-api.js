/**
 * This is an entry point for the cardano-wasm API for
 * use from vanilla JS, from the browser without bundling
 * nor using NPM. See the README.md and examples folder
 * for more info.
 **/

import { createInitializer } from './main.js';

const wasmUrl = './cardano-wasm.wasm';

const getWasi = async () => {
  const { WASI } = await import("https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js");
  return new WASI([], [], []);
};

const loadWasmModule = async (importObject) => {
  const response = await fetch("./cardano-wasm.wasm");
  return await WebAssembly.instantiateStreaming(response, importObject);
};

const createClient = function (address) {
  const grpcWeb = cardano_node.grpcWeb;
  const client = new grpcWeb.GrpcWebClientBase({ format: 'text' });
  return {
    // Send a unary gRPC request, taking and returning the protobuf messages in base64
    unary: async function (path, request) {
      const methodDescriptor = new grpcWeb.MethodDescriptor(
        path, grpcWeb.MethodType.UNARY, Uint8Array, Uint8Array, (bytes) => bytes, (bytes) => bytes);
      const response = await client.thenableCall(
        address + path, Uint8Array.from(atob(request), (char) => char.charCodeAt(0)), {}, methodDescriptor);
      let binary = '';
      for (let i = 0; i < response.length; i += 0x8000) {
        binary += String.fromCharCode.apply(null, response.subarray(i, i + 0x8000));
      }
      return btoa(binary);
    }
  }
}

const initialise = createInitializer(getWasi, loadWasmModule, createClient);
export default initialise;

