import { createInitializer } from './main.js';
import { WASI } from '@bjorn3/browser_wasi_shim';
import wasmUrl from './cardano-wasm.wasm';
import grpcWebScript from './cardano_node_grpc_web_pb.js';

const script = document.createElement('script');
script.textContent = grpcWebScript;
document.head.appendChild(script);

const getWasiInstance = async () => {
    return new WASI([], [], []);
};

const loadWasmModule = async (importObject) => {
    const response = await fetch(wasmUrl);
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

const initialise = createInitializer(getWasiInstance, loadWasmModule, createClient);

export default initialise;

