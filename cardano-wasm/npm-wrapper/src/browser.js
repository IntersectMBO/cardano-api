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
    return {
        node: new cardano_node.node.NodePromiseClient(address, null, null),
        query: new cardano_node.query.QueryServicePromiseClient(address, null, null),
        submit: new cardano_node.submit.SubmitServicePromiseClient(address, null, null)
    }
}

const initialise = createInitializer(getWasiInstance, loadWasmModule, createClient);

export default initialise;

