import { createInitializer } from './main.js';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import path from 'path';
import { WASI } from '@bjorn3/browser_wasi_shim';
import * as grpc from '@grpc/grpc-js';
import protobuf from 'protobufjs';
import bundle from './bundle.json';

const root = protobuf.Root.fromJSON(bundle);
const clientCache = new Map();

async function executeGrpcCall(hostUrl, serviceName, methodName, payloadJson) {

  let client = clientCache.get(hostUrl);
  if (!client) {
    const GenericClient = grpc.makeGenericClientConstructor({}, 'GenericService');
    client = new GenericClient(hostUrl, grpc.credentials.createInsecure());
    clientCache.set(hostUrl, client);
  }
  const serviceDef = root.lookupService(serviceName);
  if (!serviceDef) throw new Error(`Service ${serviceName} not found in bundle`);

  const methodDef = serviceDef.methods[methodName];
  if (!methodDef) throw new Error(`Method ${methodName} not found`);

  const RequestType = root.lookupType(methodDef.requestType);
  const ResponseType = root.lookupType(methodDef.responseType);

  const payloadObj = JSON.parse(payloadJson);
  const err = RequestType.verify(payloadObj);
  if (err) throw new Error(`Invalid JSON for ${methodName}: ${err}`);

  const reqMsg = RequestType.create(payloadObj);
  const reqBuffer = Buffer.from(RequestType.encode(reqMsg).finish());

  const path = `/${serviceName}/${methodName}`;

  return new Promise((resolve, reject) => {
    client.makeUnaryRequest(
      path,
      (x) => x, // Serializer: Pass-through
      (x) => x, // Deserializer: Pass-through
      reqBuffer,
      (err, responseBuffer) => {
        if (err) {
          reject(new Error(`gRPC Error: ${err.message}`));
        } else {
          const decoded = ResponseType.decode(responseBuffer);
          const responseJson = ResponseType.toObject(decoded, {
            longs: String,
            enums: String,
            bytes: String,
            defaults: true
          });
          resolve(JSON.stringify(responseJson));
        }
      }
    );
  });
}

globalThis.executeGrpcCall = executeGrpcCall;

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
  return {};
};

const initialise = createInitializer(getWasi, loadWasmModule, createClient);
export default initialise;
