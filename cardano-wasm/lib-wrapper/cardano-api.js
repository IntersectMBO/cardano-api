/// <reference path="./cardano-api.d.ts" />

import { WASI } from "https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js";
import ghc_wasm_jsffi from "./cardano-wasm.js";
const __exports = {};
const wasi = new WASI([], [], []);
async function initialise() {
  let { instance } = await WebAssembly.instantiateStreaming(fetch("./cardano-wasm.wasm"), {
    ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  Object.assign(__exports, instance.exports);
  wasi.initialize(instance);

  // Wrap a function with variable arguments to make the parameters inspectable
  function fixateArgs(params, func) {
    const paramString = params.map(p => p.name).join(',');
    // Dynamically create a function that captures 'func' from the closure.
    // 'this' and 'arguments' are passed through from the wrapper to 'func'.
    // Using eval allows the returned function to have named parameters for inspectability.
    const wrapper = eval(`
      (function(${paramString}) {
        return func.apply(this, arguments);
      })
    `);
    return wrapper;
  }

  // Same as fixateArgs but for async functions
  async function fixateArgsAsync(params, func) {
    const paramString = params.map(p => p.name).join(',');
    // Dynamically create an async function.
    const wrapper = eval(`
      (async function(${paramString}) {
        return await func.apply(this, arguments);
      })
    `);
    return wrapper;
  }

  // Dynamically build the API
  const apiInfo = await instance.exports.getApiInfo();
  let makers = {};
  let cardanoAPI = { objectType: "cardano-api" };
  // Create maker functions for each virtual object type
  apiInfo.virtualObjects.forEach(vo => {
    makers[vo.objectName] = function (initialHaskellValuePromise) {
      // currentHaskellValueProvider is a function that returns a Promise for the Haskell value
      // It starts with the initial value promise and fluent methods accumulate transformations
      let currentHaskellValueProvider = () => initialHaskellValuePromise;
      let jsObject = { objectType: vo.objectName };

      vo.methods.forEach(method => {
        if (method.return.type === "fluent") {
          // Fluent methods are synchronous and update the provider
          // A fluent method is one that returns the same object type
          jsObject[method.name] = fixateArgs(method.params, function (...args) {
            const previousProvider = currentHaskellValueProvider;
            // We update the provider so that it resolves the previous provider and chains the next call
            currentHaskellValueProvider = async () => {
              const prevHaskellValue = await previousProvider();
              return instance.exports[method.name](prevHaskellValue, ...args);
            };
            return jsObject; // Return current object for supporting chaining
          });
        } else {
          // Non-fluent methods (newObject or other) are async and apply the accumulated method calls
          jsObject[method.name] = fixateArgs(method.params, async function (...args) {
            const haskellValue = await currentHaskellValueProvider(); // Resolve accumulated method calls
            const resultPromise = instance.exports[method.name](haskellValue, ...args); // Call the non-fluent method

            if (method.return.type === "newObject") { // It returns a new object
              return makers[method.return.objectType](resultPromise);
            } else { // It returns some primitive or other JS type (not a virtual object)
              return resultPromise;
            }
          });
        }
      });
      return jsObject;
    };
  });

  // Populate the main API object with static methods
  apiInfo.mainObject.methods.forEach(method => {
    cardanoAPI[method.name] = async function (...args) {
      const resultPromise = instance.exports[method.name](...args);

      if (method.return.type === "newObject") { // Create a new object
        return makers[method.return.objectType](resultPromise);
      } else { // Return some primitive or other JS type (not a virtual object)
        return resultPromise;
      }
    };
  });
  return cardanoAPI;
}

window.base64ToHex = function (base64) {
  // Decode Base64 to raw binary string
  const binary = atob(base64);
  let hex = '';
  for (let i = 0; i < binary.length; i++) {
    // Convert each char code to a 2-digit hex string
    let byte = binary.charCodeAt(i).toString(16);
    if (byte.length < 2) byte = '0' + byte;
    hex += byte;
  }
  return hex;
}

export default initialise;
