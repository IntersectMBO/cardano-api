import { WASI } from "https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js";
import ghc_wasm_jsffi from "./cardano-wasm.js";
const __exports = {};
const wasi = new WASI([], [], []);
async function initialize() {
  let { instance } = await WebAssembly.instantiateStreaming(fetch("./cardano-wasm.wasm"), {
    ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  Object.assign(__exports, instance.exports);
  wasi.initialize(instance);

  // Dynamically build the API
  const apiInfo = await instance.exports.getAPIInfo();
  var makers = {};
  var cardanoAPI = { objectType: "cardano-api" };

  // Create maker functions for each virtual object type
  apiInfo.virtualObjects.forEach(vo => {
    makers[vo.objectName] = async function (asyncHaskellValue) {
      const haskellValue = await asyncHaskellValue;
      var jsObject = { objectType: vo.objectName };

      vo.methods.forEach(method => {
        jsObject[method.name] = function (...args) {
          const resultPromise = instance.exports[method.name](haskellValue, ...args);

          if (method.return.type === "fluent") { // Same object, fluent interface
            return makers[vo.objectName](resultPromise);
          } else if (method.return.type === "newObject") { // Convert to a new object
            return makers[method.return.objectType](resultPromise);
          } else { // Other return type, just return the result
            return resultPromise;
          }
        };
      });
      return jsObject;
    };
  });

  // Populate the main API object with static methods
  apiInfo.staticMethods.forEach(method => {
    cardanoAPI[method.name] = async function (...args) {
      const resultPromise = instance.exports[method.name](...args);

      if (method.return.type === "newObject") { // Create a new object
        return makers[method.return.objectType](resultPromise);
      } else { // Other return type, just return the result
        return resultPromise;
      }
    };
  });
  return cardanoAPI;
}
export default initialize;
