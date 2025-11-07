/// <reference path="./cardano-api.d.ts" />

/**
 * This module abstracts the resource fetching so that
 * it can be done in different ways depending on the
 * environment (e.g: node.js, webpack, vite, vanilla js).
 *
 * The entry point is a wrapper that is specific for
 * each environment. For example, "cardano-api.js"
 * provides an entry point for use from vanilla JS from
 * the browser directly. We will provide other wrappers
 * for the NPM package (e.g: "browser.js" and "node.js").
 **/

const __exports = {};

export function createInitializer(getWasi, loadWasmModule, createClient) {
  /**
   * Global utilities module used in JS foreign imports in WASM
   */
  globalThis.cardanoWasm = {
    /**
     * Convert Base64 to Base16 encoding
     */
    base64ToHex: function (base64) {
      if (typeof atob === 'function') {
        const binary = atob(base64);
        return [...binary].reduce((hex, char) => {
          const byteHex = char.charCodeAt(0).toString(16).padStart(2, '0');
          return hex + byteHex;
        }, '');
      } else {
        return Buffer.from(base64, 'base64').toString('hex');
      }
    }
  }

  globalThis.createClient = createClient;

  return async function initialise() {

    const ghc_wasm_jsffi = (await eval(`import('./cardano-wasm.js')`)).default;

    const wasi = await getWasi();

    const importObject = {
      ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
      wasi_snapshot_preview1: wasi.wasiImport,
    };

    const wasmModule = await loadWasmModule(importObject);

    const { instance } = wasmModule;
    Object.assign(__exports, instance.exports);
    wasi.initialize(instance);

    // Wrap a function with variable arguments to make the parameters inspectable
    function fixateArgs(params, func) {
      const paramString = params.map(p => p.name).join(',');
      // Dynamically create a function that captures 'func' from the closure.
      // 'this' and 'arguments' are passed through from the wrapper to 'func'.
      // Using eval allows the returned function to have named parameters for inspectability.
      const wrapper = eval(`
        (function (f) {
          return (function (${paramString}) {
            return f.apply(this, arguments);
          });
        })
      `)(func);
      return wrapper;
    }

    // Same as fixateArgs but for async functions
    async function fixateArgsAsync(params, func) {
      const paramString = params.map(p => p.name).join(',');
      // Dynamically create an async function.
      const wrapper = eval(`
        (function (f) {
           return (async function (${paramString}) {
             return await f.apply(this, arguments);
           });
        })
      `)(func);
      return wrapper;
    }

    // Dynamically build the API
    const apiInfo = await instance.exports.getApiInfo();
    let makers = {};
    let cardanoApi = { objectType: "cardano-api" };

    // Create maker functions for each virtual object type

    function populateMethodOrGroup(methodPopulator, methodOrGroup, target) {
      if (methodOrGroup.type === "method") {
        methodPopulator(methodOrGroup.method, target);
      } else if (methodOrGroup.type === "group") {
        let group = methodOrGroup.group;
        target = target[group.name] = {};
        group.methods.forEach(methodOrGroup =>
          populateMethodOrGroup(methodPopulator, methodOrGroup, target)
        );
      }
    }

    apiInfo.virtualObjects.forEach(vo => {
      makers[vo.objectName] = function (initialHaskellValuePromise) {
        // currentHaskellValueProvider is a function that returns a Promise for the Haskell value
        // It starts with the initial value promise and fluent methods accumulate transformations
        let currentHaskellValueProvider = () => initialHaskellValuePromise;

        function populateDynamicMethod(method, target) {
          if (method.return.type === "fluent") {
            // Fluent methods are synchronous and update the provider
            // A fluent method is one that returns the same object type
            target[method.name] = fixateArgs(method.params, function (...args) {
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
            target[method.name] = fixateArgs(method.params, async function (...args) {
              const haskellValue = await currentHaskellValueProvider(); // Resolve accumulated method calls
              const resultPromise = instance.exports[method.name](haskellValue, ...args); // Call the non-fluent method

              if (method.return.type === "newObject") { // It returns a new object
                return makers[method.return.objectType](resultPromise);
              } else { // It returns some primitive or other JS type (not a virtual object)
                return resultPromise;
              }
            });
          }
        }

        let jsObject = { objectType: vo.objectName };

        vo.methods.forEach(method => {
          populateMethodOrGroup(populateDynamicMethod, method, jsObject);
        });
        return jsObject;
      };
    });

    // Populate the main API object with static methods

    function populateStaticMethod(method, target) {
      target[method.name] = async function (...args) {
        if (method.group) {
          if (!target[method.group]) {
            target[method.group] = {};
          }
          target = target[method.group];
        }
        const resultPromise = instance.exports[method.name](...args);

        if (method.return.type === "newObject") { // Create a new object
          return makers[method.return.objectType](resultPromise);
        } else { // Return some primitive or other JS type (not a virtual object)
          return resultPromise;
        }
      };
    }

    apiInfo.mainObject.methods.forEach(methodOrGroup => {
      populateMethodOrGroup(populateStaticMethod, methodOrGroup, cardanoApi);
    });

    return cardanoApi;
  }

};

