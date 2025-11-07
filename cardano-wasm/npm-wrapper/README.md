
# cardano-wasm

JavaScript bindings to the WASM build of [cardano-api](https://github.com/intersectmbo/cardano-api). You can find more information about the WASM build in the [cardano-wasm](https://github.com/IntersectMBO/cardano-api/tree/master/cardano-wasm) subfolder.

This package provides functionalities to inspect, create, and manipulate transactions and other information related to Cardano blockchains, as well as interacting directly with them through the `cardano-node`.

-----

## 📦 Installation

To install the package, run the following command in your project's directory:

```bash
npm install cardano-wasm
```

-----

## 🚀 Usage

Because this library is a wrapper around a WebAssembly (WASM) module, you must first initialize it asynchronously. The dynamic `import()` function returns a promise that resolves to the module, and its `default` export is an `async` function that loads and prepares the API.

Here is a clean, modern example using an `async` IIFE (Immediately Invoked Function Expression) to get you started.

Create an `index.js` file:

```javascript
// A self-executing async function to initialize and use the Cardano API
(async () => {
  try {
    // 1. Dynamically import the package.
    const cardanoModule = await import('cardano-wasm');

    // 2. The module's default export is an async function that initializes the WASM instance.
    //    Await this to get the usable API object.
    const api = await cardanoModule.default();
    console.log("Cardano API loaded successfully!");

    // 3. Now you can use the API to perform Cardano operations.
    //    For example, let's create a new transaction body.
    const tx = await api.tx.newTx();
    console.log("New Transaction Body Created:", tx);

    // You can now build upon the 'tx' object to add inputs, outputs, etc.

  } catch (error) {
    console.error("Failed to load or use the Cardano API:", error);
  }
})();
```

### Node.js

To run this file, you can simply use Node.js:

```bash
node index.js
```

When using `node.js`, the `grpc` module will use normal GRPC and can be used with UNIX socket files as follows:

```bash
    const grpc = await api.newGrpcConnection("unix:///path/to/cardano-node/rpc/socket/node1/rpc.sock");
    const era = await grpc.getEra();
```

### Webpack

Alternatively you can use `cardano-wasm` as part of a `webpack` project, but you'll need to install `html-webpack-plugin` and `copy-webpack-plugin`:

```bash
npm install html-webpack-plugin copy-webpack-plugin
```

And do a couple of adjustments to the `webpack.config.js`:

```js
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');

module.exports = {
  entry: './src/index.js',

  target: ['web', 'es2020'],

  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
    publicPath: '',
  },

  experiments: {
    asyncWebAssembly: true,
  },

  devtool: 'source-map',

  plugins: [
    new HtmlWebpackPlugin({ template: './index.html' }),
    new CopyWebpackPlugin({
      patterns: [
        {
          from: 'node_modules/cardano-wasm/dist/cardano-wasm.js',
          to: 'cardano-wasm.js',
        },
      ],
    }),
  ],

  module: {
    rules: [
      {
        test: /\.wasm$/,
        type: 'asset/resource',
      },
    ],
  },

  mode: 'development',
};
```

Your `webpack.config.js` configuration may vary, but it could be necessary to adjust the `target`, `experiments`, `devtool`, `plugins`, and `module` keys as shown in the example above.

When using from the browser, the `grpc` module uses `web-grpc`, so the RPC socket from `cardano-node` needs to be proxied with a tool like `envoy` (see the [README.md](https://github.com/IntersectMBO/cardano-api/tree/master/cardano-wasm) at GitHub repo for more info).

-----

## 📖 API Reference

For a detailed understanding of all available functionalities, please refer to the official [**cardano-wasm documentation**](https://cardano-api.cardano.intersectmbo.org/cardano-wasm/typedoc/).

-----

## 🚗 Road map

So far, `cardano-wasm` supports:
- Basic wallet management.
- Basic transaction building.
- Transaction signing and submission through web-grpc.
- Basic node communication through web-grpc.

In the future, we aim to add support for:
- Core wallet and staking features:
  - Extended wallet management (with mnemonics and stake addresses)
  - Staking and Delegation (certificates)
  - Multi-asset and metadata support
- Native scripts and Plutus support
- Governance:
  - DRep management
  - Voting
  - Constitution and committee management
- Advanced transaction features
  - Validity interval
  - Reward withdrawals

In parallel, we will be working on expanding the support for querying the node through gRPC and web-grpc.

-----

## 📄 License

This project is licensed under **Apache-2.0 license**.
