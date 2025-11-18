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

