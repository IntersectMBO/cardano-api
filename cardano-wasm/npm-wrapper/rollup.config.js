import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import copy from 'rollup-plugin-copy';
import { string } from 'rollup-plugin-string';

const isProduction = process.env.NODE_ENV === 'production';

const plugins = [
  resolve({ browser: true }),
  string({
    include: '**/cardano_node_grpc_web_pb.js',
  }),
  commonjs(),
];

export default [
  // --- Node.js Build ---
  // Responsible for creating the CommonJS (.cjs) and ES Module (.mjs) files for Node.
  {
    input: 'src/node.js',
    output: [
      {
        file: 'dist/node.cjs',
        format: 'cjs',
        sourcemap: true,
        inlineDynamicImports: true,
      },
      {
        file: 'dist/node.mjs',
        format: 'esm',
        sourcemap: true,
        inlineDynamicImports: true,
      }
    ],
    plugins: [
      resolve({ preferBuiltins: true }),
      commonjs(),
      copy({
        targets: [
          { src: 'src/cardano-wasm.js', dest: 'dist' },
          { src: 'src/cardano-wasm.wasm', dest: 'dist' },
          { src: 'cardano_node_grpc_web_pb.js', dest: 'dist' },
          { src: 'src/*.d.ts', dest: 'dist' }
        ]
      })
    ],
    // Mark Node.js built-in modules as external so they are not bundled.
    external: ['fs/promises', 'path', 'url', 'wasi']
  },

  // --- Browser Builds ---
  // Responsible for creating browser-compatible bundles.
  {
    input: 'src/browser.js',
    output: [
      {
        file: 'dist/browser.umd.js',
        format: 'umd',
        name: 'cardanoApi',
        sourcemap: true,
        inlineDynamicImports: true,
      },
      {
        file: 'dist/browser.mjs',
        format: 'esm',
        sourcemap: true,
        inlineDynamicImports: true,
      }
    ],
    external: ['./cardano-wasm.wasm'],
    plugins
  }
];

