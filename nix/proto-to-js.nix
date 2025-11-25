# proto-to-js.nix
{pkgs ? import <nixpkgs> {}}: let
  # Node dependencies
  node-deps = pkgs.buildNpmPackage {
    version = "1.0.0";
    name = "proto-js-dependencies";
    src = ../nix/proto-to-js-npm-deps;
    npmDepsHash = "sha256-3uvSQQ9+bWYAyHi6aCjfDhutY5pg4TKvIZFwOPfuqmg=";
    dontNpmBuild = true;
    dontNpmInstall = true;
    installPhase = ''
      mkdir -p $out
      cp -r node_modules $out/
    '';
  };

  cardano-rpc-src = ../cardano-rpc;
in
  pkgs.stdenv.mkDerivation {
    pname = "cardano-rpc-proto-js-bundle";
    version = "0.1.0";

    src = cardano-rpc-src;

    nativeBuildInputs = [
      pkgs.nodePackages.browserify
      pkgs.protobuf # For include paths if needed
    ];

    buildPhase = ''
      runHook preBuild

      PROTO_INCLUDE_PATH=$src/proto
      GEN_JS_PATH=./generated-js
      BUNDLE_PATH=./bundled-js

      mkdir -p "$GEN_JS_PATH"
      mkdir -p "$BUNDLE_PATH"

      echo "--- Generating JSON Schema Bundle (bundle.json) ---"

      # We use pbjs to generate the single JSON bundle that drives the logic
      ${node-deps}/node_modules/.bin/pbjs \
        -t json \
        -p "$PROTO_INCLUDE_PATH" \
        -o "$GEN_JS_PATH/bundle.json" \
        $(find $PROTO_INCLUDE_PATH | grep \\.proto$)

      echo "--- Creating Universal Bridge Entrypoint ---"

      BROWSER_ENTRYPOINT_FILE=$GEN_JS_PATH/browser-index.js

      cat <<EOF > "$BROWSER_ENTRYPOINT_FILE"
      const { GrpcWebClientBase, MethodDescriptor } = require('grpc-web');
      const protobuf = require('protobufjs');
      const bundle = require('./bundle.json');

      const root = protobuf.Root.fromJSON(bundle);

      // Identity Wrapper for grpc-web compatibility
      // We pass raw bytes, but grpc-web expects a class with serialize/deserialize
      class IdentityMessage {
        constructor(bytes) { this.bytes = bytes || new Uint8Array(0); }
        serializeBinary() { return this.bytes; }
        static deserializeBinary(bytes) { return new IdentityMessage(bytes); }
      }

      /**
       * The Universal Bridge Function
       * @param {string} hostUrl - The Envoy Proxy URL (e.g. "http://localhost:8080")
       * @param {string} serviceName - Full service name (e.g. "cardano.rpc.Node")
       * @param {string} methodName - Method name (e.g. "GetEra")
       * @param {string} payloadJson - JSON string payload
       */
      async function executeGrpcCall(hostUrl, serviceName, methodName, payloadJson) {
        // 1. Setup Client
        // format: 'text' ensures application/grpc-web-text (Base64)
        const client = new GrpcWebClientBase({ format: 'text' });

        const path = "/" + serviceName + "/" + methodName;

        // 2. Lookup Schema
        const serviceDef = root.lookupService(serviceName);
        if (!serviceDef) throw new Error("Service not found in bundle: " + serviceName);

        const methodDef = serviceDef.methods[methodName];
        if (!methodDef) throw new Error("Method not found: " + methodName);

        const RequestType = root.lookupType(methodDef.requestType);
        const ResponseType = root.lookupType(methodDef.responseType);

        // 3. JSON -> Bytes
        const payloadObj = JSON.parse(payloadJson);
        const err = RequestType.verify(payloadObj);
        if (err) throw new Error("Invalid JSON: " + err);

        const rawRequestBytes = RequestType.encode(RequestType.create(payloadObj)).finish();

        // 4. Execute gRPC-Web Call
        return new Promise((resolve, reject) => {
          client.rpcCall(
            hostUrl + path,
            new IdentityMessage(rawRequestBytes),
            {},
            new MethodDescriptor(
              path, "POST", IdentityMessage, IdentityMessage,
              (m) => m.serializeBinary(),
              (b) => IdentityMessage.deserializeBinary(b)
            ),
            (err, responseMsg) => {
              if (err) {
                console.error("[JS-Bridge] Error:", err);
                reject(err.message);
              } else {
                // 5. Bytes -> JSON
                const decoded = ResponseType.decode(responseMsg.bytes);
                const responseJson = ResponseType.toObject(decoded, {
                  longs: String, enums: String, bytes: String, defaults: true
                });
                resolve(JSON.stringify(responseJson));
              }
            }
          );
        });
      }

      // EXPOSE GLOBAL
      globalThis.executeGrpcCall = executeGrpcCall;
      EOF

      echo "--- Setting up node_modules for browserify ---"
      ln -s ${node-deps}/node_modules ./node_modules

      echo "--- Bundling JS Bridge with browserify ---"
      # We bundle the bridge and all deps (protobufjs, grpc-web) into one file
      browserify "$BROWSER_ENTRYPOINT_FILE" --standalone cardano_node > "$BUNDLE_PATH/cardano_node_grpc_web_pb.js"

      echo "--- Bundling complete. Final files are in $BUNDLE_PATH ---"
      ls -l "$BUNDLE_PATH"

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out

      echo "--- Installing browser bundle to \$out/ ---"
      cp ./bundled-js/cardano_node_grpc_web_pb.js $out/

      echo "--- Installing bundle.json to \$out/ ---"
      cp ./generated-js/bundle.json $out/

      runHook postInstall
    '';

    dontConfigure = true;
  }
