# proto-to-js.nix
{pkgs ? import <nixpkgs> {}}: let
  # Node dependencies
  node-deps = pkgs.buildNpmPackage {
    version = "1.0.0";
    name = "proto-js-dependencies";
    src = ../nix/proto-to-js-npm-deps;
    npmDepsHash = "sha256-s0qG33GOJORk2fzlOH1xJkzRsVYAKrZmNL5nX3dLJZ0=";
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
      pkgs.grpc-tools
      pkgs.protobuf
      pkgs.protoc-gen-js
      pkgs.protoc-gen-grpc-web
      pkgs.nodePackages.browserify
    ];

    buildPhase = ''
      runHook preBuild

      PROTO_INCLUDE_PATH=$src/proto
      GEN_JS_PATH=./generated-js
      GEN_TS_PATH=./node-ts
      BUNDLE_PATH=./bundled-js

      mkdir -p "$GEN_JS_PATH"
      mkdir -p "$GEN_TS_PATH"
      mkdir -p "$BUNDLE_PATH"

      echo "--- Compiling .proto files (Pass 1: JavaScript Generation) ---"
      for PROTO_FILE in `find "$PROTO_INCLUDE_PATH" -type f -name "*.proto"`
      do
        protoc \
          -I="$PROTO_INCLUDE_PATH" \
          -I="${pkgs.protobuf}/include" \
          --plugin=protoc-gen-grpc=${pkgs.grpc-tools}/bin/grpc_node_plugin \
          --js_out=import_style=commonjs,binary:"$GEN_JS_PATH" \
          --grpc-web_out=import_style=commonjs,mode=grpcwebtext:"$GEN_JS_PATH" \
          --grpc_out=grpc_js,import_style=commonjs:"$GEN_JS_PATH" \
          "$PROTO_FILE"
      done

      echo "--- Compiling .proto files (Pass 2: TypeScript Declaration Generation) ---"

      for PROTO_FILE in `find "$PROTO_INCLUDE_PATH" -type f -name "*.proto"`
      do
        protoc \
          -I="$PROTO_INCLUDE_PATH" \
          -I="${pkgs.protobuf}/include" \
          --plugin=protoc-gen-ts_proto=${node-deps}/node_modules/ts-proto/protoc-gen-ts_proto \
          --ts_proto_out="$GEN_TS_PATH" \
          --ts_proto_opt=outputServices=false \
          --ts_proto_opt=esModuleInterop=true \
          --ts_proto_opt=oneof=unions \
          --ts_proto_opt=onlyTypes=true \
          --ts_proto_opt=forceLong=string \
          "$PROTO_FILE"
      done

      echo "--- Copying .ts files to final generated directory ---"
      (cd "$GEN_TS_PATH" && find . -name "*.ts" -exec cp --parents -t "../$GEN_JS_PATH" {} +)


      echo "--- Final generated files are in $GEN_JS_PATH ---"
      ls -R "$GEN_JS_PATH"

      echo "--- Creating node.js entrypoint: $NODE_ENTRYPOINT_FILE ---"
      NODE_ENTRYPOINT_FILE=$GEN_JS_PATH/node-index.js
      rm -f $NODE_ENTRYPOINT_FILE
      touch "$NODE_ENTRYPOINT_FILE"
      for JS_FULLPATH in `find "$GEN_JS_PATH" -type f -name "*_grpc_pb.js"`
      do
        JS_FILENAME=$(basename "$JS_FULLPATH")
        MODULE_NAME=''${JS_FILENAME%_grpc_pb.js}
        RELATIVE_PATH=''${JS_FULLPATH#$GEN_JS_PATH/}

        echo "exports.$MODULE_NAME = require('./$RELATIVE_PATH');" >> "$NODE_ENTRYPOINT_FILE"

        MESSAGE_RELATIVE_PATH=''${RELATIVE_PATH%_grpc_pb.js}_pb.js
        echo "exports.''${MODULE_NAME}_messages = require('./$MESSAGE_RELATIVE_PATH');" >> "$NODE_ENTRYPOINT_FILE"
      done

      echo "--- Creating browserify entrypoint: $BROWSER_ENTRYPOINT_FILE ---"
      BROWSER_ENTRYPOINT_FILE=$GEN_JS_PATH/browser-index.js
      rm -f $BROWSER_ENTRYPOINT_FILE
      touch "$BROWSER_ENTRYPOINT_FILE"
      for JS_FULLPATH in `find "$GEN_JS_PATH" -type f -name "*_grpc_web_pb.js"`
      do
        JS_FILENAME=$(basename "$JS_FULLPATH")
        MODULE_NAME=''${JS_FILENAME%_grpc_web_pb.js}
        RELATIVE_PATH=''${JS_FULLPATH#$GEN_JS_PATH/}

        echo "Adding module '$MODULE_NAME' from './$RELATIVE_PATH' to browser bundle."
        echo "exports.$MODULE_NAME = require('./$RELATIVE_PATH');" >> "$BROWSER_ENTRYPOINT_FILE"
      done

      echo "--- Setting up node_modules for browserify ---"
      ln -s ${node-deps}/node_modules ./node_modules

      echo "--- Bundling all generated JS gRPC modules with browserify ---"
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


      echo "--- Installing Node.js modules to \$out/node/ ---"
      mkdir -p $out/node
      (cd ./generated-js && find . -name "*.js" -not -name "*_grpc_web_pb.js" -not -name "browser-index.js" | xargs cp --parents -t $out/node)

      echo "--- Installing TypeScript declarations to \$out/node-ts/ ---"
      mkdir -p $out/node-ts
      (cd ./generated-js && find . -name "*.ts" | xargs cp --parents -t $out/node-ts)


      echo "--- Installation complete. Final output structure in \$out: ---"
      ls -R $out

      runHook postInstall
    '';

    dontConfigure = true;
  }
