# proto-to-js.nix
{pkgs ? import <nixpkgs> {}}: let
  # Node dependencies
  node-deps = pkgs.buildNpmPackage {
    version = "1.0.0";
    name = "proto-js-dependencies";
    src = ../nix/proto-to-js-npm-deps;
    npmDepsHash = "sha256-cF6OP9YkQ3m9d0+ijXZb8AUWrf+VTZjykk/ApgwJMag=";
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
      BUNDLE_PATH=./bundled-js

      mkdir -p "$GEN_JS_PATH"
      mkdir -p "$BUNDLE_PATH"

      echo "--- Compiling .proto files in $PROTO_INCLUDE_PATH ---"

      # Find all .proto files and compile them.
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

      echo "--- Compilation finished. Generated files are in $GEN_JS_PATH ---"
      ls -R "$GEN_JS_PATH"

      # Generate JS file that imports the generated files for Browserify
      ENTRYPOINT_FILE=$GEN_JS_PATH/index.js
      echo "--- Creating browserify entrypoint: $ENTRYPOINT_FILE ---"

      # Ensure the entrypoint file is empty before we start.
      rm -f $ENTRYPOINT_FILE
      touch "$ENTRYPOINT_FILE"

      # Find all *_grpc_web_pb.js files and build the entrypoint content.
      for JS_FULLPATH in `find "$GEN_JS_PATH" -type f -name "*_grpc_web_pb.js"`
      do
        # Get the filename, e.g., "node_grpc_web_pb.js"
        JS_FILENAME=$(basename "$JS_FULLPATH")

        # Extract the module name by removing the suffix
        MODULE_NAME=''${JS_FILENAME%_grpc_web_pb.js}

        # Get the path relative to GEN_JS_PATH for the require() statement.
        RELATIVE_PATH=''${JS_FULLPATH#$GEN_JS_PATH/}

        echo "Adding module '$MODULE_NAME' from './$RELATIVE_PATH' to bundle."
        # Append the export line to our entrypoint file.
        # This creates the desired submodule structure.
        echo "exports.$MODULE_NAME = require('./$RELATIVE_PATH');" >> "$ENTRYPOINT_FILE"
      done

      echo "--- Generated entrypoint content: ---"
      cat "$ENTRYPOINT_FILE"

      echo "--- Setting up node_modules for browserify ---"
      ln -s ${node-deps}/node_modules ./node_modules

      echo "--- Bundling all generated JS gRPC modules with browserify ---"

      # Bundle the entrypoint file into a single standalone module.
      # The --standalone flag exposes the exports under the 'cardano_node' global variable.
      browserify "$ENTRYPOINT_FILE" --standalone cardano_node > "$BUNDLE_PATH/cardano_node_grpc_web_pb.js"

      echo "--- Bundling complete. Final file is in $BUNDLE_PATH ---"
      ls -l "$BUNDLE_PATH"

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out

      echo "--- Installing browser bundle ---"
      cp ./bundled-js/cardano_node_grpc_web_pb.js $out/

      echo "--- Installing Node.js modules ---"
      mkdir -p $out/node
      cp -r ./generated-js/* $out/node/

      echo "--- Installation complete. Output structure: ---"
      ls -R $out

      runHook postInstall
    '';

    dontConfigure = true;
  }
