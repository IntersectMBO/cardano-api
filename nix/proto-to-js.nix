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
      pkgs.quicktype
      pkgs.grpc-tools
      pkgs.perl
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
      HASKELL_PATH=./node-hs

      ROOT_MODULE="Cardano.Rpc.Gen"

      mkdir -p "$GEN_JS_PATH"
      mkdir -p "$GEN_TS_PATH"
      mkdir -p "$BUNDLE_PATH"
      mkdir -p "$HASKELL_PATH"

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

      echo "--- Compiling .proto files (Pass 2: TS Generation) ---"

      for PROTO_FILE in `find "$PROTO_INCLUDE_PATH" -type f -name "*.proto"`
      do
        # Relative path to the proto file
        REL_PATH="''${PROTO_FILE#$PROTO_INCLUDE_PATH/}"

        # Relative path to the proto file dir
        REL_DIR=$(dirname "$REL_PATH")

        # Now we generate the Haskell
        # ts-proto generates the TS file at $GEN_TS_PATH + REL_PATH with .ts extension
        TS_SOURCE_FILE="$GEN_TS_PATH/''${REL_PATH%.proto}.ts"

        # Generate TS for proto file
        protoc \
          -I="$PROTO_INCLUDE_PATH" \
          -I="${pkgs.protobuf}/include" \
          --plugin=protoc-gen-ts_proto=${node-deps}/node_modules/ts-proto/protoc-gen-ts_proto \
          --ts_proto_out="$GEN_TS_PATH" \
          --ts_proto_opt=outputServices=false \
          --ts_proto_opt=esModuleInterop=true \
          --ts_proto_opt=onlyTypes=true \
          --ts_proto_opt=forceLong=string \
          --ts_proto_opt=stringEnums=true \
          "$PROTO_FILE"

        # Patch the typescript (replace Uint8Array with string)
        perl -pi -e 's/\bUint8Array\b/string/g' "$TS_SOURCE_FILE"

      done

      echo "--- Copying .ts files to final generated directory ---"

      (cd "$GEN_TS_PATH" && find . -name "*.ts" -exec cp --parents -t "../$GEN_JS_PATH" {} +)

      echo "--- Compiling .proto files (Pass 3: Haskell Generation) ---"

      for PROTO_FILE in `find "$PROTO_INCLUDE_PATH" -type f -name "*.proto"`
      do
        # Relative path to the proto file
        REL_PATH="''${PROTO_FILE#$PROTO_INCLUDE_PATH/}"

        # Relative path to the proto file dir
        REL_DIR=$(dirname "$REL_PATH")

        # Proto file name without extension
        FILENAME=$(basename "$REL_PATH" .proto)
        FILENAME_CAP=$(echo "$FILENAME" | sed 's/^./\U&/')

        # Convert path to a Haskell module name
        # Transform:
        #   a. Add the file name to the dir
        RAW_MODULE_PATH="$REL_DIR/$FILENAME"
        #   b. Replace / with .
        RAW_MODULE_PATH=''${RAW_MODULE_PATH#./}
        #   c. Capitalize the first letter of every word and clean up the ./ prefix if it exists
        MODULE_SUFFIX=$(echo "$RAW_MODULE_PATH" | sed -r 's/(^|\/)([a-z])/\1\U\2/g' | sed 's/\//./g')

        # Add the root module path
        FULL_MODULE_NAME="$ROOT_MODULE.$MODULE_SUFFIX"

        # Module name for the Haskell module with GRPC path info
        FULL_PATHS_MODULE_NAME="''${FULL_MODULE_NAME}_Paths"

        # We convert the module dots back to slashes for the physical path
        HASKELL_OUT_DIR="$HASKELL_PATH/''${FULL_MODULE_NAME//.//}"

        # We get the path to the containing dir
        HASKELL_OUT_PARENT=$(dirname "$HASKELL_OUT_DIR")

        # Create the containing dir (and all parent dirs that doen't exist)
        mkdir -p "$HASKELL_OUT_PARENT"

        # Now we generate the Haskell
        # ts-proto generates the TS file at $GEN_TS_PATH + REL_PATH with .ts extension
        TS_SOURCE_FILE="$GEN_TS_PATH/''${REL_PATH%.proto}.ts"

        if [ -f "$TS_SOURCE_FILE" ]; then
            echo " -> Generating Haskell: $FULL_MODULE_NAME"

            ${pkgs.quicktype}/bin/quicktype \
              --src "$TS_SOURCE_FILE" \
              --src-lang typescript \
              --lang haskell \
              --out "$HASKELL_OUT_PARENT/$FILENAME_CAP.hs" \
              --module "$FULL_MODULE_NAME"
        else
            echo "WARNING: Could not find generated TS file: $TS_SOURCE_FILE"
        fi

        # Finally generate the GRPC service path info
        # A sibling file (e.g. Query_Paths.hs) in the same folder

        awk -v modName="$FULL_PATHS_MODULE_NAME" '
            BEGIN {
                print "module " modName " where";
                pkg = "";
                svc = "";
            }
            /^\s*package/ { pkg = $2; gsub(";", "", pkg); }
            /^\s*service/ { svc = $2; gsub("{", "", svc); }
            /^\s*rpc/ {
                # $2 might be "GetEra" or "GetEra(Request)"
                # We split by "(" to ensure we only get the name
                split($2, a, "(");
                method = a[1];

                # Clean up any trailing garbage just in case
                gsub(/[^a-zA-Z0-9_]/, "", method);

                fullPath = "/" pkg "." svc "/" method;

                print "";
                print "-- | gRPC Path for " method;
                print "method_" method " :: String";
                print "method_" method " = \"" fullPath "\"";
            }
          ' "$PROTO_FILE" > "$HASKELL_OUT_PARENT/''${FILENAME_CAP}_Paths.hs"
      done


      echo "--- Copying .hs files to final generate directory ---"

      cp -r $HASKELL_PATH $GEN_JS_PATH/

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

      echo "--- Installing Haskell type modules to \$out/node-hs/ ---"
      mkdir -p $out/node-hs
      (cd ./generated-js/node-hs && find . -name "*.hs" | xargs cp --parents -t $out/node-hs)

      echo "--- Installation complete. Final output structure in \$out: ---"
      ls -R $out

      runHook postInstall
    '';

    dontConfigure = true;
  }
