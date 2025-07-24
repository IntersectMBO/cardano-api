# proto-to-js.nix

{ pkgs ? import <nixpkgs> {} }:

let

  # Node dependencies
  node-deps = pkgs.buildNpmPackage {
    version = "1.0.0";
    name = "proto-js-dependencies";
    src = ../nix/npm-deps;
    npmDepsHash = "sha256-b8x9xZ0dCu1cvILF0HPVVLfkCGHOWCcPUKyC2x1gQ+c=";
    dontNpmBuild = true;
    dontNpmInstall = true;
    installPhase = ''
      mkdir -p $out
      cp -r node_modules $out/
    '';
  };

  cardano-rpc-src = ../cardano-rpc;

in pkgs.stdenv.mkDerivation {
  pname = "cardano-rpc-proto-js-bundle";
  version = "0.1.0";

  src = cardano-rpc-src;

  nativeBuildInputs = [
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

    echo "--- Compiling .proto file: $PROTO_FILE ---"

    for PROTO_FILE in `find "$PROTO_INCLUDE_PATH" -type f -name "*.proto"`
    do
      protoc \
        -I="$PROTO_INCLUDE_PATH" \
        --js_out=import_style=commonjs,binary:"$GEN_JS_PATH" \
        --grpc-web_out=import_style=commonjs,mode=grpcwebtext:"$GEN_JS_PATH" \
        "$PROTO_FILE"
    done

    echo "--- Compilation finished. Generated files are in $GEN_JS_PATH ---"
    ls -R "$GEN_JS_PATH"

    # Check if there are any files in the top-level generated directory
    if [ ! "$(ls -1 "$GEN_JS_PATH" | head -n 1)" ]; then
      echo "Error: protoc did not generate any gRPC-Web files!"
      exit 1
    fi

    echo "--- Setting up node_modules for browserify ---"
    ln -s ${node-deps}/node_modules ./node_modules

    echo "--- Bundling generated JS with browserify ---"

    for GENERATED_GRPC_FILE in `find "$GEN_JS_PATH" -type f -name "*.js"`
    do
      browserify --standalone grpc "$GENERATED_GRPC_FILE" > "$BUNDLE_PATH/$(basename $GENERATED_GRPC_FILE)"
    done

    echo "--- Bundling complete. Final files are in $BUNDLE_PATH ---"
    ls "$BUNDLE_PATH"

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp ./bundled-js/*_grpc_web_pb.js "$out/"
    runHook postInstall
  '';

  dontConfigure = true;
}

