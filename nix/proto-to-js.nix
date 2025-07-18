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

    mkdir -p $GEN_JS_PATH
    mkdir -p $BUNDLE_PATH

    PROTO_FILE=$PROTO_INCLUDE_PATH/cardano/rpc/node.proto

    echo "--- Compiling .proto file: $PROTO_FILE ---"

    protoc \
      -I=$PROTO_INCLUDE_PATH \
      --js_out=import_style=commonjs,binary:$GEN_JS_PATH \
      --grpc-web_out=import_style=commonjs,mode=grpcwebtext:$GEN_JS_PATH \
      $PROTO_FILE

    echo "--- Compilation finished. Generated files are in $GEN_JS_PATH ---"
    ls -R $GEN_JS_PATH

    GENERATED_GRPC_FILE=$GEN_JS_PATH/cardano/rpc/node_grpc_web_pb.js

    if [ ! -f "$GENERATED_GRPC_FILE" ]; then
        echo "Error: Protoc did not generate the expected gRPC-Web file!"
        exit 1
    fi

    echo "--- Setting up node_modules for browserify ---"
    ln -s ${node-deps}/node_modules ./node_modules

    echo "--- Bundling generated JS with browserify ---"

    browserify --standalone grpc $GENERATED_GRPC_FILE > $BUNDLE_PATH/node_grpc_web_pb.js

    echo "--- Bundling complete. Final file is in $BUNDLE_PATH ---"
    ls $BUNDLE_PATH

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp ./bundled-js/node_grpc_web_pb.js $out/
    runHook postInstall
  '';

  dontConfigure = true;
}

