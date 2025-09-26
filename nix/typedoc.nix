# typedoc.nix

{ pkgs ? import <nixpkgs> {} }:

let
  # Node dependencies
  typedoc-deps = pkgs.buildNpmPackage {
    version = "1.0.0";
    name = "typedoc-deps";
    src = ../nix/typedoc-npm-deps;
    npmDepsHash = "sha256-JirDw50WURgzAeB+NysA05J5bEA8w/03jPaR1hu7VJw=";
    installPhase = ''
      mv node_modules $out
    '';
  };
in
pkgs.stdenv.mkDerivation {
  pname = "cardano-wasm-typedoc";
  version = "0.1.0";

  src = ../cardano-wasm/lib-wrapper;
  nativeBuildInputs = [
    pkgs.nodejs_24
    typedoc-deps
  ];

  buildPhase = ''
    runHook preBuild

    cp -r ${typedoc-deps} ./node_modules
    TS_FILES_PATH=$src
    
    OUT_DOCS_PATH=./generated-docs

    echo "--- Generating TypeDoc docs ---"
    node_modules/.bin/typedoc --plugin typedoc-plugin-missing-exports --plugin typedoc-plugin-rename-defaults --name cardano-wasm --disableGit --sourceLinkTemplate 'https://github.com/IntersectMBO/cardano-api/blob/master/cardano-wasm/lib-wrapper/{path}#L{line}' --basePath "$TS_FILES_PATH" --out "$OUT_DOCS_PATH" "$TS_FILES_PATH/*.d.ts" 
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out

    cp -r ./generated-docs/* $out/

    runHook postInstall
  '';

  dontConfigure = true;
}

