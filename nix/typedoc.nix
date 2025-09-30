# typedoc.nix
{pkgs ? import <nixpkgs> {}}: let
  # Node dependencies
  typedoc-deps = pkgs.buildNpmPackage {
    version = "1.0.0";
    name = "typedoc-deps";
    src = ../nix/typedoc-npm-deps;
    npmDepsHash = "sha256-SiC4T1IEwF/We4rHd+k+kraZmPl72bHmGzLd9Vc8SDk=";
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
      pkgs.nodejs_22
      typedoc-deps
    ];

    buildPhase = ''
      runHook preBuild

      cp -r ${typedoc-deps} ./node_modules
      TS_FILES_PATH=$src

      OUT_DOCS_PATH=./generated-docs

      echo "--- Generating TypeDoc docs ---"
      node_modules/.bin/typedoc --excludeExternals --plugin typedoc-plugin-mdn-links --plugin typedoc-plugin-missing-exports --plugin typedoc-plugin-rename-defaults --name cardano-wasm --disableGit --sourceLinkTemplate 'https://github.com/IntersectMBO/cardano-api/blob/master/cardano-wasm/lib-wrapper/{path}#L{line}' --basePath "$TS_FILES_PATH" --out "$OUT_DOCS_PATH" "$TS_FILES_PATH/*.d.ts"

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
