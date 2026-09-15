# Covers every tool the CLI examples and the Rust, TypeScript, Go and Python language examples use.
# The Haskell example needs the repository's own dev shell instead (`.#rpc-quickstart-haskell`).
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [ buf grpcurl cargo rustc gcc nodejs go python3 ];
  # grpcio's manylinux wheel (installed by the Python example's pip install)
  # links against the host libstdc++, which a plain nixpkgs shell does not
  # otherwise put on the loader path.
  LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
}
