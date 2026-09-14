# Covers every tool the CLI examples and the Rust, TypeScript and Go language examples use.
# The Haskell example needs the repository's own dev shell instead (`.#rpc-quickstart-haskell`).
{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell { packages = with pkgs; [ buf grpcurl cargo rustc gcc nodejs go ]; }
