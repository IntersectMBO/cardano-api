{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [ pkgs.python3 ];
  # grpcio's manylinux wheel links against the host libstdc++, which a plain
  # nixpkgs shell does not otherwise put on the loader path.
  LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
}
