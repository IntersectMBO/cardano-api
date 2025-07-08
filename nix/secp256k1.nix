{ stdenvNoCC
, fetchFromGitHub
, autoreconfHook
, pkg-config
, wasi-sdk
}:

stdenvNoCC.mkDerivation {
  name = "libsecp256k1";

  outputs = [
    "out"
    "dev"
  ];

  nativeBuildInputs = [
    wasi-sdk
    autoreconfHook
  ];

  configureFlags = [
    "--host=wasm32-wasi"
    "--enable-module-schnorrsig"
    "SECP_CFLAGS=-fPIC"
  ];

  postInstall = ''
    wasm32-wasi-clang -shared -Wl,--whole-archive $out/lib/libsecp256k1.a -o $out/lib/libsecp256k1.so
  '';
}
