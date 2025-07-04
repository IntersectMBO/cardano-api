{
  stdenvNoCC,
  fetchFromGitHub,
  autoreconfHook,
  wasi-sdk,
}:
stdenvNoCC.mkDerivation {
  name = "libsodium";

  src = fetchFromGitHub {
    owner = "jedisct1";
    repo = "libsodium";
    rev = "9511c982fb1d046470a8b42aa36556cdb7da15de";
    hash = "sha256-ZPVzKJZRglZT2EJKqdBu94I4TRrF5sujSglUR64ApWA=";
  };

  nativeBuildInputs = [
    wasi-sdk
    autoreconfHook
  ];

  configureFlags = [
    "--host=wasm32-wasi"
  ];

  postInstall = ''
    wasm32-wasi-clang -shared -Wl,--whole-archive $out/lib/libsodium.a -o $out/lib/libsodium.so
  '';
}
