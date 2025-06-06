{ stdenvNoCC
, fetchFromGitHub
, autoreconfHook
, wasi-sdk
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  name = "blst";

  nativeBuildInputs = [
    wasi-sdk
  ];

  buildPhase = ''
    runHook preBuild

    ./build.sh

    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall

    mkdir -p $out/{lib,include}
    for lib in libblst.{a,so,dylib}; do
      if [ -f $lib ]; then
        cp $lib $out/lib/
      fi
    done
    cp bindings/{blst.h,blst_aux.h} $out/include

    for lib in blst.dll; do
      if [ -f $lib ]; then
        mkdir -p $out/bin
        cp $lib $out/bin/
      fi
    done

    mkdir -p $out/lib/pkgconfig
    cat <<EOF > $out/lib/pkgconfig/libblst.pc
    prefix=$out
    exec_prefix=''\\''${prefix}
    libdir=''\\''${exec_prefix}/lib
    includedir=''\\''${prefix}/include

    Name: libblst
    Description: bogus
    URL: bogus
    Version: 0.0

    Cflags: -I''\\''${includedir}
    Libs: -L''\\''${libdir} -lblst
    Libs.private:
    EOF

    wasm32-wasi-clang -shared -Wl,--whole-archive $out/lib/libblst.a -o $out/lib/libblst.so

    runHook postInstall
  '';
})
