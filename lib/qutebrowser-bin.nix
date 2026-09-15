{ lib, stdenvNoCC, fetchurl, undmg, makeWrapper }:

stdenvNoCC.mkDerivation rec {
  pname = "qutebrowser-bin";
  version = "3.7.0";

  src = fetchurl {
    url = "https://github.com/qutebrowser/qutebrowser/releases/download/v${version}/qutebrowser-${version}-arm64.dmg";
    hash = "sha256-mBcCQb8Sov4d6r/8gSC3DcjgDlVbtxLDh1HESPBJVKM=";
  };

  nativeBuildInputs = [ undmg makeWrapper ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall

    app="$(echo *.app)"
    mkdir -p "$out/Applications" "$out/bin"
    cp -R "$app" "$out/Applications/"
    makeWrapper "$out/Applications/$app/Contents/MacOS/qutebrowser" "$out/bin/qutebrowser"

    runHook postInstall
  '';

  meta = {
    description = "Keyboard-driven, vim-like browser based on Python and Qt";
    homepage = "https://www.qutebrowser.org/";
    license = lib.licenses.gpl3Plus;
    mainProgram = "qutebrowser";
    platforms = [ "aarch64-darwin" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
