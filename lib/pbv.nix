{ lib, stdenv, fetchFromGitHub, swift, darwin }:
stdenv.mkDerivation rec {
  name = "macos-pasteboard";

  src = fetchFromGitHub {
    owner = "chbrown";
    repo = name;
    rev = "6d58ddcff833397b15f4435e661fc31a1ec91321";
    sha256 = "6QpvIPy259d7BtA6s2NxS5JqiBPngPwgVgJl509btuY=";
  };

  nativeBuildInputs = [ swift darwin.apple_sdk.frameworks.Cocoa ];

  prePatch = ''
    substituteInPlace Makefile \
        --replace 'xcrun -sdk macosx swiftc' swiftc
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/pbv $out/bin/
  '';

  meta = with lib; {
    homepage = "https://github.com/chbrown/macos-pasteboard";
    description = ''
      Like OS X's built-in pbpaste but more flexible and raw
    '';
    platforms = platforms.darwin;
    license = "MIT";
  };
}
