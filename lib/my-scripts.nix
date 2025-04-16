{ lib, stdenv }:

stdenv.mkDerivation {
  pname = "my-scripts";
  version = "1.0.0";
  src = ../bin;

  installPhase = ''
    mkdir -p $out/bin
    cp * $out/bin/
    chmod +x $out/bin/*
  '';

  meta = with lib; {
    description = "Personal scripts";
    platforms = platforms.all;
  };
}
