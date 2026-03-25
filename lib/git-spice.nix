{ lib, buildGo125Module, fetchFromGitHub }:

buildGo125Module rec {
  pname = "git-spice";
  version = "0.24.2";

  src = fetchFromGitHub {
    owner = "abhinav";
    repo = "git-spice";
    rev = "v${version}";
    sha256 = "sha256-Zt4PG3pWJ0h22fBJnsIVqcSk2BwwuOHdmSOrAMENN70=";
  };

  vendorHash = "sha256-tlAex6SFTprJtpMexMjAUNanamqraHYJuwtABx52rWQ=";

  ldflags = [
    "-s"
    "-w"
    "-X main._version=${version}"
  ];

  # avoid building ./doc module.
  subPackages = [ "." ];

  doCheck = false;

  # gs conflicts with ghostscript
  postInstall = ''
    mv $out/bin/gs $out/bin/git-spice
  '';
}
