{ lib, buildGo124Module, fetchFromGitHub }:

buildGo124Module rec {
  pname = "git-spice";
  version = "0.15.1";

  src = fetchFromGitHub {
    owner = "abhinav";
    repo = "git-spice";
    rev = "v${version}";
    sha256 = "sha256-mx34JGgY6qKhPdZVs1Z9gVO/VhHnFrl6TThq5dEz/zc=";
  };

  vendorHash = "sha256-uh4GUkfWo12pYQD/Mpw+EWwmukHUpxOii7DTu6C84zo=";

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
