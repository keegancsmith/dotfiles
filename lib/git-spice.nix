{ lib, buildGo123Module, fetchFromGitHub }:

buildGo123Module rec {
  pname = "git-spice";
  version = "0.10.0";

  src = fetchFromGitHub {
    owner = "abhinav";
    repo = "git-spice";
    rev = "v${version}";
    sha256 = "sha256-1EWuKjvDeOV6W+nntdevUI/SO68ssYgoxJ5QIy5jkFM=";
  };

  vendorHash = "sha256-F9CyhUtdkwvEsmQ+T5zt2n+TBRhVgyr2CEOvIzcXpug=";

  # avoid building ./doc module.
  subPackages = [ "." ];

  doCheck = false;

  # gs conflicts with ghostscript
  postInstall = ''
    mv $out/bin/gs $out/bin/git-spice
  '';
}
