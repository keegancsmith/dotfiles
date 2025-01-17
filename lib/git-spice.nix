{ lib, buildGo123Module, fetchFromGitHub }:

buildGo123Module rec {
  pname = "git-spice";
  version = "0.9.0";

  src = fetchFromGitHub {
    owner = "abhinav";
    repo = "git-spice";
    rev = "v${version}";
    sha256 = "sha256-Q5cNkX6ZtNXh+qDjpR0a2FfHmk5YA9izLCBRPFRpdvs=";
  };

  vendorHash = "sha256-4NkeLDToefiRYv9xta3U6O/5L2/J0d+59Er515R2zcw=";

  # avoid building ./doc module.
  subPackages = [ "." ];

  doCheck = false;

  # gs conflicts with ghostscript
  postInstall = ''
    mv $out/bin/gs $out/bin/git-spice
  '';
}
