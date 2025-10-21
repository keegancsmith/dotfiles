{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "counsel-repo";
  version = "3e7960bfd9b11d82055fb4cfc647570135f62671";

  src = fetchFromGitHub {
    owner = "keegancsmith";
    repo = "counsel-repo";
    rev = version;
    sha256 = "sha256-T9WcSS2kts+G4sOPsco+Repf/0MJqUXebkYjwFAmWJc=";
  };

  vendorHash = null;
}
