{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "counsel-repo";
  version = "bd34a0f2de1fa88b0640b6a511bcd0702e6f3105";

  src = fetchFromGitHub {
    owner = "keegancsmith";
    repo = "counsel-repo";
    rev = version;
    sha256 = "sha256-67S9uiBOXkg/B7zXMTsK2FeLKOMeW0K9Z/IuTsHD8h0=";
  };

  vendorHash = null;
}
