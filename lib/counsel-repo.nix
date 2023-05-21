{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "counsel-repo";
  version = "c052b44ad53fb906a78437e003d55991bedee0ab";

  src = fetchFromGitHub {
    owner = "keegancsmith";
    repo = "counsel-repo";
    rev = version;
    sha256 = "sha256-pM7S3IPo09uHvueRtBgbWbnDBB3b9gnGiQkmi0mLAWo=";
  };

  vendorSha256 = null;
}
