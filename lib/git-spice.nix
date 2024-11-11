{ lib, buildGo123Module, fetchFromGitHub }:

buildGo123Module rec {
  pname = "git-spice";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "abhinav";
    repo = "git-spice";
    rev = "v${version}";
    sha256 = "sha256-UPGGhhxgxQvTOXmhpTZtpcE43QA52bJeA9uw57TWPrI=";
  };

  vendorHash = "sha256-JP/2v8BJrgruiW3MWEs01YwJYsKJa9h9RKkh07KLpMk=";

  # avoid building ./doc module.
  subPackages = ["."];

  doCheck = false;
}
