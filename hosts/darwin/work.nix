{ pkgs, nixpkgs, nixpkgs-unstable, ... }:

{
  homebrew.enable = true;
  homebrew.brews = [
    "ibazel"
    "bazelisk"
  ];
  homebrew.casks = [
    "dbeaver-community"
    "mitmproxy"
    "superwhisper"
  ];
  homebrew.masApps = {
    "okta verify" = 490179405;
  };
}
