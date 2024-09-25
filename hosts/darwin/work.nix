{ pkgs, nixpkgs, nixpkgs-unstable, ... }:

{
  homebrew.enable = true;
  homebrew.brews = [
    "ibazel"
    "bazelisk"
  ];
  homebrew.casks = [
    "dbeaver-community"
    "figma"
    "linear-linear"
    "mitmproxy"
    "notion"
  ];
  homebrew.masApps = {
    "1password" = 1333542190;
    "golinks" = 1478821913;
    "okta verify" = 490179405;
  };
}
