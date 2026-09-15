{ ... }:

{
  homebrew.enable = true;
  homebrew.brews = [
    "ibazel"
    "bazelisk"
  ];
  homebrew.casks = [
    "mitmproxy"
  ];
  homebrew.masApps = {
    "okta verify" = 490179405;
  };
}
