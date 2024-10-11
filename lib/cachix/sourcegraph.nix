# This doesn't configure us to use the Sourcegraph cache, but rather makes it
# possible to trust it for the sourcegraph repo's flake.nix
{
  nix.settings = {
    trusted-substituters = [ "https://sourcegraph-noah.cachix.org" "https://sourcegraph-keegan.cachix.org" ];
  };
}
