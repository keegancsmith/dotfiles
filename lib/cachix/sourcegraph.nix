# This doesn't configure us to use the Sourcegraph cache, but rather makes it
# possible to trust it for the sourcegraph repo's flake.nix
{
  nix.settings = {
    trusted-substituters = [ "https://sourcegraph-noah.cachix.org" "https://sourcegraph-keegan.cachix.org" ];
    trusted-public-keys = [ "sourcegraph-noah.cachix.org-1:rTTKnyuUmJuGt/UAXUpdOCOXDAfaO1AYy+/jSre3XgA=" "sourcegraph-keegan.cachix.org-1:cBc8JVINx349y/Ztd4ae2hp9Vb/tZRM5ZMjdjWCStlk=" ];
  };
}
