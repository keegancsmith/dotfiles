final: prev: rec {
  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  pbv = prev.callPackage ./pbv.nix { };
}
