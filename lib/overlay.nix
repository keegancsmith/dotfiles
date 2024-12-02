final: prev: rec {
  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  git-spice = prev.callPackage ./git-spice.nix { };

  myNodePackages = import ./node-packages/default.nix {
    pkgs = prev;
  };
}
