final: prev: rec {
  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  git-spice = prev.callPackage ./git-spice.nix { };

  my-bazelisk = prev.callPackage ./bazelisk.nix { };

  my-scripts = prev.callPackage ./my-scripts.nix { };

  myNodePackages = import ./node-packages/default.nix {
    pkgs = prev;
    nodejs = prev.nodejs_22;
  };
}
