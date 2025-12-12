final: prev: rec {
  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  git-spice = prev.callPackage ./git-spice.nix { };

  starship-jj = prev.callPackage ./starship-jj.nix { };

  my-bazelisk = prev.callPackage ./bazelisk.nix { };

  my-scripts = prev.callPackage ./my-scripts.nix { };

  myNodePackages = import ./node-packages/default.nix {
    pkgs = prev;
    nodejs = prev.nodejs_22;
  };

  myEmacs = (prev.emacsPackagesFor prev.emacs30).emacsWithPackages (
    epkgs: [ epkgs.vterm ]
  );
}
