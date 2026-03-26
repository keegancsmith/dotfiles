final: prev: rec {
  # direnv's GNUmakefile adds -linkmode=external on Darwin which requires cgo
  direnv = prev.direnv.overrideAttrs (old: {
    env = (old.env or { }) // { CGO_ENABLED = "1"; };
  });

  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  git-spice = prev.callPackage ./git-spice.nix { };

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
