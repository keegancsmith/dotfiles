final: prev: rec {
  # direnv's GNUmakefile adds -linkmode=external on Darwin which requires cgo
  direnv = prev.direnv.overrideAttrs (old: {
    env = (old.env or { }) // { CGO_ENABLED = "1"; };
    doCheck = !prev.stdenv.isDarwin;
  });

  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  git-spice = prev.callPackage ./git-spice.nix { };

  my-bazelisk = prev.callPackage ./bazelisk.nix { };

  my-scripts = prev.callPackage ./my-scripts.nix { };

  myEmacs = (prev.emacsPackagesFor prev.emacs30).emacsWithPackages (
    epkgs: [ epkgs.vterm epkgs.treesit-grammars.with-all-grammars ]
  );

  # Backport nixpkgs 5f9ab4dd. The 1.98.9 bump retained the 1.98.5 vendor hash.
  tailscale =
    if prev.tailscale.version == "1.98.9" then
      prev.tailscale.overrideAttrs
        {
          vendorHash = "sha256-Sd2iLJ7eDfDYdIRuW4xuiKgzhQWJWGAnz97FJWrVRlE=";
        }
    else
      prev.tailscale;
}
