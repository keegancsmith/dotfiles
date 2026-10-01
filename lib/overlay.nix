final: prev: rec {
  # direnv's GNUmakefile adds -linkmode=external on Darwin which requires cgo
  direnv = prev.direnv.overrideAttrs (old: {
    env = (old.env or { }) // { CGO_ENABLED = "1"; };
    doCheck = !prev.stdenv.hostPlatform.isDarwin;
  });

  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  git-spice = prev.callPackage ./git-spice.nix { };

  my-bazelisk = prev.callPackage ./bazelisk.nix { };

  my-scripts = prev.callPackage ./my-scripts.nix { };

  qutebrowser-bin = prev.callPackage ./qutebrowser-bin.nix { };

  # Contains my fix for "rescan directories of messages modified since last scan"
  muchsync = prev.muchsync.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "keegancsmith";
      repo = "muchsync";
      rev = "137b983f0b65375d4f011a4d83aab41ba43481d1";
      hash = "sha256-xvh8vsss/JvgAvU88vHx55cex799sRBaSAI1VofZ6K0=";
    };

    # The GitHub checkout lacks the generated files included in release tarballs.
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
      prev.autoreconfHook
      prev.pandoc
    ];
  });

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
