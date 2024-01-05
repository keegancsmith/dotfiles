final: prev: rec {
  counsel-repo = prev.callPackage ./counsel-repo.nix { };

  pbv = prev.callPackage ./pbv.nix { };

  myNodePackages = import ./node-packages/default.nix {
    pkgs = prev;
  };

  # override snippet from overrides.nix in nixpkgs
  graphite-cli = myNodePackages."@withgraphite/graphite-cli".override {
    name = "graphite-cli";
    nativeBuildInputs = with prev; [ installShellFiles pkg-config ];
    buildInputs = with prev; [ cairo pango pixman ];
    # 'gt completion' auto-detects zshell from environment variables:
    # https://github.com/yargs/yargs/blob/2b6ba3139396b2e623aed404293f467f16590039/lib/completion.ts#L45
    postInstall = ''
      installShellCompletion --cmd gt \
        --bash <($out/bin/gt completion) \
        --zsh <(ZSH_NAME=zsh $out/bin/gt completion)
    '';
  };

  # Emacs 29.1 has issues saving with latest version of gnupg
  # https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
  #
  # I do not override gnupg since that causes me to rebuild the world.
  gnupg240 = prev.gnupg.overrideAttrs
    (orig: {
      version = "2.4.0";
      src = prev.fetchurl {
        url = "mirror://gnupg/gnupg/gnupg-2.4.0.tar.bz2";
        hash = "sha256-HXkVjdAdmSQx3S4/rLif2slxJ/iXhOosthDGAPsMFIM=";
      };
    });
}
