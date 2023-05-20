{ config, pkgs, nixpkgs, lib, ... }:

{
  imports = [ ../../lib/cachix.nix ];

  environment.systemPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bashInteractive
    cachix
    comma
    coreutils
    curl
    direnv
    dtach
    emacs
    fd
    ffmpeg
    file
    fzf
    git
    ghostscript
    gnupg
    go
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud_sql_proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
    gopls
    graphviz
    htop
    jq
    kitty
    ledger
    lieer
    man-pages
    man-pages-posix
    mcfly
    mpv
    neovim
    nix-direnv
    nix-index
    nixpkgs-fmt
    notmuch
    pandoc
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    pinentry
    python39Packages.yt-dlp
    ripgrep
    screen
    starship
    tmux
    unzip
    watchman
    wget
  ] ++ lib.optional (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") [ (pkgs.callPackage ../../lib/pbv.nix { }) ];

  fonts.fonts = with pkgs; [ hack-font go-font iosevka ];
  fonts.fontDir.enable = true;

  homebrew.enable = true;
  homebrew.casks = [
    "adobe-acrobat-reader"
    "discord"
    "google-chrome"
    "hammerspoon"
    "insomnia"
    "karabiner-elements"
    "keybase"
    "raycast"
    "spotify"
    "steam"
    "visual-studio-code"
    "wkhtmltopdf"
  ];
  homebrew.masApps = {
    "1password" = 1333542190;
    "okta verify" = 490179405;
    "slack" = 803453959;
    "tailscale" = 1475387142;
    "vimari" = 1480933944;
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  system.defaults.NSGlobalDomain = {
    AppleKeyboardUIMode = 3;
    ApplePressAndHoldEnabled = false;
    InitialKeyRepeat = 10;
    KeyRepeat = 1;
    NSAutomaticCapitalizationEnabled = false;
    NSAutomaticDashSubstitutionEnabled = false;
    NSAutomaticPeriodSubstitutionEnabled = false;
    NSAutomaticQuoteSubstitutionEnabled = false;
    NSAutomaticSpellingCorrectionEnabled = false;
    NSNavPanelExpandedStateForSaveMode = true;
    NSNavPanelExpandedStateForSaveMode2 = true;
    _HIHideMenuBar = true;
  };

  environment.shells = [ pkgs.bashInteractive ];

  # For some reason symlinking it into the default location doesn't work.
  environment.darwinConfig =
    "/Users/keegan/src/github.com/keegancsmith/dotfiles/flake.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # For nix-direnv
  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
    experimental-features = "nix-command flakes";
  };
  environment.pathsToLink = [ "/share/nix-direnv" ];

  # Pin flake system version of nixpkgs to the input version.
  nix.registry.nixpkgs.flake = nixpkgs;

  # Pin nix-channel for nixpkgs to flake input.
  nix.nixPath = [{
    nixpkgs = nixpkgs.outPath;

    # Default value in nix-darwin
    darwin-config = "${config.environment.darwinConfig}";
  }];

  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.vim.package = pkgs.neovim;

  documentation.enable = true;
  programs.info.enable = true;
  programs.man.enable = true;

  services.tailscale.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
