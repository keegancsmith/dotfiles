{ config, pkgs, nixpkgs, nixpkgs-unstable, lib, ... }:

{
  imports = [ ../../lib/cachix.nix ];

  nixpkgs.overlays = [
    (import ../../lib/overlay.nix)
  ];

  environment.systemPackages = (with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bashInteractive
    cachix
    comma
    coreutils
    counsel-repo
    curl
    direnv
    dtach
    emacs29-macport
    fd
    ffmpeg
    file
    fzf
    gh
    git
    go
    golangci-lint
    gopls
    ghostscript
    gnupg
    graphite-cli
    graphviz
    htop
    hyperfine
    imagemagick
    jq
    kitty
    ledger
    man-pages
    man-pages-posix
    mosh
    neovim
    nix-direnv
    nix-index
    nixpkgs-fmt
    pandoc
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    python39Packages.yt-dlp
    ripgrep
    screen
    sqlite
    sqlitebrowser
    starship
    tmux
    unzip
    watchman
    wget
    zstd
  ] ++ lib.optional (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") mpv) ++ (with nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}; [
    lieer
    notmuch
  ]);

  fonts.packages = with pkgs; [ hack-font iosevka jetbrains-mono ];

  homebrew.enable = true;
  homebrew.brews = [
    "pinentry-mac"
  ];
  homebrew.casks = [
    "adobe-acrobat-reader"
    "discord"
    "google-chrome"
    "hammerspoon"
    "insomnia"
    "kap"
    "karabiner-elements"
    "keybase"
    "qutebrowser"
    "raycast"
    "scratch"
    "sloth"
    "spotify"
    "steam"
    "telegram"
    "visual-studio-code"
    "wkhtmltopdf"
    "zed"
  ];
  homebrew.masApps = {
    "print to pdf" = 1639234272;
    "slack" = 803453959;
    "tailscale" = 1475387142;
    "vimari" = 1480933944;
    "xcode" = 497799835;
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  system.defaults.NSGlobalDomain = {
    AppleKeyboardUIMode = 3;
    ApplePressAndHoldEnabled = false;
    InitialKeyRepeat = 25;
    KeyRepeat = 6;
    NSAutomaticCapitalizationEnabled = false;
    NSAutomaticDashSubstitutionEnabled = false;
    NSAutomaticPeriodSubstitutionEnabled = false;
    NSAutomaticQuoteSubstitutionEnabled = false;
    NSAutomaticSpellingCorrectionEnabled = false;
    NSNavPanelExpandedStateForSaveMode = true;
    NSNavPanelExpandedStateForSaveMode2 = true;
    # New macbooks have notch for camera so no point hiding menubar for them.
    _HIHideMenuBar = (pkgs.stdenv.hostPlatform.system == "x86_64-darwin");
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
  nix.registry.nixpkgs-unstable.flake = nixpkgs-unstable;

  # Pin nix-channel for nixpkgs to flake input.
  nix.nixPath = [{
    nixpkgs = nixpkgs.outPath;

    nixpkgs-unstable = nixpkgs-unstable.outPath;

    # Default value in nix-darwin
    darwin-config = "${config.environment.darwinConfig}";
  }];

  programs.bash.enable = true;
  programs.bash.completion.enable = true;

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
