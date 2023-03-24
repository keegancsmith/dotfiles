{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    bashInteractive
    cachix
    comma
    coreutils
    curl
    direnv
    dtach
    emacs
    fd
    file
    fzf
    git
    go
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
    nixfmt
    notmuch
    python39Packages.yt-dlp
    ripgrep
    screen
    slack
    starship
    tmux
    unzip
    vscode
    watchman
    wget
  ];

  fonts.fonts = with pkgs; [ hack-font go-font iosevka ];
  fonts.fontDir.enable = true;

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  # For some reason symlinking it into the default location doesn't work.
  environment.darwinConfig = "/Users/keegan/src/github.com/keegancsmith/dotfiles/hosts/fa/darwin-configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  nixpkgs.config.allowUnfree = true;

  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

  programs.vim.package = pkgs.neovim;

  services.tailscale.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
