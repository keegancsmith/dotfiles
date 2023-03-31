{ config, pkgs, emacs-overlay, ... }:

{
  imports = [ ../../lib/cachix.nix ];

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
    emacsGit
    fd
    file
    fzf
    git
    go
    gopls
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

  # build newer emacs.
  nixpkgs.overlays = [ emacs-overlay.overlay ];
  services.emacs.package = pkgs.emacsGit;

  nixpkgs.config.allowUnfree = true;

  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

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
