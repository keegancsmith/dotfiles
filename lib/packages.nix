{ pkgs, unstablePkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers ]))
    bashInteractive
    btop
    cachix
    comma
    counsel-repo
    curl
    dig
    direnv
    dtach
    fastmod
    fd
    file
    fzf
    gh
    ghostscript
    git
    gnuplot
    graphviz
    htop
    hyperfine
    imagemagick
    jq
    kbfs # provides git-remote-keybase
    ledger
    man-pages
    man-pages-posix
    mise
    mosh
    muchsync
    my-scripts
    nh
    nix-direnv
    nix-index
    nix-init
    nixpkgs-fmt
    nurl
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    (python3.withPackages (ps: with ps; [ numpy pandas requests uv ]))
    ripgrep
    screen
    shellcheck
    shfmt
    sqlite
    sqlitebrowser
    starship
    tmux
    unzip
    uv
    watchman
    wget
    zstd
  ]) ++ (with unstablePkgs; [
    go_1_27
    golangci-lint
    gopls
  ]);
}
