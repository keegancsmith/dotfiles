{ pkgs, unstablePkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers ]))
    bashInteractive
    btop
    cachix
    (writeShellScriptBin "claude" ''npx -y @anthropic-ai/claude-code "$@"'')
    comma
    counsel-repo
    curl
    difftastic
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
    git-up
    git-spice
    gnuplot
    graphviz
    htop
    hyperfine
    imagemagick
    jq
    kbfs # provides git-remote-keybase
    kitty
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
    go_1_26
    golangci-lint
    gopls
  ]);
}
