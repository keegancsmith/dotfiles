{ pkgs, nixpkgs-unstable, ... }:

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
    nix-direnv
    nix-index
    nixpkgs-fmt
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    (python3.withPackages (ps: with ps; [ numpy pandas requests uv ]))
    ripgrep
    screen
    shellcheck
    shfmt
    sqlite
    sqlitebrowser
    starship
    starship-jj
    tmux
    unzip
    uv
    watchman
    wget
    zstd
  ]) ++ (with nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}; [
    go_1_25
    golangci-lint
    gopls
    jjui
    jujutsu
    yt-dlp
  ]);
}
