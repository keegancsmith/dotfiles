{ buildFHSEnv }:

# Give up on nixifying bazel, and just use bazelisk + FHSEnv with a bunch of
# usual utilities.
buildFHSEnv {
  name = "bazel";
  runScript = "bazelisk";
  targetPkgs = pkgs: (with pkgs; [
    bazelisk

    bashInteractive
    coreutils
    curl
    diffutils
    docker-credential-helpers
    file
    findutils
    gawk
    git
    gnugrep
    gnused
    gnutar
    gzip
    hexdump
    openssh
    patch
    perl
    python3
    stdenv.cc
    unzip
    which
    zip
    zlib
    zlib.dev
  ]);
  # unsharePid required to preserve bazel server between bazel invocations,
  # the rest are disabled just in case
  unsharePid = false;
  unshareUser = false;
  unshareIpc = false;
  unshareNet = false;
  unshareUts = false;
  unshareCgroup = false;
}
