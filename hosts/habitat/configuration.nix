{ config, pkgs, ... }:

let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  imports = [ ./hardware-configuration.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # append entries for other OSs detected by os-prober.
  boot.loader.grub.useOSProber = true;

  networking.hostName = "habitat";

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  networking.useDHCP = false;
  networking.interfaces.enp5s0.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  hardware.opengl.enable = true;

  services.xserver = {
    enable = true;

    layout = "us";

    xkbOptions = "caps:escape";

    videoDrivers = [ "nvidia" ];

    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };

    displayManager = { defaultSession = "xfce+i3"; };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [ rofi dmenu i3status i3lock ];
    };
  };

  # compositor
  services.picom.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.keegan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "sound" "plex" ];
  };

  # build newer emacs with native-comp and pgtk.
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/ba0e667840fabccfac038e2150c302f0d1e98fe1.tar.gz";
      sha256 = "0y6lf09rslm7nwgpxrfszfvh1rqxsx5f8p2hv28przmal553yjv6";
    }))
  ];

  # google-chrome is unfree.
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    graphviz
    curl
    wget
    git
    git-up
    unstable.git-branchless
    unstable.go
    unstable.golangci-lint
    python3
    direnv
    nix-direnv
    unstable.gopls
    htop
    emacsPgtkGcc
    notmuch
    gmailieer
    aspell
    ripgrep
    bash
    unstable.starship
    qutebrowser
    google-chrome
    spotify
    mpv
    ledger
    jq
    fzf
    fd
    nmap
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    rofi-pass
    screen
    tmux
    watchman
    youtube-dl
    python39Packages.yt-dlp
    k9s
    gcc
    mosh
    maim
    xclip
    alacritty
    kitty
    kubectl
    kubectx
    google-cloud-sdk
    cloud-sql-proxy
    lsof
    nixfmt
    mupdf
    ghostscript
    gv
    unstable.zoom-us
    kbfs # provides git-remote-keybase
    man-pages
    man-pages-posix
    signal-desktop
    unstable.obs-studio
    btrfs-progs
    unstable.minecraft
    xautolock
    xss-lock
    unstable.difftastic
    synergy
  ];

  fonts.fonts = with pkgs; [ hack-font go-font ];

  # Needed for nix-direnv. Prevents GC. Also try out nix flakes
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';
  nix.package = unstable.nixFlakes;
  environment.pathsToLink = [ "/share/nix-direnv" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.neovim.enable = true;
  programs.neovim.viAlias = true;
  programs.neovim.vimAlias = true;

  documentation.dev.enable = true;

  services.emacs.package = pkgs.emacsPgtkGcc;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.forwardX11 = true;

  services.tailscale.enable = true;

  services.unbound.enable = true;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      addresses = true;
      domain = true;
      enable = true;
    };
  };

  services.keybase.enable = true;

  services.plex = {
    enable = true;
    openFirewall = true;
    package = unstable.plex;
  };

  services.sonarr = {
    enable = true;
    openFirewall = true;
    user = "plex";
    group = "plex";
    dataDir = "/var/lib/plex/.config/NzbDrone";
  };

  services.transmission = {
    enable = true;
    openFirewall = true;
    openRPCPort = true;
    user = "plex";
    group = "plex";
    home = "/var/lib/plex/transmission";
    settings = {
      rpc-bind-address = "0.0.0.0";
      rpc-whitelist = "127.0.0.1,100.*.*.*";
      rpc-host-whitelist = "habitat,habitat.local";
    };
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Default is performance. This seems like a more reasonable default, even
  # though this is a desktop machine.
  powerManagement.cpuFreqGovernor = "schedutil";

  nix.allowedUsers = [ "@wheel" ];

  virtualisation.docker.enable = true;

  networking.firewall = {

    # Limit most stuff to tailscale network
    interfaces.tailscale0 = {
      allowedTCPPorts = [
        # rclone webdav port for org-files <-> beorg
        8780

        # synergy
        24800
      ];

      allowedUDPPortRanges = [
        # mosh
        {
          from = 60000;
          to = 60010;
        }
      ];
    };

    interfaces.enp5s0 = {
      allowedUDPPortRanges = [
        # mosh
        {
          from = 60000;
          to = 60010;
        }
      ];
    };

  };

  networking.hosts = { "127.0.0.1" = [ "sourcegraph.test" ]; };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
