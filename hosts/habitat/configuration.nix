{ config, pkgs, nixpkgs, nixpkgs-unstable, ... }: {

  imports = [ ./hardware-configuration.nix ../../lib/cachix.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # append entries for other OSs detected by os-prober.
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.configurationLimit = 10;

  networking.hostName = "habitat";

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  networking.useDHCP = false;
  networking.interfaces.enp5s0.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  hardware.opengl.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  services.displayManager.defaultSession = "xfce+i3";

  services.xserver = {
    enable = true;

    xkb = {
      layout = "us";
      options = "caps:escape";
    };

    videoDrivers = [ "nvidia" ];

    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [ rofi dmenu i3status i3lock ];
    };
  };

  # compositor
  services.picom.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.sane-airscan ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.keegan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "sound" "plex" "scanner" "lp" ];
  };

  nixpkgs.overlays = [
    (import ../../lib/overlay.nix)
  ];

  # google-chrome is unfree.
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = (with pkgs; [
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bashInteractive
    btrfs-progs
    caffeine-ng
    comma
    counsel-repo
    curl
    difftastic
    dig
    direnv
    discord
    dtach
    emacs29
    fd
    ffmpeg
    file
    fzf
    gcc
    ghostscript
    git
    git-up
    gnome.simple-scan
    go
    golangci-lint
    gopls
    google-chrome
    graphviz
    gv
    htop
    imagemagick
    jq
    kbfs # provides git-remote-keybase
    kdenlive
    kitty
    ledger
    lsof
    maim
    man-pages
    man-pages-posix
    mosh
    mpv
    mupdf
    gh
    graphite-cli
    nix-direnv
    nix-index
    nixpkgs-fmt
    nmap
    nodejs_20
    nodePackages.typescript
    nodePackages.typescript-language-server
    obs-studio
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    python3
    qutebrowser
    ripgrep
    rofi-pass
    screen
    signal-desktop
    simplescreenrecorder
    spotify
    sqlite
    sqlitebrowser
    starship
    steam-run
    synergy
    tmux
    unzip
    watchman
    wget
    xautolock
    xclip
    xss-lock
    zoom-us
    zstd
  ]) ++ (with (import nixpkgs-unstable { system = "x86_64-linux"; config = { allowUnfree = true; }; }); [
    vscode
    yt-dlp
  ]);

  fonts.packages = with pkgs; [ hack-font iosevka jetbrains-mono ];

  # Needed for nix-direnv. Prevents GC. Also try out nix flakes
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';
  nix.package = pkgs.nixFlakes;
  environment.pathsToLink = [ "/share/nix-direnv" ];

  # set <nixpkgs> on NIX_PATH for users to flake input rather than using
  # channels.
  environment.etc."nix/inputs/nixpkgs".source = nixpkgs.outPath;
  nix.nixPath = [ "/etc/nix/inputs" ];

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

  programs.nix-ld.enable = true;

  services.emacs.package = pkgs.emacs29;

  services.gnome.gnome-keyring.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;
  services.openssh.settings.Macs = [
    # Defaults
    "hmac-sha2-512-etm@openssh.com"
    "hmac-sha2-256-etm@openssh.com"
    "umac-128-etm@openssh.com"

    # pass mobile app requires this
    "hmac-sha2-512"
  ];

  services.tailscale.enable = true;

  services.unbound.enable = true;
  services.unbound.settings.remote-control.control-enable = true;

  services.netatalk = {
    enable = true;
    settings = {
      timemachine_fa = {
        path = "/timemachine/fa";
        "valid users" = "keegan";
        "time machine" = true;
      };
    };
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      addresses = true;
      domain = true;
      enable = true;
      userServices = true;
    };
  };

  services.keybase.enable = true;

  services.plex = {
    enable = true;
    openFirewall = true;
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

  # Run TRIM for my SSD
  services.fstrim.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Default is performance. This seems like a more reasonable default, even
  # though this is a desktop machine.
  powerManagement.cpuFreqGovernor = "schedutil";

  nix.settings.allowed-users = [ "@wheel" ];

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
      allowedTCPPorts = [
        # netatalk
        548
      ];

      allowedUDPPortRanges = [
        # mosh
        {
          from = 60000;
          to = 60010;
        }
      ];
    };

  };

  networking.hosts = {
    "127.0.0.1" = [ "sourcegraph" "sourcegraph.test" "bitbucket" ];
    "100.100.74.50" = [ "cliche" ];
    "100.116.165.93" = [ "real" ];
    "100.96.124.130" = [ "fa" ];
  };

  networking.firewall.checkReversePath = "loose";

  # Testing default for 23.05
  services.nscd.enableNsncd = true;

  # Plex has a bug which means it never cleanly shutsdown. Use a more
  # aggressive timeout instead of the default 1m30s.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
