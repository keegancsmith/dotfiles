{ config, pkgs, nixpkgs, nixpkgs-unstable, lib, ... }: {

  imports = [ ./hardware-configuration.nix ./disk-config.nix ../../lib/cachix.nix ];

  boot.loader.efi.canTouchEfiVariables = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 10;
    memtest86.enable = true;
  };

  # Already using systemd, might as well use it even more.
  boot.initrd.systemd.enable = true;

  # Memtest86 reported 00099c390160 to 00099d3992b0 as the lowest and highest
  # error address. Reserve that memory to prevent using it.
  boot.kernelParams = [ "memmap=16M$0x99c390000" ];

  networking.hostName = "habitat";

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  networking.useDHCP = false;
  networking.interfaces.enp6s0.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  hardware.opengl.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  services.displayManager.defaultSession = "xfce+i3";
  services.displayManager.autoLogin.user = "keegan";

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
    (writeShellScriptBin "switch" (
      if builtins.elem "work" config.system.nixos.tags
      then "sudo /nix/var/nix/profiles/system/specialisation/personal/bin/switch-to-configuration switch"
      else "sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch"
    ))
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    bashInteractive
    btrfs-progs
    cachix
    caffeine-ng
    comma
    counsel-repo
    curl
    difftastic
    dig
    direnv
    discord
    docker
    docker-credential-helpers
    dtach
    emacs29
    fd
    ffmpeg-full
    file
    fzf
    gcc
    ghostscript
    git
    git-up
    git-spice
    gnome.simple-scan
    golangci-lint
    gopls
    graphviz
    gv
    htop
    hyperfine
    imagemagick
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
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
    gnumake
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
    zstd
  ]) ++ (with (import nixpkgs-unstable { system = "x86_64-linux"; config = { allowUnfree = true; }; }); [
    go_1_23
    google-chrome
    spotify
    vscode
    yt-dlp
    zoom-us
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

  # still using the plex user, need to migrate data first
  users.users.plex = {
    group = "plex";
    uid = 193;
  };
  users.groups.plex = {
    gid = 193;
  };

  services.jellyfin = {
    enable = true;
    openFirewall = true;
    user = "plex";
    group = "plex";
    dataDir = "/var/lib/plex/jellyfin";
    cacheDir = "/var/lib/plex/jellyfin-cache";
  };

  services.kolide-launcher.enable = true;

  #  Track which specialisation I have activated, shown in i3status
  system.nixos.tags = [ "work" ];
  environment.etc."nix/profile-name".text = builtins.concatStringsSep "-" config.system.nixos.tags;
  specialisation.personal.configuration = {
    system.nixos.tags = lib.mkForce [ "personal" ];
    services.kolide-launcher.enable = lib.mkForce false;

    security.sudo.wheelNeedsPassword = false;

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
  };

  # Run TRIM for my SSD
  services.fstrim.enable = true;

  services.dbus.implementation = "broker";

  security.sudo = {
    enable = true;
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

    interfaces.enp6s0 = {
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
    "127.0.0.1" = [
      "bitbucket"
      "sourcegraph"
      "sourcegraph.test"
    ] ++ (map (subdomain: "${subdomain}.sourcegraph.test") [
      "default"
      "tenant1"
      "tenant2"
    ]);
    "100.100.74.50" = [ "cliche" ];
    "100.116.165.93" = [ "real" ];
    "100.96.124.130" = [ "fa" ];
  };

  networking.firewall.checkReversePath = "loose";

  # Testing default for 23.05
  services.nscd.enableNsncd = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
