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
    extraGroups = [ "wheel" "docker" "sound" ];
  };

  # build newer emacs with native-comp and pgtk. commit date: 2021-11-25
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/640ff172f47d3c6ae959494b888f19a75d41ffbd.tar.gz";
      sha256 = "1r9fnd9kc8vgkr9d4w29zmc534ks5a9dqh096h667l12n7vlyjvx";
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
    go
    python3
    direnv
    nix-direnv
    gopls
    htop
    emacsPgtkGcc
    unstable.notmuch # follow master in emacs
    gmailieer
    aspell
    ripgrep
    bash
    starship
    qutebrowser
    google-chrome
    spotify
    slack
    mpv
    ledger
    jq
    fzf
    fd
    nmap
    (pass.withExtensions (ext: [ ext.pass-otp ]))
    rofi-pass
    screen
    watchman
    youtube-dl
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
  ];

  fonts.fonts = with pkgs; [ hack-font go-font ];

  # Needed for nix-direnv. Prevents GC.
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';
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

  services.tailscale.enable = true;

  services.unbound.enable = true;

  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;
  services.avahi.publish.enable = true;

  services.keybase.enable = true;

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

  networking.hosts = { "127.0.0.1" = [ "sourcegraph.test" ]; };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
