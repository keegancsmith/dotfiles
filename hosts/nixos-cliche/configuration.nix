{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos-cliche";

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  networking.useDHCP = false;
  networking.interfaces.ens33.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  hardware.opengl.enable = true;

  services.xserver = {
    enable = true;

    layout = "us";

    xkbOptions = "caps:escape";

    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };

    displayManager = { defaultSession = "xfce+i3"; };

    displayManager.autoLogin = {
      enable = true;
      user = "keegan";
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [ rofi dmenu i3status i3lock ];
    };
  };

  # compositor
  services.picom.enable = true;

  users.users.keegan = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "sound" ];
    shell = pkgs.fish;
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
    bash
    dillo
    direnv
    emacsPgtkGcc
    fzf
    git
    go
    jump
    kitty
    man-pages
    man-pages-posix
    ripgrep
    starship
    zig
    zls
  ];

  fonts.fonts = with pkgs; [ hack-font go-font ];

  # Needed for nix-direnv. Prevents GC.
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    extra-experimental-features = nix-command flakes
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

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.tailscale.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Default is performance. This seems like a more reasonable default, even
  # though this is a desktop machine.
  powerManagement.cpuFreqGovernor = "schedutil";

  virtualisation.vmware.guest.enable = true;

  nix.settings.allowed-users = [ "@wheel" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
