{ pkgs, ... }: {
  # Virtualization for windows VM https://nixos.wiki/wiki/Virt-manager
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = [ "keegan" ];
  users.groups.kvm.members = [ "keegan" ];
  virtualisation.spiceUSBRedirection.enable = true;
  virtualisation.libvirtd.enable = true;

  # Enable TPM emulation (for Windows 11). From https://crescentro.se/posts/windows-vm-nixos/
  virtualisation.libvirtd.qemu = {
    swtpm.enable = true;
    ovmf.packages = [ pkgs.OVMFFull.fd ];
  };

  environment.systemPackages = [ pkgs.virtiofsd ];
}
