{
  nix.settings = {
    substituters = [ "https://nixpkgs-terraform.cachix.org" ];
    trusted-public-keys = [
      "nixpkgs-terraform.cachix.org-1:8Sit092rIdAVENA3ZVeH9hzSiqI/jng6JiCrQ1Dmusw="
    ];
  };
}
