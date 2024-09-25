{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    kolide-launcher.url = "github:/kolide/nix-agent/main";
    kolide-launcher.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, darwin, disko, kolide-launcher, ... }@attrs: {
    nixosConfigurations.habitat = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [
        disko.nixosModules.disko
        kolide-launcher.nixosModules.kolide-launcher
        ./hosts/habitat/configuration.nix
        ./lib/work.nix
      ];
    };
    darwinConfigurations.fa = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = attrs;
      modules = [
        ./hosts/darwin/darwin-configuration.nix
        ./hosts/darwin/work.nix
        ./lib/work.nix
      ];
    };
    darwinConfigurations.cliche = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      specialArgs = attrs;
      modules = [ ./hosts/darwin/darwin-configuration.nix ];
    };
    darwinConfigurations.real = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      specialArgs = attrs;
      modules = [ ./hosts/darwin/darwin-configuration.nix ];
    };
    formatter = {
      x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
      x86_64-darwin = nixpkgs.legacyPackages.x86_64-darwin.nixpkgs-fmt;
      aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;
    };
  };
}
