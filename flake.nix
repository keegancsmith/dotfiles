{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.05";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    kolide-launcher.url = "github:/kolide/nix-agent/main";
    kolide-launcher.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    claude-desktop.url = "github:k3d3/claude-desktop-linux-flake";
    claude-desktop.inputs.nixpkgs.follows = "nixpkgs";
    claude-desktop.inputs.flake-utils.follows = "flake-utils";
  };
  outputs = { self, nixpkgs, darwin, disko, kolide-launcher, flake-utils, ... }@attrs: {
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
  } // flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system}; in
    {
      formatter = pkgs.nixpkgs-fmt;
      packages = {
        counsel-repo = pkgs.callPackage ./lib/counsel-repo.nix { };
        git-spice = pkgs.callPackage ./lib/git-spice.nix { };
      };
    }
  );
}
