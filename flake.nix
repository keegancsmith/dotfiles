{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  outputs = { self, nixpkgs, darwin, ... }@attrs: {
    nixosConfigurations.habitat = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [ ./hosts/habitat/configuration.nix ];
    };
    darwinConfigurations.fa = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = attrs;
      modules = [ ./hosts/darwin/darwin-configuration.nix ];
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
