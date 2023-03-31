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
      modules = [ ./hosts/fa/darwin-configuration.nix ];
    };
  };
}
