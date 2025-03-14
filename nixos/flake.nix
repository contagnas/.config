{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      inherit (self) outputs;
    in {
      nixosConfigurations = {
        # key here matches `hostname` => `nixos-rebuild --flake .`
        # otherwise, `nixos-rebuild --flake .#nixos`
        nixos = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs system; };

          modules = [./configuration.nix];
        };
      };
    };
}
