{
  description = "nixos configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix/release-24.11";
      # inputs.nixpkgs.follows = "nixpkgs";
      # inputs.home-manager.follows = "home-manager";
    };

  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
    systems = [
      "x86_64-linux"
    ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in {
    # `nix fmt`
    formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixos);

    # custom packages exported as overlays
    overlays = [inputs.niri.overlays.niri];

    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .'
    nixosConfigurations = {
      # key should match system hostname
      nixos = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./configuration.nix
          inputs.niri.nixosModules.niri
          inputs.stylix.nixosModules.stylix
          home-manager.nixosModules.home-manager {
            home-manager.backupFileExtension = "hmbak";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.chills = import ./home.nix;
          }
        ];
      };
    };
  };
}
