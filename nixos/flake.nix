{
  description = "nixos configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    window-switcher = {
      url = ./pkgs/window-switcher;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    musnix = {
      url = "github:musnix/musnix";
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
            home-manager.backupFileExtension = "hmbackup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.chills = ./home.nix;
            home-manager.extraSpecialArgs = {inherit inputs;};
          }
          inputs.musnix.nixosModules.musnix
        ];
      };
    };
  };
}
