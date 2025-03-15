{ 
  description = "Home Manager Configuration";

  inputs = {
    home-manager.url = "github:nix-community/home-manager";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { home-manager, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      username = "chills";
    in {
      homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          #overlays = [
          #  (import (builtins.fetchTarball {
          #    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
          #    sha256 = "11dx9xcf71g991k25vv840k6np0f3q6r0x3dgwmcmx6dxv0alsk4";
          #  }))
          #];
          config.allowUnfree = true;
          config.permittedInsecurePackages = [
            "python3.10-mistune-0.8.4"
          ];
        };

        modules = [
          ./home.nix
          {
            home = {
              username = "chills";
              homeDirectory = "/home/${username}";
              stateVersion = "22.05";
            };
          }
        ];
      };
    };
}
