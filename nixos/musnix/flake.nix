{
  description = "Real-time audio in NixOS";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }: {
    nixosModules.musnix = import ./default.nix;
    nixosModules.default = self.nixosModules.musnix;
  };
}
