{
  description = "Niri Window Switcher with fuzzel";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      packages.x86_64-linux.niri-fuzzel-switcher = pkgs.stdenv.mkDerivation {
        pname = "niri_fuzzel_switcher";
        version = "1.0";

        src = ./.;

        nativeBuildInputs = [ pkgs.makeWrapper ];
        buildInputs = [ pkgs.jq pkgs.niri pkgs.fuzzel ];

        installPhase = ''
          mkdir -p $out/bin
          install -m 755 $src/niri_fuzzel_switcher $out/bin/niri_fuzzel_switcher
          wrapProgram $out/bin/niri_fuzzel_switcher \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.jq pkgs.niri pkgs.fuzzel ]}
        '';
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.niri-fuzzel-switcher;
    };
}
