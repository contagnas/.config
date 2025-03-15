{inputs, ...}: {
  # custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs final.pkgs;

  modifications = final: prev: {
  };

  inherit inputs.niri.overlays.niri;

  # unstable nixpkgs will be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
