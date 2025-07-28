{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  stylix.enable = true;
  stylix.polarity = "dark";
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/chalk.yaml";
  stylix.image = ./wallpaper.png;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = ["ntfs"];
  boot.kernelParams = [
    "cgroup_enable=memory"
    "cgroup_enable=cpuset"
    "cgroup_memory=1"
    "nvidia-drm.modeset=1"
    "nvidia-drm.fbdev=1"
  ];

  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  networking.networkmanager.enable = false;

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    defaultNetwork.settings.dns_enabled = true;
    dockerSocket.enable = true;
  };

  time.timeZone = "America/Los_Angeles";

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp6s0.useDHCP = true;

  nix = {
    extraOptions = ''
        experimental-features = nix-command flakes ca-derivations dynamic-derivations
    '';
    package = pkgs.nixVersions.nix_2_26;
    settings.trusted-users = [ "root" "chills" ];
  };

  services.xserver.videoDrivers = ["nvidia"];
  programs.niri.enable = true;

  hardware.nvidia = {
    # modesetting is required
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    # do not use the nvidia open source kernel module 
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  security.rtkit.enable = true;

  musnix.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  users.users.chills = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "networkmanager" "podman" "audio"]; 
    shell = pkgs.nushell;
  };

  networking.firewall.allowedTCPPorts = [ 6969 8080 8000 8980 ];

  environment.systemPackages = with pkgs; [
    git
  ];

  system.stateVersion = "21.11";
}
