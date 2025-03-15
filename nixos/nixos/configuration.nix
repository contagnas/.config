{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./musnix
    ];

  nixpkgs.config.allowUnfree = true;

  musnix.enable = true;

  stylix.enable = true;
  # stylix.targets.cavalier.enable  = false;
  stylix.polarity = "dark";
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/chalk.yaml";
  stylix.image = ../wallpaper.png;

  # stylix.image = pkgs.fetchurl {
  #   url = "https://www.pixelstalk.net/wp-content/uploads/2016/05/Epic-Anime-Awesome-Wallpapers.jpg";
  #   sha256 = "enQo3wqhgf0FEPHj2coOCvo7DuZv+x5rL/WIo4qPI50=";
  # };


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
        experimental-features = nix-command flakes ca-derivations
    '';
    package = pkgs.nixVersions.latest;
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

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.rate" = 48000;
        "default.clock.quantum" = 32;
        "default.clock.min-quantum" = 32;
        "default.clock.max-quantum" = 32;
      };
    };
  };

  users.users.chills = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "networkmanager" "podman" "audio"]; 
  };

  networking.firewall.allowedTCPPorts = [ 6969 8080 8000 8980 ];

  environment.systemPackages = with pkgs; [
    git
  ];

  system.stateVersion = "21.11";
}
