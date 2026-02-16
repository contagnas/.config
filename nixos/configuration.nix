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
    # Preserve VRAM allocations across DPMS to avoid NVKMS allocation failures on wake.
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
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

  systemd.settings = {
    Manager = {
      DefaultTimeoutStopSec = "10s";
    };
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.dhcpcd.wait = "if-carrier-up";
  services.upower.enable = true;
  networking.interfaces.enp6s0.useDHCP = true;

  nix = {
    extraOptions = ''
        experimental-features = nix-command flakes ca-derivations dynamic-derivations
    '';
    package = pkgs.nixVersions.nix_2_28;
    settings.trusted-users = [ "root" "chills" ];
  };

  services.xserver.videoDrivers = ["nvidia"];
  services.greetd = {
    enable = true;
    useTextGreeter = true;
    settings = {
      initial_session = {
        user = "chills";
        command = "${pkgs.niri}/bin/niri-session";
      };
      default_session = {
        command = "${pkgs.greetd}/bin/agreety --cmd ${pkgs.niri}/bin/niri-session";
      };
    };
  };
  programs.niri = {
    enable = true;
    package = pkgs.niri;
  };
  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      stdenv.cc.cc
      zlib
    ];
  };

  hardware.nvidia = {
    # modesetting is required
    modesetting.enable = true;
    powerManagement.enable = true;
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

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [];
    config.common = {
      default = ["wlr"];
      "org.freedesktop.impl.portal.ScreenCast" = ["wlr"];
      "org.freedesktop.impl.portal.Screenshot" = ["wlr"];
    };
  };

  users.users.chills = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "networkmanager" "podman" "audio"]; 
    shell = pkgs.nushell;
  };

  # xdg-desktop-portal-wlr expects a wlroots compositor name; use sway to avoid "niri" being rejected.
  environment.sessionVariables = {
    XDG_CURRENT_DESKTOP = "sway";
    XDG_SESSION_DESKTOP = "sway";
  };

  networking.firewall.allowedTCPPorts = [ 6969 8080 8000 8980 9757 ];
  networking.firewall.allowedUDPPorts = [ 5353 9757 ];

  environment.systemPackages = with pkgs; [
    git
    alsa-utils
  ];

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="2708", ATTRS{idProduct}=="0006", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}+="audient-restore.service", ENV{SYSTEMD_UID}="${toString config.users.users.chills.uid}"
  '';

  systemd.user.services.audient-restore = {
    description = "Restore Audient EVO 4 ALSA settings";
    serviceConfig = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${config.users.users.chills.home}/.config/alsa";
      ExecStart = "${pkgs.alsa-utils}/bin/alsactl --file ${config.users.users.chills.home}/.config/alsa/audient-evo4.state restore EVO4";
    };
  };

  systemd.user.services.audient-save = {
    description = "Automatically save Audient EVO 4 ALSA settings";
    serviceConfig = {
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${config.users.users.chills.home}/.config/alsa";
      ExecStart = "${pkgs.alsa-utils}/bin/alsactl --file ${config.users.users.chills.home}/.config/alsa/audient-evo4.state daemon EVO4";
      Restart = "always";
    };
    wantedBy = [ "default.target" ];
  };

  system.stateVersion = "21.11";
}
