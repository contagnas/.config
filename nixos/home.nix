{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: rec {
  imports = [
    inputs.noctalia.homeModules.default
  ];

  home = {
    username = "chills";
    homeDirectory = "/home/chills";
  };

  home.packages = with pkgs; [
    # apps
    google-chrome
    discord
    steam
    spotify

    # tools
    just
    ripgrep
    fd
    direnv
    atuin
    unzip
    jq
    helvum

    # fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    dejavu_fonts
    font-awesome
    jetbrains-mono

  ];

  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 24;
    # gtk.enable = true;
    # x11.enable = true;
  };

  programs.home-manager.enable = true;
  programs.alacritty.enable = true;
  programs.foot.enable = true;
  programs.htop.enable = true;
  programs.nushell.enable = true;
  programs.emacs = {
    enable = true;
    # pgtk = pure gtk, has wayland support
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      epkgs.filechooser
    ];
  };
  services.emacs = {
    enable = true;
    package = config.programs.emacs.finalPackage;
    client.enable = true;
    startWithUserSession = "graphical";
  };
  systemd.user.services.emacs.Service = {
    Restart = lib.mkForce "always";
    RestartSec = "1s";
  };
  programs.git.enable = true;
  programs.fuzzel.enable = true;
  programs.fuzzel.package = (pkgs.fuzzel.overrideAttrs
    (oldAttrs: rec {
      src = pkgs.fetchFromGitea {
        domain = "codeberg.org";
        owner = "chills";
        repo = "fuzzel";
        rev = "1ec6cf21fe08e8a827e7501252ba9f37292ba969";
        hash = "sha256-IDcIeJCXi2Q5x/lRzgKe/9Epeum0KZIGJlzNwPwnmXk=";
      };
    })
  );
  programs.fuzzel.settings = {
    border.radius = "0";
    main.dpi-aware = "no";
    main.line-height = "16";
  };

  programs.noctalia-shell = {
    enable = true;
    systemd.enable = true;
    settings = import ./noctalia/settings.nix;
    colors = import ./noctalia/colors.nix;
    plugins = import ./noctalia/plugins.nix;
  };

  systemd.user.services.noctalia-lock-on-start = {
    Unit = {
      Description = "Lock Noctalia on session start";
      ConditionPathExists = "!%t/noctalia-lock-on-start.done";
      After = [
        config.wayland.systemd.target
        "noctalia-shell.service"
      ];
      PartOf = [ config.wayland.systemd.target ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -lc 'i=0; while [ $i -lt 20 ]; do ${lib.getExe config.programs.noctalia-shell.package} ipc call lockScreen lock && ${pkgs.coreutils}/bin/touch \"$XDG_RUNTIME_DIR/noctalia-lock-on-start.done\" && exit 0; i=$((i+1)); ${pkgs.coreutils}/bin/sleep 0.5; done; exit 0'";
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
  };

  stylix.targets.niri.enable = true;
  # stylix.targets.mako.enable = true;
  stylix.targets.foot.enable = true;

  stylix.fonts = {
    sansSerif = config.stylix.fonts.monospace;
    serif = config.stylix.fonts.monospace;
  };

  programs.niri = {
    settings = let
      xwaylandPort = ":69";
    in {
      binds = with config.lib.niri.actions; {
        "Mod+Shift+E".action = quit;
        "Mod+Q".action = close-window;

        "Mod+H".action = focus-column-left;
        "Mod+J".action = focus-window-or-workspace-down;
        "Mod+K".action = focus-window-or-workspace-up;
        "Mod+L".action = focus-column-right;

        "Mod+Shift+H".action = move-column-left;
        "Mod+Shift+J".action = move-window-down-or-to-workspace-down;
        "Mod+Shift+K".action = move-window-up-or-to-workspace-up;
        "Mod+Shift+L".action = move-column-right;

        "Mod+Alt+H".action = set-column-width "-5%";
        "Mod+Alt+L".action = set-column-width "+5%";

        "Mod+F".action = switch-preset-column-width;
        "Mod+Shift+F".action = fullscreen-window;
        "Mod+Ctrl+F".action = toggle-windowed-fullscreen;
        "Mod+C".action = center-column;
        "Mod+Tab".action = toggle-overview;

        "Mod+WheelScrollUp".action = focus-workspace-up;
        "Mod+WheelScrollDown".action = focus-workspace-down;
        "Mod+Shift+WheelScrollUp".action = focus-column-left;
        "Mod+Shift+WheelScrollDown".action = focus-column-right;

        "Mod+R".action.spawn = [
          (lib.getExe config.programs.noctalia-shell.package)
          "ipc"
          "call"
          "launcher"
          "toggle"
        ];
        "Mod+W".action.spawn = [
          (lib.getExe config.programs.noctalia-shell.package)
          "ipc"
          "call"
          "launcher"
          "windows"
        ];
        "Mod+G".action.spawn = [(lib.getExe pkgs.google-chrome) "--remote-debugging-port=9222"];
        "Mod+Ctrl+Tab".action.spawn = "${config.home.homeDirectory}/chrome-tab-switcher.nu";
        "Mod+E".action.spawn = [
          (lib.getExe' config.programs.emacs.finalPackage "emacsclient")
          "-c"
          "-n"
        ];
        "Mod+T".action.spawn = lib.getExe pkgs.foot;

        "Mod+V".action = toggle-window-floating;
        "Mod+S".action = screenshot;
        "Mod+Shift+S".action = set-dynamic-cast-window;
      };

      outputs."DP-1" = {
        mode = {
          width = 5120;
          height = 1440;
          refresh = 119.970;
        };
      };

      input = {
        keyboard = {
          repeat-delay = 200;
          repeat-rate = 50;
        };

        mouse.accel-profile = "flat";

        focus-follows-mouse = {
            enable = true;
            max-scroll-amount = "25%";
        };

        warp-mouse-to-focus = { enable = true; };

      };

      prefer-no-csd = true;

      layout = {
        gaps = 16;
        center-focused-column = "never";
        "background-color" = "transparent";

        preset-column-widths = [
          { proportion = 0.25; }
          { proportion = 0.50; }
        ];

        default-column-width = { proportion =0.25; };

        focus-ring = {
          width = 10000;
          active.color = "#00000055";
        };

        empty-workspace-above-first = true;
      };

      "layer-rules" = [
        {
          matches = [
            { namespace = "^noctalia-wallpaper*"; }
          ];
          "place-within-backdrop" = true;
        }
      ];

      "window-rules" = [
        {
          matches = [
            {
              "app-id" = "^emacs$";
              title = "^filechooser-frame$";
            }
          ];
          "open-floating" = true;
        }
      ];

      overview = {
        "workspace-shadow".enable = false;
      };

      debug = {
        "honor-xdg-activation-with-invalid-serial" = [ ];
      };

      animations.slowdown = 0.5;

      spawn-at-startup = [
        {
          command = [
            (lib.getExe pkgs.bash)
            "-c"
            "${lib.getExe pkgs.xwayland-satellite} ${xwaylandPort} &> ~/.xwayland-satellite.log"
          ];
        }
        # { command = [(lib.getExe pkgs.mako)]; }
      ];

      environment = {
        DISPLAY = xwaylandPort;
        OZONE_PLATFORM = "wayland";
        NIXOS_OZONE_WL = "1";
        MOZ_ENABLE_WAYLAND = "1";
        GTK_USE_PORTAL = "1";
        XDG_SESSION_TYPE = "wayland";
        XDG_CURRENT_DESKTOP = "niri";
        XDG_SESSION_DESKTOP = "niri";
        QT_QPA_PLATFORM = "wayland";
        GDK_BACKEND = "wayland";
        # SDL_VIDEODRIVER = "wayland";
        GTK_THEME = "Adwaita:dark";
      };

    };
  };

  fonts.fontconfig.enable = true;

  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
