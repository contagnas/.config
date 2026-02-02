{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  system,
  ...
}: rec {
  imports = [];

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
      system = "x86_64-linux";
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

        "Mod+R".action.spawn = lib.getExe pkgs.fuzzel;
        "Mod+W".action.spawn = lib.getExe inputs.window-switcher.defaultPackage.${system};
        "Mod+G".action.spawn = [(lib.getExe pkgs.google-chrome) "--remote-debugging-port=9222"];
        "Mod+Ctrl+Tab".action.spawn = "~/chrome-tab-switcher.nu";
        # "Mod+E".action.spawn = "${pkgs.emacs}/bin/emacs"; # need to use emacs-with-packages, not base emacs
        "Mod+E".action.spawn = lib.getExe config.programs.emacs.finalPackage; # need to use emacs-with-packages, not base emacs
        "Mod+T".action.spawn = lib.getExe pkgs.foot;

        "Mod+V".action = toggle-window-floating;
        "Mod+S".action = screenshot;
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

      animations.slowdown = 0.5;

      spawn-at-startup = [
        {
          command = [
            "bash"
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
        XDG_SESSION_TYPE = "wayland";
        # xdg-desktop-portal-wlr recognizes wlroots compositors via XDG_CURRENT_DESKTOP/XDG_SESSION_DESKTOP;
        # niri is not in its allowlist, so we spoof sway to ensure screencast works.
        XDG_CURRENT_DESKTOP = "sway";
        XDG_SESSION_DESKTOP = "sway";
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
