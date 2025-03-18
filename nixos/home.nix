{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
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

    # fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    dejavu_fonts
    font-awesome
    jetbrains-mono
  ];

  gtk.cursorTheme = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
  };

  programs.home-manager.enable = true;
  programs.alacritty.enable = true;
  programs.emacs = {
    enable = true;
    # pgtk = pure gtk, has wayland support
    package = pkgs.emacs-pgtk;
  };
  programs.git.enable = true;
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    border.radius = "0";
  };

  stylix.targets.niri.enable = true;

  stylix.fonts = {
    sansSerif = config.stylix.fonts.monospace;
    serif = config.stylix.fonts.monospace;
  };

  programs.niri = {
    settings = let xwaylandPort = ":69"; in {
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
        "Mod+C".action = center-column;

        "Mod+WheelScrollUp".action = focus-workspace-up;
        "Mod+WheelScrollDown".action = focus-workspace-down;
        "Mod+Shift+WheelScrollUp".action = focus-column-left;
        "Mod+Shift+WheelScrollDown".action = focus-column-right;

        "Mod+R".action.spawn = "${pkgs.fuzzel}/bin/fuzzel";
        "Mod+G".action.spawn = "${pkgs.google-chrome}/bin/google-chrome-stable";
        # "Mod+E".action.spawn = "${pkgs.emacs}/bin/emacs"; # need to use emacs-with-packages, not base emacs
        "Mod+E".action.spawn = "${config.programs.emacs.finalPackage}/bin/emacs"; # need to use emacs-with-packages, not base emacs
        "Mod+T".action.spawn = "${pkgs.alacritty}/bin/alacritty";

        "Mod+V".action = toggle-window-floating;
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

        warp-mouse-to-focus = true;

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
      };

      animations.slowdown = 0.5;

      spawn-at-startup = [
        { command = ["${pkgs.xwayland-satellite}/bin/xwayland-satellite" xwaylandPort]; }
        { command = ["${pkgs.mako}/bin/mako"]; }
      ];

      environment = {
        DISPLAY = xwaylandPort;
        OZONE_PLATFORM = "wayland";
        NIXOS_OZONE_WL = "1";
        MOZ_ENABLE_WAYLAND = "1";
        XDG_SESSION_TYPE = "wayland";
        QT_QPA_PLATFORM = "wayland";
        GDK_BACKEND = "wayland";
        SDL_VIDEODRIVER = "wayland";
        GTK_THEME = "Adwaita:dark";
      };

    };
  };

  fonts.fontconfig.enable = true;

  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
