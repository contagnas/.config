{ config, pkgs, lib, ... }:

{
  home.username = "chills";
  home.homeDirectory = "/home/chills";

  home.sessionPath = [
    "$HOME/bin"
  ];

  home.sessionVariables = {
    GTK_THEME = "Adwaita:dark";
  };

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs;
    let patchedPywal = pywal.overrideAttrs(old : {
          patches = old.patches ++ [./pywal/rofi.patch];
        });
    in [
      scrot
      xclip
      steam
      protontricks
      blender
      htop
      killall
      google-chrome
      podman
      usbutils
      cmake
      discord
      arandr
      git
      emacs
      cachix
      ripgrep
      binutils
      rofi
      pavucontrol
      patchedPywal
      unzip
      inotify-tools
      pciutils
      jq
      patchelf
      fd
      nix-index
    ];

  services = {
    gnome-keyring.enable = true;

    random-background = {
      enable = true;
      imageDirectory = "%h/wp";
    };
  };

  programs = {
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    autorandr = {
      enable = true;
    };

    i3status = {
      enable = true;
      enableDefault = false;
      modules = {
        "volume master" = {
          position = 1;
          settings = {
            format = "♪ %volume";
            format_muted = "♪ muted (%volume)";
            device = "pulse:1";
          };
        };

        "tztime local" = {
          position = 2;
          settings = {
            format = "%Y-%m-%d %H:%M ";
          };
        };
      };
    };

    rofi = {
      enable = true;
      location = "top";
      theme = {
        "@import" = "${config.xdg.cacheHome}/wal/colors-rofi-dark";
      };
    };

    bash = {
      enable = true;
      bashrcExtra = ''
        (cat ~/.cache/wal/sequences &)
      '';
      profileExtra = ''
        eval "$(${pkgs.direnv}/bin/direnv hook bash)"
      '';
    };
  };



  xsession.enable = true;
  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    
    config = with builtins; with lib.attrsets; with lib.trivial;
      let
        alt = "Mod1";
        win = "Mod4";
        combineSets = zipAttrsWith (s: fs: head fs);
        workspaceKeys =
          let workspaceList = genList (x: (x + 1)) 9;
              shiftNum = [
                "parenright"
                "exclam"
                "at"
                "numbersign"
                "dollar"
                "percent"
                "asciicircum"
                "ampersand"
                "asterisk"
                "parenleft"
              ];
              workspaceSwitchKeys = map (ws: nameValuePair (toString ws) "workspace ${toString ws}") workspaceList;
              workspaceMoveKeys = map (ws: nameValuePair "Shift+${elemAt shiftNum ws}" "move container to workspace ${toString ws}") workspaceList;
          in listToAttrs (workspaceSwitchKeys ++ workspaceMoveKeys);
        regularKeys = {
          "h" = "focus left";
          "j" = "focus down";
          "k" = "focus up";
          "l" = "focus right";
          "Shift+H" = "move left";
          "Shift+J" = "move down";
          "Shift+K" = "move up";
          "Shift+L" = "move right";
          "v" = "split v";
          "b" = "split h";
          "f" = "fullscreen";
          "shift+f" = "fullscreen toggle global";
          "q" = "kill";
          "Shift+space" = "floating toggle";
          "space" = "focus mode_toggle";
          "w" = "exec ${pkgs.rofi}/bin/rofi -show window";
          "r" = "exec ${pkgs.rofi}/bin/rofi -show run";
          "g" = "exec ${pkgs.google-chrome}/bin/google-chrome-stable";
          "e" = "exec ${pkgs.emacs}/bin/emacs";
          "t" = "exec ${pkgs.termite}/bin/termite";
          "s" = "exec ${pkgs.scrot}/bin/scrot -s -e '${pkgs.xclip}/bin/xclip -selection clipboard -t image/png -i $f'";
          "Shift+C" = "reload";
          "Shift+R" = "restart";
          "Shift+Q" = "Quit";
          "Escape" = "mode i3";
          "i" = "mode default";
          "F12" = "mode pass";
        };
        stickyKeys = {
          # keys which do not cause i3 mode to revert to default
          "grave" = "mode god";
          "${alt}+h" = "resize shrink width 10 px or 10 ppt";
          "${alt}+j" = "resize grow height 10 px or 10 ppt";
          "${alt}+k" = "resize shrink height 10 px or 10 ppt";
          "${alt}+l" = "resize grow width 10 px or 10 ppt";
        };
        mapSet = fkey: fvalue: set:
          mapAttrs' (name: value: nameValuePair (fkey name) (fvalue value)) set;
        mapKeys = fkey: mapSet fkey id;
        mapValues = fvalue: mapSet id fvalue;
      in {
        terminal = "termite";
        modifier = win;
        floating.modifier = alt;
        fonts = {
          names = [ "DejaVu Sans Mono" ];
          size = 11.0;
        };

        gaps = {
          inner = 10;
          outer = -5;
          top = 0;
          bottom = 0;
        };

        bars = [
          {
            fonts = {
              names = [ "DejaVu Sans Mono" ];
              size = 8.0;
            };
            statusCommand = "${pkgs.i3status}/bin/i3status";
          }
        ];

        keybindings = combineSets [
          {
            "${alt}+Escape" = "mode i3";
            "${alt}+grave" = "mode god";
          }
          (mapKeys (key: "${win}+${key}") (combineSets [regularKeys stickyKeys workspaceKeys]))
        ];
        modes = {
          i3 = combineSets [
            (mapValues (value: "${value}; mode default") (combineSets [regularKeys workspaceKeys]))
            stickyKeys
          ];
          god = combineSets [
            {
              "grave" = "mode default";
              "Escape" = "mode default";
            }
            regularKeys
            stickyKeys
            workspaceKeys
          ];
          pass = {
            "F12" = "mode default";
          };
        };
        colors = let
          # let colors be managed with xresources
          defcolor = {
            border = "$bg";
            background = "$bg";
            text = "$fg";
            indicator = "$bg";
            childBorder = "$bg";
          };
        in {
          focused = defcolor;
          focusedInactive = defcolor;
          unfocused = defcolor;
          urgent = defcolor;
          placeholder = defcolor;
          background = "$bg";
        };
      };
    extraConfig = ''
        # fake-outputs 1280x1440+0+0,2560x1440+1280+0,1280x1440+3840+0+0

        # Set colors from Xresources
        # Change 'color7' and 'color2' to whatever colors you want i3 to use
        # from the generated scheme.
        # NOTE: The '#f0f0f0' in the lines below is the color i3 will use if
        # it fails to get colors from Xresources.
        set_from_resource $fg i3wm.color7 #f0f0f0
        set_from_resource $bg i3wm.color2 #f0f0f0

        # float for bevy
        for_window [class="^mousechat$" title=".*"] floating enable
        for_window [class="^miner_chat" title=".*"] floating enable
        for_window [class="^time2smash" title=".*"] floating enable
        for_window [class="^lostcities" title=".*"] floating enable
      '';
  };
}
