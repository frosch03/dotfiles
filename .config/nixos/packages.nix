{ config, lib, pkgs, modulesPath, unstable, ... }:
let 
  # unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in

{
    environment.systemPackages = with pkgs; [
        unstable.emacs
        vscode
        # firefox
        stdenv
        gnumake
        pkg-config

    #    dotfrog
        xcompmgr
        xcape
        dunst
        feh
        xbindkeys
        lxappearance
        dex
        lxsession
        
        tmux
        rxvt-unicode
        alacritty
        tree
        unixtools.killall
        neofetch

        xcape
        xorg.xmodmap
        xorg.xkbcomp
        xorg.xclock

        lemonbar-xft
        conky
        acpi
        lm_sensors
        mpd
        mpc_cli
        pavucontrol
        mpv
        vlc
        ffmpeg
        zenity

        zsh
        oh-my-zsh
        fzf
        keychain
        bc
        unison

        unzip

        git
        git-crypt
        gnupg

        # For gsettings
        glib
        arc-theme

        silver-searcher # was: ag
        unstable.xscreensaver
        arandr
        # ispell
        aspell
        aspellDicts.en
        aspellDicts.de

        # Mail
        mu
        isync
        w3m

        ghc
        haskellPackages.X11
        haskellPackages.xmonad
        haskellPackages.xmonad-extras
        haskellPackages.xmonad-contrib

        python3
        poetry

        # couchdb3

        # Utilities
        restic
        rclone
        pass
        wpa_supplicant_gui
        wmctrl
        maestral

        broot                       # file browser within console
        amfora                      # gemini terminal client
        toilet                      # for the crazy ascii art fonts
        mupdf
        evince
        zathura
        pandoc
        imagemagick
        librecad
        gimp
        geeqie

        # Photo
        rawtherapee
        darktable

        arduino
        arduino-cli

        discord

        # SDR
        rtl-sdr
        gqrx
        unstable.wsjtx
        unstable.gridtracker
    
        # steam
        steam
        xorg.libxcb
        xorg.libXinerama

        (makeDesktopItem {
        name = "org-protocol";
        exec = "emacsclient %u";
        comment = "Org protocol";
        desktopName = "org-protocol";
        type = "Application";
        mimeTypes = ["x-scheme-handler/org-protocol"];
        })

        (texlive.combine {
        inherit (texlive) scheme-medium koma-script capt-of minted fvextra upquote catchfile xstring framed xcolor latex-fonts collection-fontsrecommended biblatex biblatex-apa wrapfig ulem mathtools multirow beamer listings dvipng metafont datetime xltabular;
        })
        biber

        haskell-language-server

        unstable.signal-desktop-bin
        graphviz
        chromium
        nyxt

        jre_minimal

        tuir # commandline reddit browser

        dosbox
        unstable.minetest

        linphone
        unstable.brave

        nodejs
    ];

    services.picom.enable = false;

    # programs.vscode = {
    #     enable = true;
    #     extensions = with pkgs.vscode-extensions; [
    #       # dracula-theme.theme-dracula
    #     ];
    # };

    # programs.direnv = {
    #     enable = true;
    #     enableNixDirenvIntegration = true;
    # };

    programs.starship = {
        enable = true;
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableBashCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      histSize = 10000;

      ohMyZsh = {
        enable = true;
        # plugins = [ "git" "dirhistory" "history" ];
        plugins = [ "git" ];
        theme = "eastwood";
      };
    };

    programs.thunderbird.enable = false;
    programs.evolution.enable = true;
}
