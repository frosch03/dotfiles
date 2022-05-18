{ config, pkgs, ... }:

let
  dotfrog = (import ~/localStorage/dotfiles) {};

in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = false;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mb";
  home.homeDirectory = "/home/mb";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  home.packages = with pkgs; [
    stdenv
    gnumake
    pkg-config

    # dotfrog
    xcompmgr
    xcape
    dunst
    feh
    xbindkeys
    lxappearance
    dex
    
    tmux
    rxvt-unicode
    alacritty
    tree
    unixtools.killall
    neofetch
    htop

    xcape
    xorg.xmodmap
    xorg.xkbcomp
    xorg.xclock
    xorg.xeyes

    lemonbar-xft
    conky
    acpi
    lm_sensors
    mpd
    mpc_cli
    pavucontrol
    mpv
    ffmpeg

    zsh
    oh-my-zsh
    fzf
    keychain

    git
    git-crypt

    ag
    xscreensaver
    arandr
    ispell

    # Mail
    mu
    isync
    w3m

    # IM
    signal-desktop
    ferdi

    ghc
    python3
    python39Packages.pip

    # Utilities
    restic
    rclone
    pass
    wpa_supplicant_gui
    zip
    unzip

    broot                       # file browser within console
    amfora                      # gemini terminal client
    toilet                      # for the crazy ascii art fonts
    mupdf
    pandoc
    imagemagick
    graphviz
    librecad
    gimp


    # Photo
    rawtherapee
    darktable

    (texlive.combine {
      inherit (texlive) scheme-medium koma-script capt-of minted fvextra upquote catchfile xstring framed xcolor latex-fonts collection-fontsrecommended biblatex biblatex-apa wrapfig ulem mathtools multirow beamer listings dvipng metafont datetime xltabular;
    })

    # Job specifics
    ## haskell-language-server
    chromium
    cabal-install
    jdk11

    hunspell
    hunspellDicts.de-de
    hunspellDicts.en-us

    nyxt

    msmtp
  ];

  programs.tmux = {
	  enable = true;
	  clock24 = true;
	  plugins = with pkgs.tmuxPlugins; [
		  sensible
		  yank
		  {
			  plugin = dracula;
			  extraConfig = ''
				set -g @dracula-show-battery false
				set -g @dracula-show-powerline true
				set -g @dracula-refresh-rate 10
			'';
		  }
	  ];

	  extraConfig = ''
		set -g mouse on
	'';
  };  
  
}
