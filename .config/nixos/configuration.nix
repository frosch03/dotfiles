
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, libs, pkgs, unstable, ... }:
let 

in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./packages.nix
      ./nas-config.nix
      ./fonts.nix
      ./keyboard.nix
    ];

  nixpkgs.config.permittedInsecurePackages = [
    "erlang-22.3.4.24"
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "v2309"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # udev-rule according to: https://unix.stackexchange.com/a/415937
  # SUBSYSTEM=="usb", ACTION=="add|remove", ENV{ID_VENDOR}=="Lenovo", ENV{ID_MODEL}=="Lenovo_ThinkPad_Dock", RUN+="${pkgs.bash}/bin/bash /home/hoodoo/.local/bin/dock_handler.sh"
  #  ACTION=="add", SUBSYSTEMS=="usb", ATTRS{manufacturer}=="SABRENT", RUN+="${pkgs.bash}/bin/bash /home/frosch03/backupStorage/bin/test_device_added.sh"
  #  ACTION=="remove", SUBSYSTEMS=="usb", ATTRS{manufacturer}=="SABRENT", RUN+="${pkgs.bash}/bin/bash /home/frosch03/backupStorage/bin/test_device_removed.sh"


  services.udev.extraRules = ''
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="84:A9:38:A9:61:91", NAME="eth0"
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="F4:A4:75:A2:28:30", NAME="wlan0"

    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"

    # ACTION=="add", DRIVER=="usb", SUBSYSTEM=="usb", ATTRS{idVendor}=="17ef", ATTRS{idProduct}=="3082", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/frosch03/.Xauthority", RUN+="${pkgs.bash}/bin/sh -c 'echo == >> /home/frosch03/Desktop/udev-env.txt; env >> /home/frosch03/Desktop/udev-env.txt'"

    # ACTION=="ADD", DRIVER=="usb", SUBSYSTEM=="usb", ATTRS{idVendor}=="17ef", ATTRS{idProduct}=="3082", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/frosch03/.Xauthority", RUN+="${pkgs.bash}/bin/bash /home/frosch03/.screenlayout/smallDual3K.sh"
    
    KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/frosch03/.Xauthority", RUN+="${pkgs.bash}/bin/bash -c /home/frosch03/bin/switchScreenLayout.sh"
    KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/frosch03/.Xauthority", RUN+="${pkgs.bash}/bin/sh -c 'echo == >> /home/frosch03/Desktop/udev-env.txt; env >> /home/frosch03/Desktop/udev-env.txt'"

    # original RTL2832U vid/pid (hama nano, for example)
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0bda", ATTRS{idProduct}=="2832", ENV{ID_SOFTWARE_RADIO}="1", MODE="0660", GROUP="plugdev"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="6089", ENV{ID_SOFTWARE_RADIO}="1", MODE="0660", GROUP="plugdev"

  '';

  networking.extraHosts =
    ''
        127.0.0.1 manual.frosch03.de
        127.0.0.1 zeit.de
        127.0.0.1 www.zeit.de
        188.40.164.163 project.frosch03.de
    '';
  # networking.wireless.userControlled.enable = true;
  # networking.wireless.interfaces = [ "wlan0" ];
  # networking.wireless.networks.Internnet.pskRaw = "21412352020f3b9e734c40aa11905c2c48ae656b2e5f689f9d4d355656cfdd0d";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.interfaces.eth0.useDHCP = false;
  # networking.interfaces.wlan0.useDHCP = true;
  networking.usePredictableInterfaceNames = false;
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 8000 5984 ];
    # allowedUDPPortRanges = [
    #   { from = 4000; to = 4007; }
    #   { from = 8000; to = 8010; }
    # ];
  };


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  programs.hyprland = {
    enable = true;  
    withUWSM = true;
    xwayland.enable = true;
  };

  programs.waybar.enable = true;
  
  # systemd.services.touchegg = {
  #   description = "Touchegg Daemon";
  #   wantedBy = ["multi-user.target"];
  #   script = "${pkgs.touchegg}/bin/touchegg --daemon";
  # };

  services.displayManager.sddm = {
    enable = true;
  };
  
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = [ "modesetting" ];
    # dpi = 163;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    desktopManager.gnome.extraGSettingsOverrides = ''
      [org.gnome.desktop.interface]
      gtk-theme='Arc-Dark'
    ''; 
  };

  # TODO: Wenn NixOS config geupdated ist ... 
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        # For modern Intel CPU's
        intel-media-driver # Enable Hardware Acceleration
        # vpl-gpu-rt # Enable QSV
        # libvdpau-va-gl
      ];
    };
  };

  # Enable bluetooth (via: )https://nixos.wiki/wiki/Bluetooth)
  hardware.bluetooth.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  # Enable sound.
  services.pulseaudio.enable = false;

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;
  services.libinput.touchpad.naturalScrolling = true;
  services.libinput.touchpad.accelProfile = "flat";

  # adb setup
  programs.adb.enable = true;

  # enable Docker
  virtualisation.docker.enable = true;

  # Define user groups
  users.groups.plugdev = {};

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.frosch03 = {
    isNormalUser = true;
    description = "Matthias";
    extraGroups = [ "wheel"
                    "adbusers"
                    "dialout"
                    "docker"
                    "plugdev"
                    "input"
                    "uinput"
                  ];

  };


  environment.variables = {
    MOZ_USE_XINPUT2 = "1";
  };

  # TODO: Wenn NixOS config geupdated ist ... 
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "iHD";
  };

  security.polkit.enable = true;

  # systemd = {
  #   user.services.polkit-gnome-authentication-agent-1 = {
  #     description = "polkit-gnome-authentication-agent-1";
  #     wantedBy = [ "graphical-session.target" ];
  #     wants = [ "graphical-session.target" ];
  #     after = [ "graphical-session.target" ];
  #     serviceConfig = {
  #       Type = "simple";
  #       ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
  #       Restart = "on-failure";
  #       RestartSec = 1;
  #       TimeoutStopSec = 10;
  #     };
  #   };
  #   extraConfig = ''
  #    DefaultTimeoutStopSec=10s
  #  '';
  # };

  nixpkgs.config.allowUnfree = true;
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    arc-theme
    vim
    # emacs
    wget
    firefox
    gcc
    xorg.xrandr
    docker-compose
    pinentry-gtk2
    mate.mate-polkit
    unstable.obsidian
    polkit
    polkit_gnome

    mesa
    mesa-demos
    libGL
    glxinfo

    # Hyprland
    dunst                                                                            
    kitty                                                                            
    libnotify                                                                        
    networkmanagerapplet                                                             
    rofi                                                                     
    swww                                                                             
    swaylock
    swayidle
    unstable.ashell
    wpaperd
    
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  users.extraUsers.frosch03 = {
    shell = pkgs.zsh;
  };

  # List services that you want to enable:

  # tlp
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;

      # Optional helps save long term battery health
      START_CHARGE_THRESH_BAT0 = 40; # 40 and bellow it starts to charge
      STOP_CHARGE_THRESH_BAT0 = 90; # 90 and above it stops charging

    };
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.couchdb = {
    enable = true;
    package = pkgs.couchdb3;
    bindAddress = "127.0.0.1";
    adminPass = "test";
  };

  # services.couchdb.enable = true;
  # services.couchdb3.bindAddress = "127.0.0.1";
  # services.couchdb3.adminPass = "test";

  programs.light.enable = true;

  services.autorandr.enable = true;
  # services.autorandr.hooks.postswitch = {
  #   "xmonad" = "${pkgs.bash}/bin/bash -c 'xmonad --restart'";
  # };
  services.autorandr.profiles = {
    "laptopScreenOnly" = {
      fingerprint = {
        eDP-1 = "00ffffffffffff000dae171400000000001e0104a51e13780334d4a6544e9b240f515600000001010101010101010101010101010101423c80a070b024402e1ea6002dbc10000018000000fd00303c4a4a0f010a202020202020000000fe00434d4e010a2020202020202020000000fe004e3134304a434e2d4753390a20000e";
      };
      config = {
        eDP-1 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "1920x1200";
        };
      };
    };
    "smallDual3k" = {
      fingerprint = {
        DP-3-2 = "00ffffffffffff004c2d15104e3445302b200104b53f24783ac8b5ad50449e250f5054bfef80714f810081c08180a9c0b3009500010122cc0050f0703e801810350078682100001a000000fd001e4b1e873c000a202020202020000000fc004c5532385235350a2020202020000000ff00484e4d544130303838320a202001a902031cf0475f101f041303122309070783010000e305c000e3060501023a801871382d40582c450078682100001e565e00a0a0a029503020350078682100001a04740030f2705a80b0588a0078682100001e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000085";
        DP-3-3 = "00ffffffffffff004c2d15104647574332300104b53f24783ac8b5ad50449e250f5054bfef80714f810081c08180a9c0b30095000101e2ca0038f0703e801810350078682100001a000000fd001e4b1e873c000a202020202020000000fc004c5532385235350a2020202020000000ff0048345a4e3330313035330a202001e602031cf0475f101f041303122309070783010000e305c000e3060501023a801871382d40582c450078682100001e565e00a0a0a029503020350078682100001a04740030f2705a80b0588a0078682100001e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000085";
      };
      config = {
        eDP-1 = {
          enable = false;
        };
        DP-3-2 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "2560x1440";
        };
        DP-3-3 = {
          enable = true;
          position = "2560x0";
          mode = "2560x1440";
        };
      };
    };
    "smallDual3k_open" = {
      fingerprint = {
        eDP-1 = "00ffffffffffff000dae171400000000001e0104a51e13780334d4a6544e9b240f515600000001010101010101010101010101010101423c80a070b024402e1ea6002dbc10000018000000fd00303c4a4a0f010a202020202020000000fe00434d4e010a2020202020202020000000fe004e3134304a434e2d4753390a20000e";
        DP-3-2 = "00ffffffffffff004c2d15104e3445302b200104b53f24783ac8b5ad50449e250f5054bfef80714f810081c08180a9c0b3009500010122cc0050f0703e801810350078682100001a000000fd001e4b1e873c000a202020202020000000fc004c5532385235350a2020202020000000ff00484e4d544130303838320a202001a902031cf0475f101f041303122309070783010000e305c000e3060501023a801871382d40582c450078682100001e565e00a0a0a029503020350078682100001a04740030f2705a80b0588a0078682100001e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000085";
        DP-3-3 = "00ffffffffffff004c2d15104647574332300104b53f24783ac8b5ad50449e250f5054bfef80714f810081c08180a9c0b30095000101e2ca0038f0703e801810350078682100001a000000fd001e4b1e873c000a202020202020000000fc004c5532385235350a2020202020000000ff0048345a4e3330313035330a202001e602031cf0475f101f041303122309070783010000e305c000e3060501023a801871382d40582c450078682100001e565e00a0a0a029503020350078682100001a04740030f2705a80b0588a0078682100001e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000085";
      };
      config = {
        eDP-1 = {
          enable = false;
        };
        DP-3-2 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "2560x1440";
        };
        DP-3-3 = {
          enable = true;
          position = "2560x0";
          mode = "2560x1440";
        };
      };
    };
  };


  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 5"; }
      { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 5"; }
    ];
  };


  # Backup solution
  services.restic.backups = {
    local = {
      user = "frosch03";
      repository = "rclone:frognas:/share/CE_CACHEDEV1_DATA/Backups/v2308";
      passwordFile = "/etc/nixos/static/secrets/restic-password";
      paths = [ "/home/frosch03/backupStorage" ];
      # extraBackupArgs = [ "--exclude-file=/home/frosch03/backupStorage/Org/org-roam.bak" ];
      timerConfig = {
        OnCalendar = "daily";
      };
    };
  };  
  
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # NordVPN:
  # services.openvpn.servers = {
  #   frankfurtViaNordVPN  = { config = '' config /home/frosch03/Downloads/de1026.nordvpn.com.tcp443.ovpn ''; };
  # };

  services.openvpn.servers = {
    officeVPN = {
      autoStart = false;
      config = '' config /root/nixos/openvpn/smart.ovpn '';
      updateResolvConf = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version    of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

  # According to: https://www.emacswiki.org/emacs/TrampMode#h5o-33
  # Create symlinks to allow older versions of emacs tramp to connect to this computer
  system.activationScripts.tramp = ''
  for bin in ls uname base64; do
    if [ ! -e /bin/$bin ]; then
       ln -s /run/current-system/sw/bin/$bin /bin/$bin
    fi
  done
  if [ ! -e /bin/sudo ]; then
     ln -s /run/wrappers/bin/sudo /bin/sudo
  fi
  '';

  security.sudo = {
    enable = true;
    extraRules = [
      { commands = [{
          command = "/run/current-system/sw/bin/tlp chargeonce";
          options = [ "NOPASSWD" ];
        } {
          command = "/run/current-system/sw/bin/tlp-stat";
          options = [ "NOPASSWD" ];
        }
      ];
      groups = [ "wheel" ];
    }];
  };

  # According to: https://github.com/NixOS/nixpkgs/issues/3368#issuecomment-50434494
  # programs.bash.promptInit = ''
  # case "$TERM" in
  #   PROMPT_COLOR="1;31m"
  #   let $UID && PROMPT_COLOR="1;32m"
  #   xterm*|rxvt*|kterm|aterm|gnome*) # Others can go here.
  #     PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
  #     if test "$TERM" = "xterm"; then
  #       PS1="\[\033]2;\h:\u:\w\007\]$PS1"
  #     fi
  #     ;;
  #   *)
  #     PS1="[\u@\h:\w]$ "
  #     ;;
  # esac
  # '';

  # location.provider = "geoclue2"
  location.latitude = 48.785277;
  location.longitude = 9.1981657;
  # All values except 'enable' are optional.
  services.redshift = {
    enable = true;
    brightness = {
      # Note the string values below.
      day = "1";
      night = "0.8";
    };
    temperature = {
      day = 5500;
      night = 3700;
    };
  };

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    # pinentryPackage = "pinentry-gtk2";
    enableSSHSupport = true;
  };

  services.fwupd.enable = true;

  # Enable NTFS support
  boot.supportedFilesystems = [ "ntfs" ];

  # Blacklist DVB-T
  boot.extraModprobeConfig = "blacklist dvb_usb_rtl28xxu";

  # Modem Manager
  systemd = {
    services = {
      ModemManager.enable = true;
      ModemManager.wantedBy = [ "default.target" ];
    };
  };

  # For Obelisk and:
  # for devx shell for haskell
  # via: https://discourse.haskell.org/t/jerry-rigging-ghcup-on-nixos/7295/7
  nix.settings.substituters = [ 
    "https://nixcache.reflex-frp.org" # Obelisk
    "https://cache.iog.io"            # devx shell
    "https://cache.zw3rk.com"         # devx shell
  ];
  nix.settings.trusted-public-keys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" # Obelisk
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # devx shell
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="   # devx shell
  ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}


