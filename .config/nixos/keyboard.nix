{ pkgs, ... }:

{
  services.xserver.xkb.extraLayouts = {
    custom-frog = {
      description = "frosch03's keyboard layout";
      languages = [ "eng" ];
      symbolsFile = pkgs.writeText "frogs-symbols.xkb" (builtins.readFile ./frogs-symbols);
    };
  };
  services.xserver.xkb.layout = "custom-frog";
  services.xserver.xkb.variant = "";  
  services.xserver.xkb.options = "altgr-intl,altgr:altgr,terminate:ctrl_alt_bksp";  

  services.kmonad = {
    enable = true;
    keyboards = {
      x1KeyboardKMonadOutput = {
        device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
        config = builtins.readFile ./kmonad/x1-keyboard.xkb;
      };
      ergodoxEzKMonadOutput = {
        device = "/dev/input/by-id/usb-ZSA_Ergodox_EZ_0-event-kbd";
        config = builtins.readFile ./kmonad/ergodox.xkb;
      };
    };
  };
}
