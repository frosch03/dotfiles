{ config, pkgs, lib, ... }:

let
  pragmatapro = pkgs.callPackage ./pragmata.nix { };

in {
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      corefonts
      inconsolata ubuntu_font_family dejavu_fonts
      lmodern source-code-pro
      fira fira-code fira-code-symbols fira-mono
      noto-fonts noto-fonts-cjk-sans noto-fonts-emoji
      pragmatapro
      inconsolata
      pkgs.nerd-fonts._0xproto
      pkgs.nerd-fonts.droid-sans-mono
    ];
  };
}