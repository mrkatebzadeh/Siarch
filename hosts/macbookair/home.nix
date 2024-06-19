{ config, pkgs, outputs, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
in
{
  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
      outputs.overlays.local
    ];
    config = {
      allowUnfree = true;
    };
  };
  home.packages = with pkgs; [
    unstable.sketchybar-app-font
    fonts.sf-pro
  ];
  programs.kitty = {
    enable = true;
    settings = {
      font_size = 14;
      macos_titlebar_color = "#303446";
    };
  };


  xdg.configFile."borders".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/borders";
  xdg.configFile."sketchybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/sketchybar";
  xdg.configFile."skhd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/skhd";
  xdg.configFile."yabai".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/yabai";
  xdg.configFile."iterm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/iterm";
}
