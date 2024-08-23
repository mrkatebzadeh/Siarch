{ config, pkgs, outputs, lib, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
  emacsPackage = pkgs.emacs29-macport;
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
    pkg-config
    mu
    isync
    msmtp
    pass
    meson
    unstable.sketchybar-app-font
    fonts.sf-pro
    scripts.common
    scripts.macos
  ];
  programs.kitty = {
    enable = true;
    settings = {
      font_size = 14;
      macos_titlebar_color = "#303446";
    };
  };

  programs.emacs = {
    enable = true;
    package = emacsPackage;
    extraPackages = epkgs: [
      epkgs.mu4e
    ];
  };

  xdg.configFile."borders".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/borders";
  xdg.configFile."sketchybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/sketchybar";
  xdg.configFile."skhd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/skhd";
  xdg.configFile."yabai".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/yabai";
  xdg.configFile."iterm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/iterm";
  xdg.configFile."emacs".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/emacs.d";
  home.file.".local/share/maildir".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/share/maildir";
  home.file.".mbsyncrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/email/mbsyncrc";
  home.file.".msmtprc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/email/msmtprc";
}
