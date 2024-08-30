{ config, pkgs, outputs, lib, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";

  emacs = pkgs.emacs-macport.override {
    # withXwidgets = true;
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };

  emacs-with-packages = (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: with epkgs; [
    epkgs.mu4e
    pkgs.mu
    vterm
    multi-vterm
    pdf-tools
    treesit-grammars.with-all-grammars
  ]);
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
    direnv
    nil
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
    package = emacs-with-packages;
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
