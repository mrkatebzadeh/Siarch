{ config, pkgs, outputs, ... }:
let common = import ./common_pkgs.nix { inherit pkgs; };
in
{
  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  home.packages = with pkgs; [
    bc
    brave
    cargo
    clang
    dmenu
    dosfstools
    dunst
    dwm
    dwmblocks
    exfat
    fd
    ffmpeg
    gnome.gnome-keyring
    gpg-tui
    highlight
    i3
    killall
    lf
    liberation_ttf
    libnotify
    lynx
    maim
    man
    mediainfo
    mpc-cli
    mpd
    mpv
    mutt-wizard
    ncmpcpp
    newsboat
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    ntfs3g
    poppler
    pulseaudio
    pulsemixer
    rustc
    sc-im
    simple-mtpfs
    slock
    st
    taskspooler
    unclutter
    unrar
    unstable.neovim
    unzip
    waybar
    rofi-wayland
    xcape
    xdotool
    xorg.xbacklight
    xorg.xdpyinfo
    xorg.xinit
    xorg.xorgserver
    xorg.xprop
    xorg.xwininfo
    xwallpaper
    youtube-dl
    zathura
  ] ++
  common.packages;

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
  programs.kitty = {
    enable = true;
    settings.font_size = 11;
  };
}
