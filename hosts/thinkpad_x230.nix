{ config, pkgs, outputs, ... }:
let common = import ./common_pkgs.nix { inherit pkgs; };
in
{
  home.username = "siavash";
  home.homeDirectory = "/home/siavash";

  home.stateVersion = "22.11";

  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  home.packages = with pkgs; [
    wofi
    bc
    brave
    dmenu
    dosfstools
    dunst
    dwm
    dwmblocks
    exfat
    fd
    ffmpeg
    clang
    gnome.gnome-keyring
    gpg-tui
    highlight
    i3
    waybar
    lf
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
    ntfs3g
    poppler
    pulseaudio
    pulsemixer
    sc-im
    simple-mtpfs
    slock
    st
    killall
    taskspooler
    unclutter
    unrar
    unstable.neovim
    unzip
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
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
  ] ++
  common.packages;

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
