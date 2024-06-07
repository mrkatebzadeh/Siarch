{ config, pkgs, outputs, ... }:
let common = import ../common/pkgs.nix { inherit pkgs; };
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
    bc
    brave
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
    wofi
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
  ] ++ common.packages;

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
