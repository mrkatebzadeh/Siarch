{ config, pkgs, outputs, ... }:
let common = import ../common/pkgs.nix { inherit pkgs; };
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
    vscode-extensions.ms-vscode.cpptools
    bc
    bluetuith
    bluez
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
    hyprpaper
    i3
    killall
    libnotify
    lynx
    maim
    man
    mediainfo
    mpc-cli
    mpd
    mpv
    mutt-wizard
    termusic
    newsboat
    ntfs3g
    poppler
    pulseaudio
    pulsemixer
    rofi-wayland
    rust-analyzer
    rustfmt
    rustc
    sc-im
    simple-mtpfs
    slock
    swaylock
    st
    taskspooler
    unclutter
    unrar
    unstable.neovim
    unzip
    waybar
    xcape
    xdotool
    xorg.xbacklight
    xorg.xdpyinfo
    xorg.xinit
    xorg.xorgserver
    xorg.xprop
    xorg.xwininfo
    xwallpaper
    yt-dlp
    zathura
    zotero
    neomutt
    isync
    msmtp
    pass
    cacert
    gettext
    mutt-wizard
    telegram-desktop
    slack
    fonts.sf-pro
  ] ++
  common.packages;

  home.file = { };
  home.sessionVariables = { };

  fonts.fontconfig.enable = true;

  programs.home-manager.enable = true;
  programs.kitty = {
    enable = true;
    settings.font_size = 11;
    settings.background_opacity = "0.8";
    keybindings = {
      "alt+c" = "copy_to_clipboard";
      "alt+v" = "paste_from_clipboard";
    };
  };

  programs.thunderbird = {
    enable = true;
    profiles.x230 = {
      isDefault = true;
    };
  };
}
