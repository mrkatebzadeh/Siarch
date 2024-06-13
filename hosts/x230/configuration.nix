{ config, pkgs, outputs, inputs, ... }:
let common = import ../common/pkgs.nix { inherit pkgs; };
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
    youtube-dl
    zathura
    zotero
  ] ++ [
    (nerdfonts.override {
      fonts = [ "FiraCode" "DroidSansMono" ];
    })
  ] ++
  common.packages ++ [ inputs.sf-fonts.packages."x86_64-linux".sf-pro ];

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

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
