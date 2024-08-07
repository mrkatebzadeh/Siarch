{ config, pkgs, outputs, ... }:
let
  common = import ../common/pkgs.nix { inherit pkgs; };
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

  imports = [
    ./jobs/email.nix
  ];

  home.packages = with pkgs; [
    cinnamon.nemo
    firefox
    hyprland
    slurp
    grim
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
    exfat
    fd
    ffmpeg
    gnome.gnome-keyring
    gpg-tui
    highlight
    hyprpaper
    killall
    libnotify
    lynx
    maim
    man
    mediainfo
    mutt-wizard
    termusic
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
    wl-clipboard
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
    scripts.common
    scripts.hypr
    teams-for-linux
  ] ++
  common.packages;

  home.file = { };
  home.sessionVariables = { };

  fonts.fontconfig.enable = true;

  programs.home-manager.enable = true;
  programs.kitty = {
    enable = true;
    settings.font_size = 11;
    settings.background_opacity = "0.9";
    keybindings = {
      "alt+c" = "copy_to_clipboard";
      "alt+v" = "paste_from_clipboard";
    };
  };

  programs.thunderbird = {
    enable = true;
    profiles.hpz2 = {
      isDefault = true;
    };
  };

  home.pointerCursor = {
    name = "Catppuccin-Frappe-Dark-Cursors";
    package = pkgs.catppuccin-cursors.frappeDark;
  };

  gtk = import ../../home/nixfiles/gtk.nix { inherit config pkgs; };
  # xdg.configFile."bspwm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/bspwm";
  # xdg.configFile."deadd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/deadd";
  xdg.configFile."dunst".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/dunst";
  # xdg.configFile."flashfocus".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/flashfocus";
  # xdg.configFile."gtk-2.0".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gtk-2.0";
  # xdg.configFile."gtk-3.0".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gtk-3.0";
  xdg.configFile."hypr".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/hypr";
  # xdg.configFile."i3".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/i3";
  # xdg.configFile."ncmpcpp".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/ncmpcpp";
  # xdg.configFile."mpv".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/mpv";
  # xdg.configFile."mpd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/mpd";
  # xdg.configFile."picom".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/picom";
  # xdg.configFile."polybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/polybar";
  xdg.configFile."rofi".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/rofi";
  # xdg.configFile."sxkhd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/sxkhd";
  xdg.configFile."waybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/waybar";

}
