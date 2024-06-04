{ config, pkgs, ... }:

{
  home.username = "siavash";
  home.homeDirectory = "/home/siavash";

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = with pkgs; [
    abook
    asdf-vm
    atool
    wofi
    bc
    brave
    btop
    curl
    #dmenu
    dosfstools
    dunst
    #dwm
    dwmblocks
    emacs
    exfat
    fd
    ffmpeg
    fontconfig
    fzf
    gawk
    clang
    git
    git-filter-repo
    gnome.gnome-keyring
    gpg-tui
    highlight
		i3
    htop
		waybar
    jadx
    kitty
    lazydocker
    lazygit
    less
    lf
    libnotify
    lsd
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
    nix-search-cli
    ntfs3g
    nushell
    poppler
    pulseaudio
    pulsemixer
    python311Packages.pynvim
    ripgrep
    sc-im
    simple-mtpfs
    slack
    slock
    st
    libertine
    killall
    starship
    stow
    stylua
    sxiv
    taskspooler
    telegram-desktop
    tree-sitter
    unclutter
    unrar
    unstable.neovim
    unzip
    vim
    wget
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
    zellij
    zsh
  noto-fonts
  noto-fonts-cjk
  noto-fonts-emoji
  liberation_ttf
  ];

    fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
