{ config, pkgs, ... }:

{
  home.username = "siavash";
  home.homeDirectory = "/home/siavash";

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = with pkgs; [

    asdf-vm
    btop
    curl
    emacs
    fd
    fontconfig
    fzf
    gawk
    git
    git-filter-repo
    gpg-tui
    htop
    jadx
    kitty
    lazydocker
    lazygit
    less
    lsd
    nix-search-cli
    nushell
    python311Packages.pynvim
    ripgrep
    slack
    starship
    stow
    stylua
    telegram-desktop
    tree-sitter
    unstable.neovim
    vim
    wget
    zellij
  ];

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
