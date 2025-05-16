{ config
, pkgs
, outputs
, ...
}:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";

  emacs = pkgs.emacs.override {
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
    ];
    config = {
      allowUnfree = true;
    };
  };

  home.packages = with pkgs; [
    clang-tools
    atuin
    pciutils
    btop
    coreutils
    curl
    lazygit
    lsd
    ripgrep
    starship
    tree-sitter
    zellij
    rustup
    bc
    cmake
    patchelf
    dosfstools
    fd
    flamegraph
    highlight
    killall
    unstable.neovim
    unzip
  ];

  programs.emacs = {
    enable = true;
    package = emacs-with-packages;
  };


  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;

  xdg.configFile."emacs".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/emacs.d";
  xdg.configFile."helix".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/helix";
}
