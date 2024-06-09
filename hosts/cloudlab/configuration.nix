{ config
, pkgs
, outputs
, ...
}:
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
    nixpkgs-fmt
    clang-tools
    nodePackages.bash-language-server
    cmake-language-server
    vscode-extensions.vadimcn.vscode-lldb
    cpplint
    shellcheck
    shfmt
    atuin
    btop
    coreutils
    curl
    lazygit
    lsd
    meslo-lgs-nf
    ripgrep
    starship
    tree-sitter
    zellij
    bc
    cargo
    clang
    cmake
    dosfstools
    fd
    flamegraph
    highlight
    killall
    unstable.neovim
    unzip
  ];

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
