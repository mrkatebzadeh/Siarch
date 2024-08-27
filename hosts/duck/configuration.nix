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

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
