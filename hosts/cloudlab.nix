{ config
, pkgs
, outputs
, ...
}:
let common = import ./common_pkgs.nix { inherit pkgs; };
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
    bc
    cargo
    clang
    dosfstools
    fd
    highlight
    killall
    lf
    liberation_ttf
    lynx
    man
    nodejs
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    poppler
    rustc
    sc-im
    taskspooler
    unrar
    unstable.neovim
    unzip
  ] ++
  common.packages;

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };


  programs.home-manager.enable = true;
}