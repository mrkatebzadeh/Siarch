{ config
, pkgs
, outputs
, ...
}:
let common = import ./common_pkgs.nix { inherit pkgs; };
in
{
  home.username = "siavash";
  home.homeDirectory = "/users/siavash";

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
    dosfstools
    fd
    clang
    highlight
    lf
    lynx
    man
    poppler
    sc-im
    killall
    taskspooler
    unrar
    unstable.neovim
    unzip
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
  ] ++
  common.packages;

  fonts.fontconfig.enable = true;

  home.file = { };
  home.sessionVariables = { };

  programs.home-manager.enable = true;
}
