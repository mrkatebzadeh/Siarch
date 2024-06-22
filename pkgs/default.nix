{ pkgs ? import <nixpkgs> { }, ... }: {
  fonts = pkgs.callPackage ./fonts { };
  hyprscripts = pkgs.callPackage ./hyprscripts { };
}
