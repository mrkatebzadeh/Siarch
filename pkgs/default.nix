{ pkgs ? import <nixpkgs> { }, lib, ... }: {
  fonts = pkgs.callPackage ./fonts { };
  scripts = pkgs.callPackage ./scripts { };
}
