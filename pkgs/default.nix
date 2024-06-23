{ pkgs ? import <nixpkgs> { }, ... }: {
  fonts = pkgs.callPackage ./fonts { };
  scripts = pkgs.callPackage ./scripts { };
}
