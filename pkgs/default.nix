{ pkgs ? import <nixpkgs> { }, ... }: {
  fonts = pkgs.callPackage ./fonts { };
}
