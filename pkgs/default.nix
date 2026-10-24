{ pkgs ? import <nixpkgs> { }, ... }: {
  fonts = pkgs.callPackage ./fonts { };
  siarch-scripts = pkgs.callPackage ./scripts { };
}
