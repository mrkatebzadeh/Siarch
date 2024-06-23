{ pkgs ? import <nixpkgs> { }, ... }: {
  hypr = pkgs.callPackage ./hypr { };
  common = pkgs.callPackage ./common { };
  macos = pkgs.callPackage ./macos { };
}
