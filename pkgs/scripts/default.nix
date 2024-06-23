{ pkgs ? import <nixpkgs> { }, ... }: {
  hypr = pkgs.callPackage ./hypr { };
  macos = pkgs.callPackage ./macos { };
}
