{ pkgs ? import <nixpkgs> { }, ... }: {
  sf-pro = pkgs.callPackage ./sf-pro.nix { };
  sf-compact = pkgs.callPackage ./sf-compact.nix { };
  sf-arabic = pkgs.callPackage ./sf-arabic.nix { };
  sf-mono = pkgs.callPackage ./sf-mono.nix { };
  new-york = pkgs.callPackage ./new-york.nix { };
}
