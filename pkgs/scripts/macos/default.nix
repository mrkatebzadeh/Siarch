{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "macos";
  version = "1.0.0";

  src = ./src;

  installPhase = ''
    mkdir -p $out/bin
    cp * $out/bin/
  '';

  meta = with pkgs.lib; {
    description = "My scripts for MacOS";
    license = licenses.mit;
    maintainers = [ maintainers.mrkatebzadeh ];
  };
}
