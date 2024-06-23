{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "common";
  version = "1.0.0";

  src = ./src;

  installPhase = ''
    mkdir -p $out/bin
    cp * $out/bin/
  '';

  meta = with pkgs.lib; {
    description = "My common (platform independent) scripts";
    license = licenses.mit;
    maintainers = [ maintainers.mrkatebzadeh ];
  };
}
