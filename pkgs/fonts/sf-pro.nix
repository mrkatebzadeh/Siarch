{ fetchurl, pkgs, stdenv, ... }:
pkgs.stdenv.mkDerivation {
  pname = "sf-pro-font";
  version = "1.0";

  src = pkgs.fetchurl {
    url =
      "https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg";
    # sha256 = "0ycxhncd6if94w5n8cpdwr8rhrqdhgi913plbr2q8fia2266bk07";
    sha256 = "sha256-IccB0uWWfPCidHYX6sAusuEZX906dVYo8IaqeX7/O88=";
  };

  nativeBuildInputs = [ pkgs.p7zip ];

  unpackCmd = ''
    7z x $curSrc
    find . -name "*.pkg" -print -exec 7z x {} \;
    find . -name "Payload~" -print -exec 7z x {} \;
  '';

  sourceRoot = "./Library/Fonts";

  dontBuild = true;

  installPhase = ''
    find . -name '*.ttf' -exec install -m444 -Dt $out/share/fonts/truetype {} \;
    find . -name '*.otf' -exec install -m444 -Dt $out/share/fonts/opentype {} \;
  '';

  meta = with pkgs.lib; {
    homepage = "https://developer.apple.com/fonts/";
    description = "Apple fonts";
  };
}
