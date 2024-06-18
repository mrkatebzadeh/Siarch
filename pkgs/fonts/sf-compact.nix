{ fetchurl, pkgs, stdenv, ... }:
pkgs.stdenv.mkDerivation {
  pname = "sf-compact-font";
  version = "1.0";

  src = pkgs.fetchurl {
    url =
      "https://devimages-cdn.apple.com/design/resources/download/SF-Compact.dmg";
    sha256 = "0fkxm7nyl180qp8k2alynvy1w1a32zdm78dpq1zhv9q4gr1hp2ig";

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
