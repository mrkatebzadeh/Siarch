{ fetchurl, pkgs, stdenv, ... }:
pkgs.stdenv.mkDerivation {
  pname = "sf-arabic-font";
  version = "1.0";

  src = pkgs.fetchurl {
    url =
      "https://devimages-cdn.apple.com/design/resources/download/SF-Arabic.dmg";
    sha256 = "0jxx5ys2i0f8rvlyq7vckc8fjzkqs7l4nsyj8z6j4gcywskl3jfm";

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
