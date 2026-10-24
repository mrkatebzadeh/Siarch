{ fetchurl, pkgs, stdenv, ... }:
pkgs.stdenv.mkDerivation {
  pname = "sf-pro-font";
  version = "1.0";

  src = pkgs.fetchurl {
    url =
      "https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg";
      sha256 = "sha256-loqzuLH5LC2K9h6waA9cIiTE541ZuYa/AEUCp/wBKRg=";
  };

  nativeBuildInputs = [ pkgs._7zz pkgs.libarchive ];

  unpackCmd = ''
    mkdir -p sf-pro-unpack/{dmg,pkg,fonts} || exit 1
    7zz e "$curSrc" -osf-pro-unpack/dmg '*.pkg' '*/*.pkg' -y || exit 1
    find sf-pro-unpack/dmg -type f -name '*.pkg' -print0 > sf-pro-unpack/packages || exit 1
    test -s sf-pro-unpack/packages || exit 1
    while IFS= read -r -d "" package; do
      bsdtar -xf "$package" -C sf-pro-unpack/pkg || exit 1
    done < sf-pro-unpack/packages
    find sf-pro-unpack/pkg -type f -name Payload -print0 > sf-pro-unpack/payloads || exit 1
    test -s sf-pro-unpack/payloads || exit 1
    while IFS= read -r -d "" payload; do
      bsdtar -xf "$payload" -C sf-pro-unpack/fonts || exit 1
    done < sf-pro-unpack/payloads
  '';

  sourceRoot = "sf-pro-unpack/fonts";

  dontBuild = true;

  installPhase = ''
    find . -type f -name '*.ttf' -exec install -m444 -Dt "$out/share/fonts/truetype" {} + || exit 1
    find . -type f -name '*.otf' -exec install -m444 -Dt "$out/share/fonts/opentype" {} + || exit 1
    test -n "$(find "$out/share/fonts" -type f \( -name '*.ttf' -o -name '*.otf' \) -print -quit)" || exit 1
  '';

  meta = with pkgs.lib; {
    homepage = "https://developer.apple.com/fonts/";
    description = "Apple fonts";
  };
}
