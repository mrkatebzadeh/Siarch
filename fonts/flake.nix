{
  description = "Apple fonts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    {

      overlay = final: prev: {
        sf-mono-font = final.callPackage self.packages.sf-mono-font { };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        sources = {
          sf-compact = {
            url =
              "https://devimages-cdn.apple.com/design/resources/download/SF-Compact.dmg";
              sha256 = "0fkxm7nyl180qp8k2alynvy1w1a32zdm78dpq1zhv9q4gr1hp2ig";
          };

          sf-pro = {
            url =
              "https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg";
              sha256 = "0ycxhncd6if94w5n8cpdwr8rhrqdhgi913plbr2q8fia2266bk07";
          };
          sf-mono = {
            url =
              "https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg";
              sha256 = "0psf2fkqpwzw3hifciph7ypvjklb4jkcgh0mair0xvlf6baz3aji";
          };
          sf-arabic = {
            url =
              "https://devimages-cdn.apple.com/design/resources/download/SF-Arabic.dmg";
              sha256 = "0jxx5ys2i0f8rvlyq7vckc8fjzkqs7l4nsyj8z6j4gcywskl3jfm";
          };
          new-york = {
            url =
              "https://devimages-cdn.apple.com/design/resources/download/NY.dmg";
              sha256 = "1c5h9szggmwspba8gj06jlx30x83m9q6k9cdyg8dkivnij9am369";
          };
        };

      in rec {
        packages = flake-utils.lib.flattenTree (pkgs.lib.mapAttrs (name: value:

          pkgs.stdenv.mkDerivation {
            pname = "${name}-font";
            version = "1.0";

            src = pkgs.fetchurl {
              url = value.url;
              sha256 = value.sha256;
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
              # license = licenses.unfree;
              maintainers = [ maintainers.pinpox ];
            };
          }) sources);
        defaultPackage = packages.sf-mono;
      });
}
