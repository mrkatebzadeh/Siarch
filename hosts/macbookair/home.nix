{ config, pkgs, outputs, lib, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
  aspellWithDicts = pkgs.aspellWithDicts (d: [ d.en ]);
  emacs = pkgs.emacs30.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };

  emacs-patched = emacs.overrideAttrs (old: {
    patches =
      (old.patches or [ ])
      ++ [
        # Fix OS window role (needed for window managers like yabai)
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
          sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
        })
        # Use poll instead of select to get file descriptors
        /* (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
          sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
        }) */
        # Enable rounded window with no decoration
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
          sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
        })
        # Make Emacs aware of OS-level light/dark mode
        /* (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
          sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
        }) */
      ];
  });

  emacs-macport = pkgs.emacs-macport;


  emacs-with-packages = (pkgs.emacsPackagesFor emacs-patched).emacsWithPackages (epkgs: with epkgs; [
    epkgs.mu4e
    pkgs.mu
    vterm
    multi-vterm
    pdf-tools
    treesit-grammars.with-all-grammars
    jedi
    flymake-grammarly
  ]);


  isync-oauth2 = with pkgs; buildEnv {
    name = "isync-oauth2";
    paths = [ isync ];
    pathsToLink = [ "/bin" ];
    nativeBuildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram "$out/bin/mbsync" \
        --prefix SASL_PATH : "${cyrus_sasl}/lib/sasl2:${cyrus-sasl-xoauth2}/lib/sasl2"
    '';
  };
in
{

  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
      outputs.overlays.local
    ];
    config = {
      allowUnfree = true;
    };
  };


  home.packages = with pkgs; [
    aspellWithDicts
    ani-cli
    iina
    yt-dlp
    direnv
    nil
    pkg-config
    mu
    isync-oauth2
    msmtp
    pass
    meson
    unstable.sketchybar-app-font
    fonts.sf-pro
    scripts.common
    scripts.macos
    neovide
  ];

  programs.kitty = {
    enable = true;
    settings = {
      font_size = 14;
      macos_titlebar_color = "#303446";
    };
  };

  programs.emacs = {
    enable = true;
    package = emacs-with-packages;
    # package = pkgs.emacs30;
    extraConfig = ''(setq org-ditaa-jar-path "${pkgs.ditaa}/lib/ditaa.jar")'';
  };

  programs.starship = import ../../home/nixfiles/starship.nix {
    inherit pkgs lib;
  };
  programs.zsh = import ../../home/nixfiles/zsh.nix { inherit config pkgs; };

  # xdg.configFile."alacritty".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/alacritty";
  # xdg.configFile."helix".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/helix";
  xdg.configFile."borders".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/borders";
  xdg.configFile."sketchybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/sketchybar";
  xdg.configFile."skhd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/skhd";
  xdg.configFile."yabai".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/yabai";
  xdg.configFile."iterm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/iterm";
  xdg.configFile."aerospace".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/aerospace";
  home.file.".local/share/maildir".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/share/maildir";
  home.file.".mbsyncrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/email/mbsyncrc";
  home.file.".msmtprc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/email/msmtprc";
  home.file.".wezterm.lua".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/wezterm/wezterm.lua";
  home.file."Library/Application Support/nushell".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nushell";
}
