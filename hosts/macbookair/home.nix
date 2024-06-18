{ pkgs, outputs, ... }: {
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
    unstable.sketchybar-app-font
    fonts.sf-pro
  ];
  programs.kitty = {
    enable = true;
    settings = {
      font_size = 14;
      macos_titlebar_color = "#303446";
    };
  };
}
