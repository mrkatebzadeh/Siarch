{ pkgs, ... }:
{
  font = {
    name = "FiraCode Nerd Font Mono";
  };

  settings = {
    background = "#303446";
    foreground = "#C6D0F5";
    color0 = "#51576D";
    color8 = "#626880";
    color1 = "#E78284";
    color9 = "#E78284";
    color2 = "#A6D189";
    color10 = "#A6D189";
    color3 = "#E5C890";
    color11 = "#E5C890";
    color4 = "#8CAAEE";
    color12 = "#8CAAEE";
    color5 = "#F4B8E4";
    color13 = "#F4B8E4";
    color6 = "#81C8BE";
    color14 = "#81C8BE";
    color7 = "#B5BFE2";
    color15 = "#A5ADCE";
    term = "xterm-256color";
    window_padding_width = 1;
    window_margin_width = 6;
    window_border_width = 1;
  };
  extraConfig = "
  symbol_map U+E5FA-U+E62B Symbols Nerd Font Mono
  # Devicons
  symbol_map U+E700-U+E7C5 Symbols Nerd Font Mono
  # Font Awesome
  symbol_map U+F000-U+F2E0 Symbols Nerd Font Mono
  # Font Awesome Extension
  symbol_map U+E200-U+E2A9 Symbols Nerd Font Mono
  # Material Design Icons
  symbol_map U+F500-U+FD46 Symbols Nerd Font Mono
  # Weather
  symbol_map U+E300-U+E3EB Symbols Nerd Font Mono
  # Octicons
  symbol_map U+F400-U+F4A8,U+2665,U+26A1,U+F27C Symbols Nerd Font Mono
  # Powerline Extra Symbols
  symbol_map U+E0A3,U+E0B4-U+E0C8,U+E0CC-U+E0D2,U+E0D4 Symbols Nerd Font Mono
  # IEC Power Symbols
  symbol_map U+23FB-U+23FE,U+2b58 Symbols Nerd Font Mono
  # Font Logos
  symbol_map U+F300-U+F313 Symbols Nerd Font Mono
  # Pomicons
  symbol_map U+E000-U+E00D Symbols Nerd Font Mono";
}
