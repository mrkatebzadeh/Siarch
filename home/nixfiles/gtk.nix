{ pkgs, ... }:
{
  enable = true;
  theme = {
    name = "Catppuccin-Frappe-Standard-Blue-Dark";
    package = pkgs.catppuccin-gtk;
  };
}
