{ pkgs, ... }:
{
  enable = true;
  config = {
    theme = "catppuccin_frappe";
    pager = "less -FR";
  };
  themes = {
    dracula = {
      src = pkgs.fetchFromGitHub {
        owner = "dracula";
        repo = "sublime";
        rev = "26c57ec282abcaa76e57e055f38432bd827ac34e";
        sha256 = "019hfl4zbn4vm4154hh3bwk6hm7bdxbr1hdww83nabxwjn99ndhv";
      };
      file = "Dracula.tmTheme";
    };
    catppuccin_frappe = {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "d714cc1d358ea51bfc02550dabab693f70cccea0";
        sha256 = "1zlryg39y4dbrycjlp009vd7hx2yvn5zfb03a2vq426z78s7i423";
      };
      file = "themes/Catppuccin Frappe.tmTheme";
    };
  };
}
