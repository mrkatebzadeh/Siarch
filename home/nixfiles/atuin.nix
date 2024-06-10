{ pkgs, ... }:
{
  enable = true;
  settings = {
    enter_accept = false;
    prefers_reduced_motion = true;
    filter_mode = "host";
    invert = false;
    inline_height = 10;
    style = "compact";
    show_help = false;
  };
}
