{ config, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
in
{
  home.stateVersion = "22.11";
  # home.packages = with pkgs; [
  # ];
  home.sessionVariables = {
    PAGER = "less";
    CLICLOLOR = 1;
    EDITOR = "nvim";
  };
  home.file.".profile".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/profile";
  # home.file.".zprofile".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zprofile";
  # home.file.".zshrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zshrc";

  xdg.configFile."alacritty".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/alacritty";
  xdg.configFile."borders".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/borders";
  xdg.configFile."bspwm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/bspwm";
  xdg.configFile."btop".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/btop";
  xdg.configFile."deadd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/deadd";
  xdg.configFile."flashfocus".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/flashfocus";
  # xdg.configFile."fontconfig".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/fontconfig";
  xdg.configFile."gdb".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gdb";
  home.file.".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/git/.gitconfig";
  xdg.configFile."gtk-2.0".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gtk-2.0";
  xdg.configFile."gtk-3.0".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gtk-3.0";
  xdg.configFile."htop".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/htop";
  xdg.configFile."hypr".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/hypr";
  xdg.configFile."i3".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/i3";
  xdg.configFile."iterm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/iterm";
  xdg.configFile."kitty".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/kitty";
  xdg.configFile."lazygit".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lazygit";
  xdg.configFile."lf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lf";
  xdg.configFile."mpd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/mpd";
  xdg.configFile."mpv".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/mpv";
  xdg.configFile."ncmpcpp".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/ncmpcpp";
  xdg.configFile."neofetch".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/neofetch";
  xdg.configFile."newsboat".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/newsboat";
 # xdg.configFile."nix".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nix";
  xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nvim";
  xdg.configFile."nushell".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nushell";
  xdg.configFile."picom".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/picom";
  xdg.configFile."polybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/polybar";
  home.file.".local/bin/scripts".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/scripts";
  # home.file.".local/share".source = "${dotfiles}/share";
  xdg.configFile."shell".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/shell";
  xdg.configFile."sketchybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/sketchybar";
  xdg.configFile."skhd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/skhd";
  xdg.configFile."sxkhd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/sxkhd";
  xdg.configFile."starship.toml".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/starship/starship.toml";
  xdg.configFile."tmux".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/tmux";
  xdg.configFile."vim".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/vim";
  xdg.configFile."yabai".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/yabai";
  xdg.configFile."wget".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/wget";
  xdg.configFile."waybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/waybar";
  xdg.configFile."zellij".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zellij";
  xdg.configFile."zsh".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zsh";
}
