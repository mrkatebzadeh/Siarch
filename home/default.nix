{ config, pkgs, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
in
{
  home.stateVersion = "22.11";

  home.sessionVariables = {
    PAGER = "less";
    CLICLOLOR = 1;
    EDITOR = "nvim";
  };

  programs.atuin = import ./nixfiles/atuin.nix { inherit pkgs; };
  programs.bat = import ./nixfiles/bat.nix { inherit pkgs; };
  programs.kitty = import ./nixfiles/kitty.nix { inherit pkgs; };
  programs.zsh = import ./nixfiles/zsh.nix { inherit pkgs; };

  home.file.".profile".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/profile";
  home.file.".zprofile".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zsh/.zprofile";
  home.file.".zshrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zsh/.zshrc";

  xdg.configFile."alacritty".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/alacritty";
  xdg.configFile."borders".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/borders";
  xdg.configFile."bspwm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/bspwm";
  xdg.configFile."btop".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/btop";
  xdg.configFile."deadd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/deadd";
  xdg.configFile."dunst".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/dunst";
  xdg.configFile."flashfocus".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/flashfocus";
  xdg.configFile."gdb".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gdb";
  home.file.".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/git/.gitconfig";
  xdg.configFile."gtk-2.0".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gtk-2.0";
  xdg.configFile."gtk-3.0".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gtk-3.0";
  xdg.configFile."htop".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/htop";
  xdg.configFile."hypr".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/hypr";
  xdg.configFile."i3".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/i3";
  xdg.configFile."iterm".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/iterm";
  xdg.configFile."lazygit".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lazygit";
  xdg.configFile."lf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lf";
  xdg.configFile."mpd".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/mpd";
  xdg.configFile."mpv".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/mpv";
  xdg.configFile."ncmpcpp".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/ncmpcpp";
  xdg.configFile."neofetch".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/neofetch";
  xdg.configFile."newsboat".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/newsboat";
  xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nvim";
  xdg.configFile."nushell".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nushell";
  xdg.configFile."picom".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/picom";
  xdg.configFile."polybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/polybar";
  xdg.configFile."rofi".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/rofi";
  home.file.".local/bin/scripts".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/scripts";
  home.file.".local/share/backgrounds".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/share/backgrounds";
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
