{ config, pkgs, lib, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
in
{
  home.stateVersion = "22.11";

  home.sessionVariables = import ./nixfiles/variables.nix;

  fonts.fontconfig = import ./nixfiles/fonts.nix { inherit pkgs; };
  home.packages = with pkgs; [
    fira-code-nerdfont
  ];

  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.local/bin/scripts"
    "/run/current-system/sw/bin"
    "/opt/homebrew/bin"
  ];
  home.shellAliases = import ./nixfiles/aliases.nix;
  programs.atuin = import ./nixfiles/atuin.nix { inherit pkgs; };
  programs.btop = import ./nixfiles/btop.nix {
    inherit pkgs;
    home = "${config.home.homeDirectory}";
  };
  programs.bat = import ./nixfiles/bat.nix { inherit pkgs; };
  programs.kitty = import ./nixfiles/kitty.nix { inherit pkgs; };

  home.file.".profile".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/profile";

  xdg.configFile."alacritty".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/alacritty";
  xdg.configFile."gdb".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gdb";
  home.file.".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/git/.gitconfig";
  xdg.configFile."htop".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/htop";
  xdg.configFile."lazygit".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lazygit";
  xdg.configFile."lf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lf";
  xdg.configFile."neofetch".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/neofetch";
  xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nvim.lua";
  xdg.configFile."newsboat".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/newsboat";
  xdg.configFile."nushell".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nushell";
  home.file.".local/bin/scripts".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/scripts";
  home.file.".local/share/backgrounds".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/share/backgrounds";
  home.file.".local/share/siarch".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/share/siarch";
  xdg.configFile."shell".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/shell";
  xdg.configFile."tmux".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/tmux";
  xdg.configFile."vim".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/vim";
  xdg.configFile."wget".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/wget";
  xdg.configFile."zellij".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/zellij";
  xdg.configFile."emacs".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/emacs.d";
}
