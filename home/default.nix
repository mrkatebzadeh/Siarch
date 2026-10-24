{ config, pkgs, lib, inputs, ... }:
let
  siarch = "${config.home.homeDirectory}/.siarch";
  dotfiles = "${siarch}/home/dotfiles";
in
{
  imports = [ inputs.nhx.homeManagerModules.default ];

  home.stateVersion = "22.11";

  # Detach the old directory link before installing files inside it.
  home.activation.migrateHelixDirectory = lib.hm.dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
    helixDir=${lib.escapeShellArg "${config.xdg.configHome}/helix"}
    legacyDir=${lib.escapeShellArg "${dotfiles}/helix"}
    if [[ -L "$helixDir" ]]; then
      resolved="$(${pkgs.coreutils}/bin/readlink -e -- "$helixDir")" || exit 1
      expected="$(${pkgs.coreutils}/bin/readlink -e -- "$legacyDir")" || exit 1
      if [[ ! -d "$legacyDir" || "$resolved" != "$expected" ]]; then
        printf '%s\n' "Refusing migration of unexpected Helix symlink: $helixDir" >&2
        exit 1
      fi
      backup="$helixDir.pre-nhx"
      if [[ -e "$backup" || -L "$backup" ]]; then
        printf '%s\n' "Refusing to overwrite Helix backup: $backup" >&2
        exit 1
      fi
      run ${pkgs.coreutils}/bin/mv -T --update=none-fail -- "$helixDir" "$backup" || exit 1
    fi
  '';

  home.sessionVariables = import ./nixfiles/variables.nix { inherit pkgs; };

  fonts.fontconfig = import ./nixfiles/fonts.nix { inherit pkgs; };
  home.packages = with pkgs; [
    nerd-fonts.fira-code
    iosevka
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
  programs.helix = {
    enable = true;
    package = pkgs.steelix;
  };
  programs.nhx = {
    enable = true;
    package = null;
    steel = {
      enable = true;
      lsp.enable = false;
    };
    plugins.forest = {
      enable = true;
      config = {
        style = "snacks";
        position = "left";
        circularKeybinds = true;
      };
      extra = ''
        (define (forest-toggle)
          (if (forest-snacks-active?) (forest-close) (forest-open)))
        (provide forest-toggle)
      '';
    };
    plugins.vim = {
      enable = true;
      package = config.programs.nhx.availablePlugins.vim;
      requirePath = "vim-hx/init.scm";
      extra = "(set-vim-keybindings!)";
    };
  };
  programs.kitty = import ./nixfiles/kitty.nix { inherit pkgs; };

  home.file.".profile".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/profile";

  # xdg.configFile."alacritty".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/alacritty";
  xdg.configFile."gdb".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gdb";
  home.file.".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/git/.gitconfig";
  xdg.configFile."htop".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/htop";
  xdg.configFile."lazygit".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lazygit";
  xdg.configFile."lf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lf";
  xdg.configFile."neofetch".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/neofetch";
  # xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/nvim.lua";
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
  # xdg.configFile."emacs".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/emacs.d";
}
