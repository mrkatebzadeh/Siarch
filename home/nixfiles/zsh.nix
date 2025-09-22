{ config, pkgs, ... }:
{
  enable = true;
  enableCompletion = true;
  autosuggestion.enable = true;
  syntaxHighlighting.enable = true;
  syntaxHighlighting.highlighters = [
    "main"
    "brackets"
    "pattern"
    "cursor"
    "regexp"
    "root"
    "line"
  ];
  history = {
    expireDuplicatesFirst = true;
    ignoreDups = true;
    ignoreSpace = true;
    extended = true;
    path = "${config.xdg.dataHome}/zsh/history";
    share = false;
    size = 100000;
    save = 100000;
  };
  initContent = ''
    unsetopt BEEP
    setopt AUTO_CD
    setopt GLOB_DOTS
    setopt NOMATCH
    setopt MENU_COMPLETE
    setopt EXTENDED_GLOB
    setopt INTERACTIVE_COMMENTS
    setopt APPEND_HISTORY
    setopt BANG_HIST
    setopt EXTENDED_HISTORY
    setopt HIST_VERIFY

    bindkey '^ ' autosuggest-accept
    bindkey -s '^o' 'yazi\n'
    bindkey  '^a' beginning-of-line
    bindkey  '^e' vi-end-of-line
    bindkey '^[[P' delete-char

    # eval "$(starship init zsh)"
  '';
  dotDir = ".config/zsh";
}
