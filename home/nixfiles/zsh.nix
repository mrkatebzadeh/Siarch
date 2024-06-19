{ pkgs, ... }:
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
  initExtra = ''
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
        setopt HIST_EXPIRE_DUPS_FIRST
        setopt HIST_IGNORE_DUPS
        setopt HIST_IGNORE_ALL_DUPS
        setopt HIST_FIND_NO_DUPS
        setopt HIST_SAVE_NO_DUPS
        setopt HIST_REDUCE_BLANKS
        setopt HIST_VERIFY

        bindkey '^ ' autosuggest-accept
        bindkey -s '^o' 'yazi\n'
        bindkey  '^a' beginning-of-line
        bindkey  '^e' vi-end-of-line
        bindkey '^[[P' delete-char

        eval "$(starship init zsh)"
  '';
  dotDir = ".config/zsh";
}
