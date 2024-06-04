# Zsh-autosuggestion

# [ -f "$HOME/.config/zsh/zplug" ] && source "$HOME/.config/zsh/zplug"

[ -f "$HOME/.config/zsh/plugs" ] && source "$HOME/.config/zsh/plugs"

[ -f "$HOME/.config/zsh/func" ] && source "$HOME/.config/zsh/func"

[ -f "$HOME/.config/zsh/bind" ] && source "$HOME/.config/zsh/bind"

[ -f "$HOME/.config/shell/aliasrc" ] && source "$HOME/.config/shell/aliasrc"

export GPG_TTY=$(tty)
# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=$HOME/.zsh_history
setopt appendhistory

autoload -Uz compinit
# zstyle ':completion:*' menu yes select
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zmodload zsh/complist
_comp_options+=(globdots)		# Include hidden files.
zle_highlight=('paste:none')
for dump in "${ZDOTDIR:-$HOME}/.zcompdump"(N.mh+24); do
  compinit
done
compinit -C

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

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Colors
autoload -Uz colors && colors

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  SESSION_TYPE=remote/ssh
# many other tests omitted
else
  case $(ps -o comm= -p "$PPID") in
    sshd|*/sshd) SESSION_TYPE=remote/ssh;;
  esac
fi
if [ -z "$SESSION_TYPE" ]; then
if command -v wal >/dev/null 2>&1 ; then
  (cat ~/.cache/wal/sequences &)
fi
fi
