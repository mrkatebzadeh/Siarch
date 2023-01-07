# Zsh-autosuggestion

[ -f "$HOME/.config/zsh/zplug" ] && source "$HOME/.config/zsh/zplug"

plug "esc/conda-zsh-completion"
plug "zsh-users/zsh-autosuggestions"
plug "hlissner/zsh-autopair"
plug "zap-zsh/supercharge"
plug "zap-zsh/vim"
plug "zap-zsh/zap-prompt"
plug "zap-zsh/fzf"
plug "zsh-users/zsh-syntax-highlighting"
bindkey '^ ' autosuggest-accept

export GPG_TTY=$(tty)
# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.cache/zsh/history
setopt appendhistory

_trap_exit() { tmux kill-session -t $$; }

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"


if command -v wal >/dev/null 2>&1 ; then
  (cat ~/.cache/wal/sequences &)
fi

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}

case "$OSTYPE" in
  linux*)
    . /usr/share/fzf/key-bindings.zsh
    . /usr/share/fzf/completion.zsh
      ;;
  darwin*);;
  *)        echo "unknown: $OSTYPE" ;;
esac

bindkey -s '^o' 'lfcd\n'

bindkey  '^a' beginning-of-line
bindkey  '^e' vi-end-of-line

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

bindkey '^[[P' delete-char


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/etc/profile.d/conda.sh" ]; then
        . "/usr/etc/profile.d/conda.sh"
    else
        export PATH="/usr/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

