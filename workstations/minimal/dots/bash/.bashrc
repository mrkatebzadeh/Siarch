stty -ixon
shopt -s autocd
HISTSIZE= HISTFILESIZE=

if [ "$EUID" -ne 0 ]
	then export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
	else export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]ROOT\[$(tput setaf 2)\]@\[$(tput setaf 4)\]$(hostname | awk '{print toupper($0)}') \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
fi

export GPG_TTY=$(tty)

set -o vi

export PATH="$(du $HOME/.scripts/ | cut -f2 | tr '\n' ':')$PATH"

alias progs="(pacman -Qet && pacman -Qm) | sort -u" 
alias orphans="pacman -Qdt" 
alias psref="gpg-connect-agent RELOADAGENT /bye" 
alias pacrepo='sudo reflector -l 20 -f 10 --save /etc/pacman.d/mirrorlist'
alias pacu='sudo pacman -Syu --noconfirm'
alias aur='yay -S --noconfirm'
alias vi='nvim'
alias rm='rm -ri'
alias :q='exit'
alias ref="source ~/.bashrc"
alias ls='ls -hN --color=auto --group-directories-first'
alias ll='ls -hNal --color=auto --group-directories-first'
alias grep="grep --color=auto"
alias ccat="highlight --out-format=ansi"
alias yt="youtube-dl --add-metadata -ic"
alias yta="youtube-dl --add-metadata -xic"
alias YT="youtube-viewer"
alias ethspeed="speedometer -r enp0s25"
alias wifispeed="speedometer -r wlp2s0"
alias starwars="telnet towel.blinkenlights.nl"

export QT_STYLE_OVERRIDE=gtk
export QT_SELECT=qt5

if [[ $LANG = '' ]]; then
	export LANG=en_GB.UTF-8
fi
