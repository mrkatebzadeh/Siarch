#!/bin/sh

# Profile file. Runs on login.

# variables and default programs:
# Adds `~/.scripts` and all subdirectories to $PATH
export PATH="$(du $HOME/.scripts/ | cut -f2 | tr '\n' ':')$PATH"
export EDITOR="nvim"
export TERMINAL="st bash"
# Link handler is set as the $BROWSER for use with urlscan.
# Set your real browser in $TRUEBROWSER.
export BROWSER="linkhandler"
export TRUEBROWSER="firefox"
export READER="zathura"
export BIB="$HOME/Dropbox/org/ref/master.bib"
export PIX="$HOME/.scripts/pix"

[ -f ~/.bashrc ] && source ~/.bashrc
setxkbmap -model pc104 -layout gb,ir -variant ,, -option grp:alt_shift_toggle
if [ "$(tty)" = "/dev/tty1" ]; then
	pgrep -x i3 || exec startx
fi

tty | grep tty >/dev/null && wal -Rns
