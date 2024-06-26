#!/bin/zsh

if [ -r $HOME/.nix-profile/bin  ]; then
    export PATH=$PATH:$HOME/.nix-profile/bin
fi

# profile file. Runs on login. Environmental variables are set here.
ZSHRC=~/.config/zsh/zshrc
if [ -r $ZSHRC  ]; then
    source $ZSHRC
fi

# If you don't plan on reverting to bash, you can remove the link in ~/.profile
# to clean up.

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export TERMINAL="terminal"
[ -f $HOME/.config/shell/userrc ] && . $HOME/.config/shell/userrc

# Adds `~/.local/bin` to $PATH
export PATH=$PATH:$HOME/.local/bin:$HOME/.local/bin/scripts
case "$OSTYPE" in
  linux*)
    export SUDO_ASKPASS="$HOME/.local/bin/dmenupass"
    export PATH="$PATH:${$(find ~/.local/bin/ -type d -printf %p:)%%:}"
    export PATH=$PATH:~/.local/bin/dwm_scripts
    export PATH=$PATH:~/.local/bin/sb_scripts
    export PATH=$PATH:~/.local/bin/i3_scripts
    export PATH=$PATH:~/.local/bin/bsp_scripts
    export PATH=$PATH:~/.local/bin/hypr_scripts
    export PATH=$PATH:~/.local/bin/hyprland_scripts
    export PATH="$PATH:/opt/omnetpp/bin/"
    export PATH="$PATH:/opt/idea/bin/"
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/omnetpp/lib
    [ -f $HOME/.config/shell/exportsrc ] && . $HOME/.config/shell/exportsrc
      ;;
  darwin*)
    # llvm path
    # libc++ LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
    export HOMEBREW_PREFIX=/opt/homebrew
    export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib/c++ -Wl,-rpath,$HOMEBREW_PREFIX/opt/llvm/lib/c++"
    export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib"
    export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/llvm/include"
    [ -f /opt/homebrew/opt/asdf/libexec/asdf.sh ] && source /opt/homebrew/opt/asdf/libexec/asdf.sh
    [ -f /usr/local/opt/asdf/libexec/asdf.sh ] && source /usr/local/opt/asdf/libexec/asdf.sh
    export PATH="/usr/local/opt/gawk/libexec/gnubin:$PATH"
    # export PATH="/usr/local/opt/llvm/bin:$PATH"
    # export LDFLAGS="-L/usr/local/opt/llvm/lib"
    # export CPPFLAGS="-I/usr/local/opt/llvm/include"
    [ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
      ;;
  *)        echo "unknown: $OSTYPE" ;;
esac
unsetopt PROMPT_SP

export PATH="$PATH:/opt/flutter/bin/"
# Default programs:
export EDITOR="nvim"
export BROWSER="brave"

# ~/ Clean-up:
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
#export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc"
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.
export NOTMUCH_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/notmuch-config"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
export LESSHISTFILE="-"
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
export INPUTRC="${XDG_CONFIG_HOME:-$HOME/.config}/shell/inputrc"
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
#export ALSA_CONFIG_PATH="$XDG_CONFIG_HOME/alsa/asoundrc"
#export GNUPGHOME="${XDG_DATA_HOME:-$HOME/.local/share}/gnupg"
export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export KODI_DATA="${XDG_DATA_HOME:-$HOME/.local/share}/kodi"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/password-store"
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
export ANDROID_SDK_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/android"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export ANSIBLE_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/ansible/ansible.cfg"
export UNISON="${XDG_DATA_HOME:-$HOME/.local/share}/unison"
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/history"
export WEECHAT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/weechat"

# Other program settings:
export DICS="/usr/share/stardict/dic/"
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"
export LESS=-R
export LESS_TERMCAP_mb="$(printf '%b' '[1;31m')"
export LESS_TERMCAP_md="$(printf '%b' '[1;36m')"
export LESS_TERMCAP_me="$(printf '%b' '[0m')"
export LESS_TERMCAP_so="$(printf '%b' '[01;44;33m')"
export LESS_TERMCAP_se="$(printf '%b' '[0m')"
export LESS_TERMCAP_us="$(printf '%b' '[1;32m')"
export LESS_TERMCAP_ue="$(printf '%b' '[0m')"
export LESSOPEN="| /usr/bin/highlight -O ansi %s 2>/dev/null"
export QT_QPA_PLATFORMTHEME="gtk2"	# Have QT use gtk2 theme.
export MOZ_USE_XINPUT2="1"		# Mozilla smooth scrolling/touchpads.
export AWT_TOOLKIT="MToolkit wmname LG3D"	#May have to install wmname
export _JAVA_AWT_WM_NONREPARENTING=1	# Fix for Java applications in dwm

# Siavash
export BIBTEXERPATH=~/Dropbox/Papers
export BIBTEXERFILE="$BIBTEXERPATH"/references.bib
export BIBTEXERCACHE=~/.cache/bibtexer
export BOOKPATH=~/Dropbox/Books
export CHROME_EXECUTABLE=$(which brave)

NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$PATH:$NPM_PACKAGES/bin"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

# This is the list for lf icons:
export LF_ICONS="di=📁:\
fi=📃:\
tw=🤝:\
ow=📂:\
ln=⛓:\
or=❌:\
ex=🎯:\
*.txt=✍:\
*.mom=✍:\
*.me=✍:\
*.ms=✍:\
*.png=🖼:\
*.webp=🖼:\
*.ico=🖼:\
*.jpg=📸:\
*.jpe=📸:\
*.jpeg=📸:\
*.gif=🖼:\
*.svg=🗺:\
*.tif=🖼:\
*.tiff=🖼:\
*.xcf=🖌:\
*.html=🌎:\
*.xml=📰:\
*.gpg=🔒:\
*.css=🎨:\
*.pdf=📚:\
*.djvu=📚:\
*.epub=📚:\
*.csv=📓:\
*.xlsx=📓:\
*.tex=📜:\
*.md=📘:\
*.r=📊:\
*.R=📊:\
*.rmd=📊:\
*.Rmd=📊:\
*.m=📊:\
*.mp3=🎵:\
*.opus=🎵:\
*.ogg=🎵:\
*.m4a=🎵:\
*.flac=🎼:\
*.wav=🎼:\
*.mkv=🎥:\
*.mp4=🎥:\
*.webm=🎥:\
*.mpeg=🎥:\
*.avi=🎥:\
*.mov=🎥:\
*.mpg=🎥:\
*.wmv=🎥:\
*.m4b=🎥:\
*.flv=🎥:\
*.zip=📦:\
*.rar=📦:\
*.7z=📦:\
*.tar.gz=📦:\
*.z64=🎮:\
*.v64=🎮:\
*.n64=🎮:\
*.gba=🎮:\
*.nes=🎮:\
*.gdi=🎮:\
*.1=ℹ:\
*.nfo=ℹ:\
*.info=ℹ:\
*.log=📙:\
*.iso=📀:\
*.img=📀:\
*.bib=🎓:\
*.ged=👪:\
*.part=💔:\
*.torrent=🔽:\
*.jar=♨:\
*.java=♨:\
"

[ -f $CARGO_HOME/env ] && source $CARGO_HOME/env

case "$OSTYPE" in
  linux*)
    # sudo -n loadkeys ${XDG_DATA_HOME:-$HOME/.local/share}/siarch/ttymaps.kmap 2>/dev/null
    ;;
  darwin*)  ;;
  *)        echo "unknown: $OSTYPE" ;;
esac
