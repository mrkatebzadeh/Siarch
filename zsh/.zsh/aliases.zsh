alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn} --exclude="*.pyc"'
alias h=history
alias rm='rm -i'
alias nano='nano -c'
alias top=htop
alias emacs=emacs -nw
alias ll='ls -al'
alias vi='vim'
alias mux='tmuxinator'
alias st='st -f "DroidSansMono Nerd Font:size=11"'

# suffix aliases http://zshwiki.org/home/examples/aliassuffix
alias -s log=nano
alias -s conf=nano
. "${HOME}/.cache/wal/colors.sh"

# Create the alias.
alias dmen='dmenu_run -nb "$color0" -nf "$color15" -sb "$color1" -sf "$color15"'

alias youtube="mpsyt"
alias music="ncmpcpp"
alias clock="ncmpcpp -s clock"
alias visualizer="ncmpcpp -s visualizer"
alias news="newsbeuter"
alias email="neomutt"
alias files="ranger"
alias edinburgh="weather edinburgh"
alias shiraz="weather shiraz"
alias audio="ncpamixer"
alias calender="calcurse"

# Audio and Music
alias mute="pamixer -m"
alias unmute="pamixer -u"
alias vu="pamixer --allow-boost -i 5"
alias vd="pamixer --allow-boost -d 5"
alias pause="mpc toggle"
alias next="mpc next"
alias prev="mpc prev"
alias trupause="mpc pause"
alias beg="mpc seek 0%"
alias lilbak="mpc seek -10"
alias lilfor="mpc seek +10"
alias bigbak="mpc seek -120"
alias bigfor="mpc seek +120"

alias talkingthrones="ytvideo 'talking thrones'"
alias lukesmith="ytvideo 'luke smith'"


