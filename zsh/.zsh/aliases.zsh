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
alias chat="weechat"
alias audio="ncpamixer"
alias calender="calcurse"
