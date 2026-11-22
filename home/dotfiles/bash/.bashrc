# Interactive shells only.
case $- in
  *i*) ;;
  *) return ;;
esac

# [ -r "$HOME/.config/shell/aliasrc" ] && . "$HOME/.config/shell/aliasrc"

if command -v starship >/dev/null 2>&1; then
  eval "$(starship init bash)"
else
  PS1='\[\e[1;32m\]\u@\h \[\e[1;34m\]\w\[\e[0m\] \$ '
fi
