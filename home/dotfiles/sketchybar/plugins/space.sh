#!/bin/bash


update() {
  if [ "$SENDER" = "space_change" ]; then
    source "$CONFIG_DIR/colors.sh"
    COLOR=$BACKGROUND_2
    if [ "$SELECTED" = "true" ]; then
      COLOR=$GREY
    fi

    sketchybar --set space.$(aerospace list-workspaces --focused) icon.highlight=true \
                      label.highlight=true \
                      background.border_color=$GREY
  fi
}

set_space_label() {
  sketchybar --set $NAME icon="$@"
}

mouse_clicked() {
  if [ "$BUTTON" = "right" ]; then
    echo ''
  else
    if [ "$MODIFIER" = "shift" ]; then
      SPACE_LABEL="$(osascript -e "return (text returned of (display dialog \"Give a name to space $NAME:\" default answer \"\" with icon note buttons {\"Cancel\", \"Continue\"} default button \"Continue\"))")"
      if [ $? -eq 0 ]; then
        if [ "$SPACE_LABEL" = "" ]; then
          set_space_label "${NAME:6}"
        else
          set_space_label "${NAME:6} ($SPACE_LABEL)"
        fi
      fi
    else
      aerospace workspace ${NAME#*.}
    fi
  fi
}

# echo plugin_space.sh $SENDER >> ~/aaaa
case "$SENDER" in
  "mouse.clicked") mouse_clicked
  ;;
  *) update
  ;;
esac
