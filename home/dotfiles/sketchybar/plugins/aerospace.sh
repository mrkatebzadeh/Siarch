#!/usr/bin/env bash

# if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
#     sketchybar --set $NAME background.drawing=on
# else
#     sketchybar --set $NAME background.drawing=off
# fi

update() {
    space=${NAME:6}
    echo $SENDER $space $FOCUSED_WORKSPACE
    apps="$(/opt/homebrew/bin/aerospace list-windows --workspace $space | awk '{print $3}')"

    icon_strip=" "
    if [ "${apps}" != "" ]; then
        while read -r app; do
            icon_strip+=" $($CONFIG_DIR/plugins/icon_map.sh "$app")"
        done <<<"${apps}"
    else
        icon_strip=" —"
    fi

    sketchybar --animate sin 10 --set space.$space label="$icon_strip"

    source "$CONFIG_DIR/colors.sh"
    COLOR=$BACKGROUND_2
    if [ "$space" = "$FOCUSED_WORKSPACE" ]; then
        COLOR=$RED
    fi
    sketchybar --set $NAME icon.highlight=$SELECTED \
        label.highlight=$SELECTED \
        background.border_color=$COLOR
}

set_space_label() {
    sketchybar --set $NAME icon="$@"
}

mouse_clicked() {
    SID=${NAME:6}
    if [ "$BUTTON" = "right" ]; then
        yabai -m space --destroy $SID
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
            # TODO
            /opt/homebrew/bin/aerospace workspace $SID
        fi
    fi
}

case "$SENDER" in
    "mouse.clicked")
        mouse_clicked
        ;;
    *)
        update
        ;;
esac
