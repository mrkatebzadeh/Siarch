#!/usr/bin/env bash

SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
sid=0
space=()
# for sid in $(/opt/homebrew/bin/aerospace list-workspaces --all); do
for i in "${!SPACE_ICONS[@]}"; do
    sid=$(($i + 1))
    sketchybar --add item space.$sid left \
        --set space.$sid \
        icon="${SPACE_ICONS[i]}" \
        icon.padding_left=10 \
        icon.padding_right=10 \
        padding_left=2 \
        padding_right=2 \
        label.padding_right=20 \
        icon.highlight_color=$RED \
        label.color=$GREY \
        label.highlight_color=$WHITE \
        label.font="sketchybar-app-font:Regular:16.0" \
        label.y_offset=-1 \
        background.color=$BACKGROUND_1 \
        background.border_color=$BACKGROUND_2 \
        script="$CONFIG_DIR/plugins/aerospace.sh $sid" \
        --subscribe space.$sid aerospace_workspace_change \
        --subscribe space.$sid mouse.clicked
done
