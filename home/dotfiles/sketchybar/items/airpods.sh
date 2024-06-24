#!/bin/bash
POPUP_OFF='sketchybar --set airpods popup.drawing=off'
POPUP_CLICK_SCRIPT='sketchybar --set $NAME popup.drawing=toggle'

airpods_charge=(
  label="$($PLUGIN_DIR/airpods_menu.sh)"
)

airpods=(
	icon.font="$FONT:Black:16.0"
	icon.color=$GREEN
	padding_left=2
	padding_right=2
	script="$PLUGIN_DIR/airpods.sh"
	click_script="$POPUP_CLICK_SCRIPT"
	popup.height=35
)

echo apple_prefs  "${apple_prefs[@]}"

sketchybar --add item airpods right \
	--set airpods "${airpods[@]}"\
	--add item airpods.charge popup.airpods \
	--set airpods.charge "${airpods_charge[@]}" \
	--subscribe airpods bluetooth_change

sketchybar --add bracket status battery wifi volume_icon airpods --set status "${status_bracket[@]}"
