#!/bin/bash
BLUETOOTH_EVENT="com.airpodsbluetooth.status"
POPUP_OFF='sketchybar --set airpods popup.drawing=off'
POPUP_CLICK_SCRIPT='sketchybar --set $NAME popup.drawing=toggle'

airpods_charge=(
  label="$($PLUGIN_DIR/airpods_menu.sh)"
)
sketchybar --add event bluetooth_update $BLUETOOTH_EVENT

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
	--subscribe airpods bluetooth_update volume_change

sketchybar --add bracket status brew github.bell wifi volume_icon airpods --set status "${status_bracket[@]}" \
	--subscribe status bluetooth_update volume_change
