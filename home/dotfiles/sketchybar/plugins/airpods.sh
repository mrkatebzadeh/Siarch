#!/bin/bash


DEVICES="$(system_profiler SPBluetoothDataType -json -detailLevel basic 2>/dev/null | jq '.SPBluetoothDataType' | jq '.[0]' | jq '.device_connected' | jq '.[] | to_entries[] | select(.value.device_minorType == "Headphones")')"

if [ "$DEVICES" = "" ]; then
	sketchybar -m --set $NAME drawing=off
else
	sketchybar -m --set $NAME drawing=on
  source "$CONFIG_DIR/icons.sh"
	sketchybar -m --set $NAME icon=$AIRPODS
fi
