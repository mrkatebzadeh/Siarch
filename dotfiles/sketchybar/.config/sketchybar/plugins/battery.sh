#!/bin/bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"

BATTERY_INFO="$(pmset -g batt)"
PERCENTAGE=$(echo "$BATTERY_INFO" | grep -Eo "\d+%" | cut -d% -f1)
CHARGING=$(echo "$BATTERY_INFO" | grep 'AC Power')

if [ $PERCENTAGE = "" ]; then
  exit 0
fi

DRAWING=on
COLOR=$WHITE
case ${PERCENTAGE} in
  9[0-9]|100) ICON=$BATTERY_100; DRAWING=off
  ;;
  [6-8][0-9]) ICON=$BATTERY_75; DRAWING=off
  ;;
  [3-5][0-9]) ICON=$BATTERY_50; COLOR=$YELLOW
  ;;
  [2][0-9]) ICON=$BATTERY_25; COLOR=$ORANGE
  ;;
  [1][0-9]) ICON=$BATTERY_0; COLOR=$RED
  ;;
  *) ICON=$BATTERY_0; COLOR=$RED
esac

if [[ $CHARGING != "" ]]; then
  ICON=$BATTERY_CHARGING
  DRAWING=off
fi
DRAWING=on
sketchybar --set $NAME drawing=$DRAWING icon="$ICON" icon.color=$COLOR 
sketchybar --set $NAME drawing=on label="$PERCENTAGE%" label.color=$COLOR

sketchybar --animate sin 20 --set $NAME 


click() {
  CURRENT_WIDTH="$(sketchybar --query $NAME | jq -r .label.width)"

  WIDTH=0
  if [ "$CURRENT_WIDTH" -eq "0" ]; then
    WIDTH=dynamic
  fi

  sketchybar --animate sin 20 --set $NAME label.width="$WIDTH"
}

case "$SENDER" in
  "mouse.clicked") click
  ;;
esac
