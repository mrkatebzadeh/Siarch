#!/usr/bin/env bash
WINDOW_TITLE="ddterm"
WINDOW_ID=$(yabai -m query --windows | jq -e ".[] | select(.title==\"$WINDOW_TITLE\") | .id") || true
if [[ -z "$WINDOW_ID" ]]; then
	pgrep -x kitty >/dev/null &&
		kitty @ new-window --title "$WINDOW_TITLE" ||
		open -na $(which kitty) --args --title "$WINDOW_TITLE"
else
	WINDOW_QUERY=$(yabai -m query --windows --window "$WINDOW_ID")
  RET=$?
	if [ $RET -ne 0 ]; then

		echo "Query failed. Killing ddterm." $RET
		pkill -x ddterm
		pgrep -x kitty >/dev/null &&
			kitty @ new-window --title "$WINDOW_TITLE" ||
			open -na $(which kitty) --args --title "$WINDOW_TITLE"
		exit 1
	fi
	IS_HIDDEN=$(echo "$WINDOW_QUERY" | jq '."is-hidden"')
	HAS_FOCUS=$(echo "$WINDOW_QUERY" | jq '."has-focus"')
	if [[ "${HAS_FOCUS}" != "true" ]]; then
		yabai -m window "$WINDOW_ID" --space mouse --move abs:0:0 --grid "10:1:0:0:1:4" --layer above --focus
	fi
	if [[ "${IS_HIDDEN}" != "true" ]]; then
		skhd -k "cmd - h"
	fi
fi
