#!/usr/bin/env bash
WINDOW_TITLE="ddterm"
yabai -m window --toggle ddterm || open -na /Applications/Kitty.app --args --title "$WINDOW_TITLE"
