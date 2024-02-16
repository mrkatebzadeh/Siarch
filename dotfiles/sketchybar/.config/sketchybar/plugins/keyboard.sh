#!/bin/bash

LAYOUT="$(defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleSelectedInputSources | grep "KeyboardLayout Name" | cut -c 33- | rev | cut -c 2- | rev)"

# specify short layouts individually.
case "$LAYOUT" in
    "British") SHORT_LAYOUT="UK";;
    "\"Persian-ISIRI 2901\"") SHORT_LAYOUT="ف";;
    *) SHORT_LAYOUT="한";;
esac

sketchybar --set keyboard label="$SHORT_LAYOUT"
