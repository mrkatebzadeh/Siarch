#!/bin/env bash

# sets wallpaper
setbg &

# polybar
$HOME/.config/i3/bin/launchbar.sh

# Fix cursor
xsetroot -cursor_name left_ptr

# kill if already running
killall -9 dunst sxhkd deadd-notification-center

# sets superkey
ksuperkey -e 'Super_L=Alt_L|F1' &
ksuperkey -e 'Super_R=Alt_L|F1' &

# start hotkey daemon
sxhkd &

# Launch notification daemon
# dunst -config $HOME/.config/dunst/dunstrc &
deadd-notification-center &





















