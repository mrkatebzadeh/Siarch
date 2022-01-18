#!/bin/sh

# Tap to click
[ ! -f /etc/X11/xorg.conf.d/40-libinput.conf ] && printf 'Section "InputClass"
        Identifier "libinput touchpad catchall"
        MatchIsTouchpad "on"
        MatchDevicePath "/dev/input/event*"
        Driver "libinput"
        # Enable left mouse button by tapping
        Option "Tapping" "on"
EndSection%' > /etc/X11/xorg.conf.d/40-libinput.conf
