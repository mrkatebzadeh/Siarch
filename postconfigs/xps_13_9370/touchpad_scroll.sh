#!/bin/sh

echo "Section \"InputClass\"
   Identifier \"touchpad\"
   Driver \"libinput\"
   MatchIsTouchpad \"on\"
   Option \"NaturalScrolling\" \"true\"
   Option \"Tapping\" \"on\"
EndSection" >> /etc/X11/xorg.conf.d/30-touchpad.conf
