#!/bin/sh

echo "Section \"InputClass\"
   Identifier \"touchpad\"
   Driver \"libinput\"
   MatchIsTouchpad \"on\"
   Option \"NaturalScrolling\" \"true\"
EndSection" >> /etc/X11/xorg.conf.d/30-touchpad.conf
