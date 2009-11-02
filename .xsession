#!/bin/bash

# Set the background color
xsetroot -solid black

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 7 --transparent true --tint 0x000000 --height 12 &

gnome-screensaver
gnome-settings-daemon

if [ -x /usr/bin/gnome-power-manager ] ; then
    sleep 1
    gnome-power-manager &
fi

if [ -x /usr/bin/nm-applet ] ; then
    nm-applet --sm-disable &
fi

exec xmonad
