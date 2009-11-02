#!/bin/bash

# Set the background color
xsetroot -solid black

# Set up an icon tray
stalonetray &

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
