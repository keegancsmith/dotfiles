#!/bin/bash

PATH=$HOME/bin:$PATH

# Set the background color
xsetroot -solid black

# Set up an icon tray This has multi monitor support. Uses this fork of
# trayer: http://github.com/sargon/trayer-srg
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 7 --transparent true --tint 0x000000 --alpha 0 \
    --height 12 --monitor 0 &

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
