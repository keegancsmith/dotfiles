#!/bin/bash

PATH=$HOME/bin:$PATH

# Set the background color
xsetroot -solid black

# Set up an icon tray This has multi monitor support. Uses this fork of
# trayer: http://github.com/sargon/trayer-srg
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x000000 --alpha 0 \
    --height 17 --monitor 0 &

gnome-screensaver
gnome-settings-daemon

sleep 1
gnome-power-manager &
nm-applet --sm-disable &
gnome-volume-control-applet &

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

exec xmonad
