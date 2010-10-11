#!/bin/zsh

# Startup applications for X
# Cross-WM, anything put in here will start immediately with X

# # Check if dbus has already started, and if it hasn't, start it.
# dbuslaunch="`which dbus-launch 2>/dev/null`"
# if [ -n "$dbuslaunch" ] && [ -x "$dbuslaunch" ] && [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
#   eval "`$dbuslaunch --sh-syntax --exit-with-session`"
#   export DBUS_SESSION_BUS_PID
#   echo "DBUS session pid: ${DBUS_SESSION_BUS_PID}"
# fi

# # Load the fonts in ~/.fonts/
# test -d "~/.fonts/" && (mkfontdir "~/.fonts/" ; xset fp+ "~/.fonts/")
# # Ask X11 to rebuild its font list.
# xset fp rehash

########################################
###     Make it pretty
########################################
# unclutter -display $DISPLAY -jitter 5 -idle 5 & # Hide the mouse
# eval $(cat ~/.fehbg) # Background
# xcompmgr -fFD 5 & # Special effects (requires Composite extension)
# xsetroot -cursor_name left_ptr # Set the X cursor, not all WM's (Xmonad!) set it

########################################
###     Keyboard stuff
########################################
#xrdb -DHOME=$HOME $(echo $(xrdb -symbols | grep -Ev "\"|'")) -all -load ~/.Xdefaults
## Only enable numlock when I'm not on my laptop, 'gryphon'
#if [ "$HOSTNAME" != "gryphon" ] ; then
#  numlockx on # Enable numlock
#else
#  # Disable tapping/scrolling during keyboard press
#  syndaemon -i2 -d -t -k -p /tmp/syndaemon.pid &
#fi
### setxkbmap should always run before xmodmap!
#setxkbmap -option terminate:ctrl_alt_bksp
#xmodmap ~/.Xmodmap # Remap some keys (swap Caps/Esc)
#autocutsel -selection PRIMARY -fork
#autocutsel -selection CLIPBOARD -fork
#cuteinformer &

# Manage the internet connection
# which wicd-gtk &>/dev/null && wicd-gtk -o &

# Change monitor gamma over time, to avoid blue in my eyes @ night.
# latitude (DC) == 39.2:-77.2
# latitude (Tempe) == 33.4:111.93
#redshift -l 33.4:-111.93 -m vidmode -t 6000:3500 &

# Just in case something happens with the configuration, hopefully a terminal
# will spawn so there is still some control and the situation is salvageable
# without requiring a hard reboot.
#urxvtd -q -o -f &
# (sleep 1; urxvtc) &

## Social application tracking / discovery
#echo "Wakoopa launched, see /tmp/wakoopa.log for output"
# wakoopa &> /dev/null &
#WhatPulse & # Keyboard & Mouse tracking

# Thanks to http://totherme.livejournal.com/4154.html for this trick!
(which xmonad &>/dev/null) && export KDEWM="$(which xmonad)"

#exec xmonad
exec ck-launch-session startkde
