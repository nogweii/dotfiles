#!/bin/zsh

cd ~

# Startup applications for X
# Cross-WM, anything put in here will start immediately with X

export PATH="$PATH:$HOME/bin:"

# Let xmonad start first, then the related background processes. Just an
# attempt at making some stuff run in parallel.
#xmonad &
#wmpid=$!
#cairo-compmgr &
dbus-monitor --session &> dbus_monitor_session &
gnome-session --session=xmonad &>>.xsession-errors &
wait $wmpid
exit 0

# Check if dbus has already started, and if it hasn't, start it.
dbuslaunch="`which dbus-launch 2>/dev/null`"
if [ -n "$dbuslaunch" ] &&
   [ -x "$dbuslaunch" ] &&
   [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
  dbussession="`$dbuslaunch --sh-syntax --exit-with-session`"
  echo $dbussession
  eval "$dbussession"
  export DBUS_SESSION_BUS_PID
  echo "DBUS session pid: ${DBUS_SESSION_BUS_PID}"
fi

{
  # Load the fonts in ~/.fonts/
  test -d "~/.fonts/" && (mkfontdir "~/.fonts/" ; xset fp+ "~/.fonts/")
  # Ask X11 to rebuild its font list.
  xset fp rehash
} &

######################################
#     Make it pretty
######################################
#unclutter -display $DISPLAY -jitter 5 -idle 5 & # Hide the mouse
eval $(cat ~/.fehbg) # Background
xcompmgr -cnF & # Special effects (requires Composite extension)
xsetroot -cursor_name left_ptr # Set the X cursor, not all WM's (Xmonad!) set it
stalonetray &
dzen-batt-mon &
dzen-network &

#######################################
##     Keyboard stuff
#######################################
xrdb-load &
# Only enable numlock when I'm not on my laptop, 'gryphon'
if [ "$HOSTNAME" != "gryphon" ] ; then
  numlockx on # Enable numlock
else
  # Enable some extra features
  synclient VertTwoFingerScroll=1 HorizTwoFingerScroll=1 HorizEdgeScroll=1 \
            PalmDetect=1 CircularScrolling=1
  # Disable tapping/scrolling during keyboard press
  syndaemon -i1 -d -k -p /tmp/syndaemon.pid &
fi
## setxkbmap should always run before xmodmap!
setxkbmap -option terminate:ctrl_alt_bksp
xmodmap ~/.Xmodmap # Remap some keys (swap Caps/Esc)
autocutsel -selection PRIMARY -fork
autocutsel -selection CLIPBOARD -fork
cuteinformer &

# Manage the internet connection
which wicd-gtk &>/dev/null && wicd-gtk &

#redshift-toggle & # Launch redshift for the first time

# Just in case something happens with the configuration, hopefully a terminal
# will spawn so there is still some control and the situation is salvageable
# without requiring a hard reboot.
urxvtd -o -f
(sleep 1; urxvtc) &

# I don't care, these apps give out a lot of random verbose output...block it.
LD_PRELOAD="" wakoopa 1> /dev/null &
LD_PRELOAD="" WhatPulse 1>/dev/null & # Keyboard & Mouse tracking
LD_PRELOAD="" skype 1>/dev/null &

#dropbox start &

# lock the server after 10 mins, configured via Xdefaults
xautolock &

wait $wmpid
