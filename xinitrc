#!/bin/zsh

# Startup applications for X
# Cross-WM, anything put in here will start immediately with X

LOG_FILE=${STARTXLOG-"/tmp/startx.log"}
cmd() {
  echo -n "cmd:: $@" >> $LOG_FILE
  if [ `which $1 2>/dev/null` ] ; then
    echo -n "\n" >> $LOG_FILE
    eval "$@ >> $LOG_FILE"
  else
    echo " - not found!" >> $LOG_FILE
  fi
}

# Check if dbus has already started, and if it hasn't, start it.
dbuslaunch="`which dbus-launch 2>/dev/null`"
if [ -n "$dbuslaunch" ] && [ -x "$dbuslaunch" ] && [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
  cmd "`$dbuslaunch --sh-syntax --exit-with-session`; export DBUS_SESSION_BUS_PID"
  echo "DBUS session pid: ${DBUS_SESSION_BUS_PID}"
fi

########################################
###     Make it pretty
########################################
cmd unclutter -display $DISPLAY -jitter 5 -idle 5 & # Hide the mouse
cmd $(cat ~/.fehbg) # Background
cmd xcompmgr -fFD 5 & # Special effects (requires Composite extension)
cmd xsetroot -cursor_name left_ptr # Set the X cursor, not all WM's (Xmonad!) set it

########################################
###     Keyboard stuff
########################################
cmd xrdb $(echo $(xrdb -symbols | grep -Ev "\"|'")) -all -load ~/.Xdefaults
# Only enable numlock when I'm not on my laptop, 'gryphon'
if [ "$HOSTNAME" != "gryphon" ] ; then
  cmd numlockx on # Enable numlock
fi
## setxkbmap should always run before xmodmap!
cmd setxkbmap -option terminate:ctrl_alt_bksp
cmd xmodmap ~/.Xmodmap # Remap some keys (swap Caps/Esc)
cmd autocutsel -selection PRIMARY -fork
cmd autocutsel -selection CLIPBOARD -fork

# Manage the internet connection
which wicd-client &>/dev/null && cmd wicd-client -o &> /tmp/wicd.log &

# Change monitor gamma over time, to avoid blue in my eyes @ night.
# latitude (DC) == 39.2:-77.2
# latitude (Tempe) == 33.4:111.93
echo redshift -l 33.4:111.93 -m vidmode -v -t 6000:3500 \&\>/tmp/redshift.log
redshift -l 33.4:111.93 -m vidmode -v -t 6000:3500 &>/tmp/redshift.log &

# Just in case something happens with the configuration, hopefully a terminal
# will spawn so there is still some control and the situation is salvageable
# without requiring a hard reboot.
cmd urxvt.sh &

## Social application tracking / discovery
#echo "Wakoopa launched, see /tmp/wakoopa.log for output"
#wakoopa &> /tmp/wakoopa.log &
#WhatPulse & # Keyboard & Mouse tracking

cmd cuteinformer &

# Thanks to http://totherme.livejournal.com/4154.html for this trick!
#(which xmonad &>/dev/null) && export KDEWM="$(which xmonad)"

#exec awesome
#exec startkde &> /tmp/startkde.log
exec xmonad
#exec VirtualBox
#exec openbox
#exec /home/colin/Downloads/musca-0.9.24/musca
#exec /home/colin/code/lich-barewm/barewm
#XDG_CONFIG_DIRS=$HOME/.config/ exec /home/colin/Downloads/subtle-0.8.1602-zeta/subtle
