#!/bin/zsh

# Startup applications for X
# Cross-WM, anything put in here will start immediately with X

# Check if dbus has already started, and if it hasn't, start it.
dbuslaunch="`which dbus-launch 2>/dev/null`"
if [ -n "$dbuslaunch" ] && [ -x "$dbuslaunch" ] && [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
  eval `$dbuslaunch --sh-syntax --exit-with-session`
fi
export DBUS_SESSION_BUS_PID
echo "DBUS session pid: ${DBUS_SESSION_BUS_PID}" 


#######################################
##     Make it pretty
#######################################
#unclutter -root -display $DISPLAY -jitter 5 -idle 5 -reset -noevents & # Hide the mouse
eval $(cat ~/.fehbg) # Background
xcompmgr & # Special effects (requires Composite extension)
xsetroot -cursor_name left_ptr # Set the X cursor, not all WM's (Xmonad!) set it


#######################################
##     Keyboard stuff
#######################################
numlockx on # Enable numlock
xbindkeys -fg ~/.xbindkeysrc.scm & # Hot keys
# setxkbmap should always run before xmodmap!
setxkbmap -option terminate:ctrl_alt_bksp
xmodmap ~/.Xmodmap # Remap some keys (swap Caps/Esc)
autocutsel -fork # Sync the clipboards

which wicd-client &>/dev/null && wicd-client -o &> /tmp/wicd.log &

urxvtd -q -o -f

# Just in case something happens with the configuration, hopefully a terminal
# will spawn so there is still some control and the situation is salvageable
# without requiring a hard reboot.
urxvtc &

 exec awesome
#exec xmonad
#exec VirtualBox
#exec openbox
#exec /home/colin/Downloads/musca-0.9.24/musca
#exec /home/colin/code/lich-barewm/barewm
