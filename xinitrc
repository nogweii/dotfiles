#!/bin/zsh

# Startup applications for X
# Cross-WM, anything put in here will start immediately with X


if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then 
	eval $(dbus-launch --sh-syntax)
fi
echo "DBUS session pid: ${DBUS_SESSION_BUS_PID}" 



#######################################
##     Make it pretty
#######################################
unclutter -root -display $DISPLAY -jitter 5 -idle 5 -reset -noevents & # Hide the mouse
eval $(cat ~/.fehbg) # Background
xcompmgr & # Special effects (requires Composite extension)



#######################################
##     Keyboard stuff
#######################################
numlockx on # Enable numlock
xbindkeys -fg ~/.xbindkeysrc.scm & # Hot keys
xmodmap ~/.Xmodmap # Remap some keys (swap Caps/Esc)
autocutsel -fork # Sync the clipboards



# Just in case something happens with the configuration, hopefully a terminal
# will spawn so there is still some control and the situation is salvageable
# without requiring a hard reboot.
urxvt &

exec awesome
##exec VirtualBox
