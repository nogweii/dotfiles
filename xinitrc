#!/bin/zsh -l

exec startkde
exit 0

cd ~

echo "------------------------------------------------" >>~/.xsession-errors
echo "New X running on $(date)" >>~/.xsession-errors

logOutput() {
    echo "----- Running $@" >> ~/.xsession-errors
    $@ 2>>~/.xsession-errors
}

# A basic shell function that checks if a program exists, and if it does & is
# executable, executes it, via logOutput
maybeExec() {
  [ -x $1 ] && logOutput $1
}

# Launch dbus session instance
dbuslaunch="`which dbus-launch 2>/dev/null`"
if [ -n "$dbuslaunch" ] && [ -x "$dbuslaunch" ] && [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
  eval `$dbuslaunch --sh-syntax --exit-with-session`
fi
echo "DBUS pid: ${DBUS_SESSION_BUS_PID}"
export DBUS_SESSION_BUS_PID

maybeExec /usr/bin/start-pulseaudio-x11
maybeExec /usr/bin/xdg-user-dirs-update

# Background
logOutput xsetroot -bg '#000'
logOutput eval $(cat ~/.fehbg)

logOutput xrdb-load

logOutput tint2 -c ${XDG_CONFIG_HOME}/tintrc >/dev/null &

logOutput mpd ${XDG_CONFIG_HOME}/mpd.conf

logOutput conky &

logOutput xscreensaver &

logOutput nm-applet &

logOutput batti &

# Find the xmodmap configuration file
xmodmap_conf="${XDG_CONFIG_HOME}/X11/modmap"
# parse out the 'Key doubles' list
keydouble_pairs=$(grep -i '^![[:space:]]*Key doubles:' "$xmodmap_conf" | sed 's/^[^:]\+:[[:space:]]*//')
# Run xmodmap first, to register the 250+ key codes first
#logOutput xmodmap $xmodmap_conf
# Then lauch keydouble with the list of keys
#logOutput ~/keydouble/keydouble $keydouble_pairs &

logOutput autocutsel -fork &
logOutput autocutsel -selection PRIMARY -fork &

# Pretty effects, like shadows & transparency
logOutput compton --dbus -b

# Color transitions for the screen.
logOutput redshift &

#####################
# TrackPoint settings
#####################

echo "----- TrackPoint Settings" >> ~/.xsession-errors

# enabling wheel emulation
logOutput xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
# setting the preferred button (#2, middle)
logOutput xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
# hold for 200 milliseconds to enable
logOutput xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 16 200
# move in any direction to scroll in that ways
logOutput xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5

# Disable the mouse emulation in the joystick. Only ran once, though :-/
xinput disable "Logitech Logitech Dual Action (keys)"
xinput set-prop "Logitech Logitech Dual Action" "Button Mapping" 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
xinput set-prop "Logitech Logitech Dual Action" "Button Number" 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
xinput set-prop "Logitech Logitech Dual Action" "Axis Mapping" 0, 0, 0, 0, 0, 0
xinput set-prop "Logitech Logitech Dual Action" "Axis Type" 0, 0, 0, 0, 0, 0

echo "----- Nvidia Settings" >> ~/.xsession-errors


exec xmonad 2>>~/.xsession-errors
