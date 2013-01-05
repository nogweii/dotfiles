#!/bin/zsh

#exec startkde
#exit 0

cd ~

echo "------------------------------------------------" >>~/.xsession-errors
echo "New X running on $(date)" >>~/.xsession-errors

logOutput() {
    echo "----- Running $@" >> ~/.xsession-errors
    $@ 2>>~/.xsession-errors
}

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        if [ -x "$f" ]; then
            echo "Loading ${f}"
            source "$f"
        fi
    done
    unset f
fi

echo "DBUS pid: ${DBUS_SESSION_BUS_PID}"
export DBUS_SESSION_BUS_PID

logOutput xsetroot -bg '#000'

logOutput xrdb-load

logOutput xfce4-panel &

logOutput mpd ${XDG_CONFIG_HOME}/mpd.conf

logOutput conky &

logOutput xscreensaver &

logOutput nm-applet &

logOutput batti &

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


echo "----- Nvidia Settings" >> ~/.xsession-errors


exec xmonad 2>>~/.xsession-errors
