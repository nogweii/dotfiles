#!/bin/zsh -l

#exec startkde
#exit 0

cd ~

echo "------------------------------------------------" >>~/.xsession-errors
date >>~/.xsession-errors

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

logOutput mpd ~/.local/config/mpd.conf

logOutput conky &

logOutput xscreensaver &

exec xmonad 2>>~/.xsession-errors
