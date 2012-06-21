#!/bin/zsh -l

cd ~

echo "------------------------------------------------" >>~/.xsession-errors
date >>~/.xsession-errors

logOutput() {
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

logOutput unagi &
#logOutput /usr/lib/notify-osd-customizable/notify-osd &
logOutput conky &

logOutput xscreensaver &

logOutput conky &
#xmodmap ~/.local/config/X11/Xmodmap 2>>~/.xsession-errors

#urxvtd -q -o -f 2>>~/.xsession-errors
#(sleep 1; urxvtc) &

#[ -r ~/.fehbg ] && eval $(cat ~/.fehbg) 2>>~/.xsession-errors

#old_xdg=$XDG_CONFIG_HOME
# Just this one application, stick it's configuration in ~/.xmonad/taffybar/
# rather than the normal XDG location.
#XDG_CONFIG_HOME="${HOME}/.xmonad" taffybar 2>>~/.xsession-errors &
#export XDG_CONFIG_HOME=$old_xdg # Just to be sure
#unset old_xdg

#exec ck-launch-session xmonad

exec ck-launch-session awesome 2>>~/.xsession-errors
