#!/bin/zsh

# Startup applications for X
# Cross-WM, anything put in here will start imediately with X


if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then 
	eval $(dbus-launch --sh-syntax)
fi
echo "DBUS session pid: ${DBUS_SESSION_BUS_PID}" 

numlockx on

# TODO: Setup Devil's Pie (devilspie) to change tint to be lowsest-level,
# positioned wherever

unclutter -root -display $DISPLAY -jitter 5 -idle 5 -reset &

eval $(cat ~/.fehbg)
#nitrogen --restore

#stalonetray --grow-gravity W -t --sticky --geometry 124x24+1280-0 -i 24 --ignore-icon-resize --no-shrink &

#xcompmgr -cC -l-5 -t-5 &
xcompmgr &
#xcbcompmgr &
#cairo-compmgr &

#xscreensaver -no-splash &

xbindkeys -fg ~/.xbindkeysrc.scm &

#gmpc &
#sleep .5

#liferea &
#parcellite &

#zim --iconify --pidfile ${HOME}/Zim/.pidfile ${HOME}/Zim &
#basket &
#notecase NoteCase.ncd &

#claws-mail &

#$HOME/bin/xdelay &
#xonclock --skin=/usr/share/xonclock/skins/chrome-128-blue-out.png --position=TOP-LEFT -d --date-format="%d%b%Y" --use-background &

#netwmpager &

#kdeinit &

#kget &

urxvt &
#xterm &

#gem server &

#volwheel &

#tint &

#devilspie &

#urxvt -name irssiterm -e "sh" -c "screen -R irssi" &

#gpodder &

if [ -f ~/.Xmodmap ] ; then
	xmodmap ~/.Xmodmap
fi

#noteo &
# Notfiy-OSD is used instead, but it's automattically called as needed when
# notification DBUS events are sent

#xrandr --dpi 96

#exec openbox
exec awesome 
#exec xmonad
#exec compiz
#exec fusion-icon
#exec wmii
#exec musca
#exec $HOME/code/barewm/barewm
#exec $HOME/code/dwm-5.5/dwm
