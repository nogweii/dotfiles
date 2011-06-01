#!/bin/zsh

cd ~

gnome-session --session=xmonad &>>.xsession-errors &
wmpid=$!
wait $wmpid
exit 0
