#!/bin/bash

# create variables
while read L; do
	k="`echo "$L" | cut -d '=' -f 1`"
	v="`echo "$L" | cut -d '=' -f 2`"
	export "$k=$v"
done < <(grep -e '^\(title\|artist\|album\|stationName\|pRet\|pRetStr\|wRet\|wRetStr\|songDuration\|songPlayed\|rating\|coverArt\|stationCount\|station[0-9]*\)=' /dev/stdin) # don't overwrite $1...

logger -t "pianobar" "eventcmd.sh: $@"

case "$1" in
	songstart)
		notify-send "Pandora Radio" "Now playing: $title by $artist"
		logger -t "pianobar" "new song: $title by $artist"
		;;

	*)
		if [ "$pRet" -ne 1 ]; then
			notify-send "Pandora Raido" "$1 failed: $pRetStr"
		elif [ "$wRet" -ne 1 ]; then
			notify-send "Pandora Raido" "$1 failed: Network error: $wRetStr"
		fi
		;;
esac

