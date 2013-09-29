# Zsh will send a notification after a command finishes (and returns to the
# prompt) if it took number of seconds, or more. It will report the time the
# command took and CPU usage (like the output of `time foo`).
#
# If zsh detects a graphical Linux environment with notify-send installed, it
# will also show a pop-up message.

# How long to wait before notifications happen
REPORTTIME=7

# It's assumed that if there is an exported DBUS address, there is a graphical
# environment. (Since dbus-launch complains if it can't connect to X) 
if [ "${commands[notify-send]}" != "" -a "${DBUS_SESSION_BUS_ADDRESS}" != "" ]; then
  __reporttime-preexec-hook() {
    # Blacklist notifications about some programs/aliases
    if [[ ! "${1}" == vim* ]]; then
      __reporttime_about="$1"
    fi
    export __reporttime_start="$SECONDS"
  }

  __reporttime-precmd-hook() {
    local time_taken

    # precmd can execute many times (like every time I hit enter, regardless if
    # I typed a new command) so only notify when there's something 'interesting'
    if [ "${__reporttime_about}" != "" ]; then
      time_taken=$(($SECONDS-$__reporttime_start))
      if (( $time_taken > $REPORTTIME )); then
        if [[ "$(print -P '%?')" = '0' ]]; then
          __reporttime_status='succeded'
        else
          __reporttime_status='failed'
        fi

        # Quick math to make larger times easier to understand (though not
        # 'perfect' since it would be fairly rare for commands to take longer
        # than an hour)
        if [[ $time_taken -ge 60 ]]; then
          time_taken="$(($time_taken/60)) min $(($time_taken%60)) s"
        else
          time_taken="$time_taken s"
        fi

        notify-send "Command Finished" \
          "'${__reporttime_about}' ${__reporttime_status} after ${time_taken}"
      fi
    fi
    # Empty out the variable so we only see the message once
    __reporttime_about=''
  }
  preexec_functions+=(__reporttime-preexec-hook)
  precmd_functions+=(__reporttime-precmd-hook)
fi
