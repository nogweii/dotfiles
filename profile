export XDG_CONFIG_HOME="${HOME}/.config" # dir-make
export XDG_CACHE_HOME="${HOME}/.local/cache" # dir-make
export XDG_DATA_HOME="${HOME}/.local/share" # dir-make
export TERMINFO="${HOME}/.local/terminfo" # dir-make
export PROJECT_DIR="${HOME}/code" # dir-make

# Ensure that we have a sane (not-empty) umask. This might happen is umask is
# not called in /etc/profile or if pam_umask hasn't had a chance yet
if [ ! $(( $(umask) + 0 )) -gt 0 ]; then
  # Nope, an empty umask this time around. Fix it with one that works well for
  # me:
  umask 022
fi

# A quick check for what type of system I'm on.
os_name=`uname`
if [ "${os_name}" = "Darwin" ]; then
  IS_OSX=1
elif [ "${os_name}" = "Linux" ]; then
  IS_LINUX=1
fi

# vim: set syn=sh:
