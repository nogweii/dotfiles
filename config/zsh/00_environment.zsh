# Tell cw programs not to send ANSI color codes to piped outputs
export NOCOLOR_PIPE=1

# Personal preferences. XDG uses these, among other applications
export EDITOR="vim"
export BROWSER="kde-open"
export PAGER="less"

# CLI default parameters
export GREP_DEFAULTS="-E -i -I -n --color=auto"
export LESS="-RSMwi"
export VI_OPTIONS="--servername VIM -p"

# Enviornment variables that affect Zsh
export HISTSIZE=2000
export SAVEHIST=20000

# Identification
export UID="$(id -u)"
export USERID="${UID}"
export ME="${UID}"

# Application settings
export SUDO_PROMPT="[38;5;5m%p[00m's password:"
export GDK_USE_XFT=1
export QT_XFT=true

# Generic Environment stuff
if [ -e /usr/lib/libtrash.so ] ; then
    export LD_PRELOAD="/usr/lib/libtrash.so ${LD_PRELOAD}"
fi

[ -z "$HOSTNAME" ] && export HOSTNAME="$(hostname)"

export TMOUT=3600

# MAILDIR & new mail alerts
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

# Every single locale-related variable I could think of
export            LOCALE="en_US.utf8"
export              LANG="en_US.utf8"
export          LC_CTYPE="en_US.utf8"
export        LC_NUMERIC="en_US.utf8"
export           LC_TIME="en_US.utf8"
export        LC_COLLATE="C"
export       LC_MONETARY="en_US.utf8"
export       LC_MESSAGES="en_US.utf8"
export          LC_PAPER="en_US.utf8"
export           LC_NAME="en_US.utf8"
export        LC_ADDRESS="en_US.utf8"
export      LC_TELEPHONE="en_US.utf8"
export    LC_MEASUREMENT="en_US.utf8"
export LC_IDENTIFICATION="en_US.utf8"
export     HARDWARECLOCK="UTC"
export          TIMEZONE="America/New_York"
# History location.
#export          HISTFILE="${XDG_CACHE_HOME}/zsh_history"
export          HISTFILE="${HOME}/.local/cache/zsh_history"
export          HISTSIZE="5000000" # Save a *lot* of history. Space is cheap
export          SAVEHIST="${HISTSIZE}"

# Various configuration files can be pointed to different places via environment
# variables, so take advantage of that to add 'XDG support'
export WINEARCH="win32"
export WINEPREFIX="${XDG_DATA_HOME}/wine/"
export ACKRC="${XDG_CONFIG_HOME}/ackrc"

# Pretty colors! Used by zstyle & ls (and probably others)
eval $(dircolors -b ${DOTSDIR}/config/dircolors)
export INPUTRC="${DOTSDIR}/inputrc"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
#export GIT_CONFIG="${DOTSDIR}/config/git/config"
export RVC_READLINE='/usr/lib/ruby/1.8/x86_64-linux/readline.so'

# A few applications (like osc) still expect GNOME_DESKTOP_SESSION_ID to exist,
# so define a value if it hasn't already.
[ -z "$GNOME_DESKTOP_SESSION_ID" ] && export GNOME_DESKTOP_SESSION_ID='this-is-deprecated'
[ -z "$KDE_SESSION_ID" ] && export KDE_SESSION_ID=$KDE_SESSION_UID

export WGETRC="${DOTSDIR}/config/wgetrc"

if [ "$COLORTERM" = "gnome-terminal" ]; then
    export TERM="xterm-256color"
fi

# Development environment stuff.
export MYDRIVEADVISOR_DB_PWD="mysqlroot"
export MYSQL_LOCAL_SOCKET="/var/run/mysqld/mysqld.sock"

export LESSHISTFILE="${XDG_CACHE_HOME}/lesshist"
export LESSHISTSIZE=2000

if [ -x /usr/lib/ssh/ksshaskpass ]; then
    export SSH_ASKPASS="/usr/lib/ssh/ksshaskpass"
elif [ -x "${commands[ksshaskpass]}" ]; then
    export SSH_ASKPASS="${commands[ksshaskpass]}"
fi

export GEMRC="${DOTSDIR}/gemrc"
export GEM_SPEC_CACHE="${HOME}/.local/cache/gem"
export CABAL_CONFIG="${DOTSDIR}/config/cabal/config"

export BUNDLE_CONFIG="${DOTSDIR}/config/bundle/config"
#export BUNDLE_PATH="${HOME}/.gem/ruby/2.2.0/"
export npm_config_prefix=$HOME/.local/node
