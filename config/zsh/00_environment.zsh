# Extra places which doesn't seem to be added to the normal paths
export PATH="${HOME}/bin:${PATH}:/usr/local/bin:/usr/local/sbin"
export PKG_CONFIG_PATH="/opt/NX/lib/pkgconfig:/opt/kde/lib/pkgconfig:/opt/mozilla/lib/pkgconfig:/opt/qt/lib/pkgconfig:/usr/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/share/pkgconfig"
if [ -d '/usr/lib/cw' ] ; then
    export PATH="/usr/lib/cw:$PATH"
    export NOCOLOR_PIPE=1
fi

# Personal preferences. XDG uses these, among other applications
export EDITOR="vim"
export BROWSER="elinks"
export PAGER="less"
export MANPAGER="vim.man"

# CLI default parameters
export GREP_DEFAULTS="-RPiI"
export LESS="-r"
export VI_OPTIONS="--servername VIM -p"

# Enviornment variables that affect Zsh
export HISTSIZE=2000
export SAVEHIST=20000

# Identification
export UID="$(id -u)"
export USERID="${UID}"
export ME="${UID}"

# Application settings
export SUDO_PROMPT="Your Password:"
export GDK_USE_XFT=1
export QT_XFT=true

# Generic Environment stuff
if [ -e /usr/lib/libtrash.so ] ; then
    export LD_PRELOAD="/usr/lib/libtrash.so ${LD_PRELOAD}"
fi
export TERMINFO="~/.terminfo"

[ -z "$HOSTNAME" ] && export HOSTNAME="$(hostname)"

eval `dircolors -b $HOME/.dir_colors`

export RECOLL_CONFDIR=$XDG_CONFIG_HOME/recoll

export TERMINAL="urxvt.sh"
export TMOUT=3600

# XDG-related stuff
export XDG_CACHE_HOME="${HOME}/.cache/"
if [ ! -d $XDG_CACHE_HOME/zsh ] ; then
    mkdir -p $XDG_CACHE_HOME/zsh
fi

export XDG_CONFIG_HOME="${HOME}/.config/"
if [ ! -d $XDG_CONFIG_HOME/zsh ] ; then
    mkdir -p $XDG_CONFIG_HOME/zsh
fi

export XDG_DATA_HOME="${HOME}/.data/"
if [ ! -d $XDG_DATA_HOME/zsh ] ; then
    mkdir -p $XDG_DATA_HOME/zsh
fi

# MAILDIR
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

export     XDG_DATA_DIRS="${HOME}/.data/:${HOME}/.local/share/:${XDG_DATA_DIRS}"
export   XDG_CONFIG_DIRS="${HOME}/.config/:${HOME}/.local/config/:${XDG_CONFIG_DIRS}"
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
export          HISTFILE="${XDG_CACHE_HOME}/zsh/history"

fpath=(~/.data/zsh $fpath)
