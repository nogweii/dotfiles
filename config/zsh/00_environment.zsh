# Extra places which doesn't seem to be added to the normal paths, or buggy
# profile.d scripts that don't do their job.
export PATH="${HOME}/bin:${PATH}:/usr/local/bin:/usr/local/sbin:/opt/java/bin"
if [ -x '/usr/bin/cope_path' ] ; then
    export PATH="$(cope_path):$PATH"
elif [ -d '/usr/lib/cw' ] ; then
    export PATH="/usr/lib/cw:$PATH"
    export NOCOLOR_PIPE=1
fi
# Local cabal install.
if [ -d "${HOME}/.cabal/bin" ] ; then
    export PATH="$PATH:${HOME}/.cabal/bin"
fi
# Append the gem installation directory, if it exists.
[ -d ~/.gem/ruby/1.9.1/bin ] && export PATH="$PATH:$HOME/.gem/ruby/1.9.1/bin"
typeset -U path # Remove duplicate entries

# Personal preferences. XDG uses these, among other applications
export EDITOR="vim"
export BROWSER="elinks"
export PAGER="less"
export MANPAGER="vim.man"

# CLI default parameters
export GREP_DEFAULTS="-E -i -I -n --color=auto"
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

export RECOLL_CONFDIR=$XDG_CONFIG_HOME/recoll

export TERMINAL="urxvt.sh"
export TMOUT=3600

# XDG-related stuff
export XDG_CACHE_HOME="${HOME}/.local/cache"
if [ ! -d $XDG_CACHE_HOME/zsh ] ; then
    mkdir -p $XDG_CACHE_HOME/zsh
fi

export XDG_CONFIG_HOME="${HOME}/.local/config"
if [ ! -d $XDG_CONFIG_HOME/zsh ] ; then
    mkdir -p $XDG_CONFIG_HOME/zsh
fi

export XDG_DATA_HOME="${HOME}/.local/share"
if [ ! -d $XDG_DATA_HOME/zsh ] ; then
    mkdir -p $XDG_DATA_HOME/zsh
fi
export XDG_DATA_DIRS="${HOME}/.local/share/:${XDG_DATA_DIRS}"
export XDG_CONFIG_DIRS="${HOME}/.local/config/:${XDG_CONFIG_DIRS}"

# MAILDIR
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

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
export          HISTSIZE="5000000"
export          SAVEHIST="${HISTSIZE}"

unset MANPATH

fpath=($XDG_DATA_HOME/zsh $fpath)
export FBCMD="${XDG_CONFIG_HOME}/fbcmd/"
export KDEHOME="${XDG_CONFIG_HOME}/kde/"
export WINEARCH="win32"
export WINEPREFIX="${XDG_DATA_HOME}/wine/"
export ACKRC="${XDG_CONFIG_HOME}/ackrc"

eval `dircolors -b ${XDG_CONFIG_HOME}/dircolors`
export SCREENRC="${XDG_CONFIG_HOME}/screenrc"
export GIT_CONFIG="${XDG_CONFIG_HOME}/git/config"
export INPUTRC="${XDG_CONFIG_HOME}/inputrc"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
