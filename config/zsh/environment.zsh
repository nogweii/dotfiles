export HISTFILE="${ZSH}/history"

# Prompt. Make it purrty!
export PROMPT="%B%n%b%d%(?..[%?])/ "
export RPROMPT="-- INSERT --"

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
if [ ! -d $XDG_CACHE_HOME ] ; then
    mkdir -p $XDG_CACHE_HOME
fi

export XDG_CONFIG_HOME="${HOME}/.config/"
if [ ! -d $XDG_CONFIG_HOME ] ; then
    mkdir -p $XDG_CONFIG_HOME
fi

export XDG_DATA_HOME="${HOME}/.data/"
if [ ! -d $XDG_DATA_HOME ] ; then
    mkdir -p $XDG_DATA_HOME
fi

# Other directories (colon separated) to search through.
export XDG_DATA_DIRS="${HOME}/.data/:${HOME}/.local/share/:${XDG_DATA_DIRS}"
export XDG_CONFIG_DIRS="${HOME}/.config/:${HOME}/.local/config/:${XDG_CONFIG_DIRS}"

# MAILDIR
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

fpath=(~/.data/zsh $fpath)
