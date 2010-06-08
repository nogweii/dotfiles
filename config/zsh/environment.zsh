export HISTFILE="${ZSH}/history"

# Prompt. Make it purrty!
export PROMPT="%B%n%b%d/ "
export RPROMPT="-- INSERT --"

# Extra places which doesn't seem to be added to the normal paths
export PATH="${HOME}/bin:${PATH}:/usr/local/bin:/usr/local/sbin"
export PKG_CONFIG_PATH="/opt/NX/lib/pkgconfig:/opt/kde/lib/pkgconfig:/opt/mozilla/lib/pkgconfig:/opt/qt/lib/pkgconfig:/usr/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/share/pkgconfig"

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

# Test the current terminal settings. If there isn't a termcap,
# then switch from rxvt-unicode to rxvt-256color (or vice-versa).
#
# Meant for Ubuntu & Arch compatibility.
if [ ! $(infocmp $TERM &>/dev/null) ] ; then
    if [ "$TERM" = "rxvt-unicode" -a $(infocmp rxvt-256color &>/dev/null)] ; then
        export TERM=rxvt-256color
    elif [ "$TERM" = "rxvt-256color" -a $(infocmp rxvt-unicode &>/dev/null)] ; then
        export TERM=rxvt-unicode
    fi
fi
