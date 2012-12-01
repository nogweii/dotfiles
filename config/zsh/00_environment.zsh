# Extra places which doesn't seem to be added to the normal paths, or buggy
# profile.d scripts that don't do their job.
export PATH="${HOME}/bin:${PATH}:/usr/local/bin:/usr/local/sbin:/opt/java/bin"
# Optional: Pretty colorized versions of various core utilities (time, free,
# etc..)
if [ -x '/usr/bin/cope_path' ] ; then
    export PATH="$(cope_path):$PATH"
elif [ -d '/usr/lib/cw' ] ; then
    export PATH="/usr/lib/cw:$PATH"
    export NOCOLOR_PIPE=1
fi
# Heroku Toolbelt, via a package. NB: This should be in the path before the
# rubygems directory so that the old gem won't take precedence in the PATH over
# the packaged version!
if [ -d "/usr/local/heroku/bin" ]; then
    export PATH="/usr/local/bin:$PATH"
fi

# Rubygems
if [ -d "$HOME/.rubygems/bin" ]; then
    export PATH="$PATH:$HOME/.rubygems/bin"

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
if [[ ! -z $commands[vim.man] ]]; then
    export MANPAGER="vim.man"
fi

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
export SUDO_PROMPT="Your Password:"
export GDK_USE_XFT=1
export QT_XFT=true

# Generic Environment stuff
if [ -e /usr/lib/libtrash.so ] ; then
    export LD_PRELOAD="/usr/lib/libtrash.so ${LD_PRELOAD}"
fi
export TERMINFO="~/.terminfo"

[ -z "$HOSTNAME" ] && export HOSTNAME="$(hostname)"

export RECOLL_CONFDIR=$dot_path/recoll

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
export          HISTFILE="${XDG_CACHE_HOME}/zsh/history"
export          HISTSIZE="5000000" # Save a *lot* of history. Space is cheap
export          SAVEHIST="${HISTSIZE}"

unset MANPATH

# Include my scripts/functions in the normal function search path
fpath=($XDG_DATA_HOME/zsh $fpath)

# Various configuration files can be pointed to different places via environment
# variables, so take advantage of that to add 'XDG support'
export FBCMD="${dot_path}/fbcmd/"
export KDEHOME="${dot_path}/kde/"
export WINEARCH="win32"
export WINEPREFIX="${XDG_DATA_HOME}/wine/"
export ACKRC="${dot_path}/ackrc"
export SCREENRC="${dot_path}/config/screenrc"

# Pretty colors! Used by zstyle & ls (and probably others)
eval `dircolors -b ${dot_path}/config/dircolors`
export SCREENRC="${dot_path}/config/screenrc"
export INPUTRC="${dot_path}/inputrc"
export MPLAYER_HOME="${dot_path}/config/mplayer"
#export GIT_CONFIG="${dot_path}/config/git/config"
export RVC_READLINE='/usr/lib/ruby/1.8/x86_64-linux/readline.so'

export GEM_HOME="${HOME}/.rubygems"
