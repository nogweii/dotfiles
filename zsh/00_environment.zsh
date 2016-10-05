# Personal preferences. XDG uses these, among other applications
export EDITOR="vim"
export PAGER="less"
if [ "${XDG_SESSION_DESKTOP}" = "gnome" ]; then
    export BROWSER="gvfs-open"
elif [ "${XDG_SESSION_DESKTOP}" = "kde" ]; then
    export BROWSER="kde-open"
fi

# CLI default parameters
export GREP_DEFAULTS="-E -i -I -n --color=auto"
export LESS="-RSMwi"

# Application settings
export SUDO_PROMPT="[38;5;5m%p[00m's password:"
export GDK_USE_XFT=1
export QT_XFT=true

[ -z "$HOSTNAME" ] && export HOSTNAME="$(hostname)"

export TMOUT=3600

# MAILDIR & new mail alerts
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

export LOCALE="en_US.utf8"

# History location.
export HISTFILE="${HOME}/.local/cache/zsh_history"
export HISTSIZE="5000000" # Save a *lot* of history. Space is cheap
export SAVEHIST="${HISTSIZE}"

# Pretty colors! Used by zstyle & ls (and probably others)
eval $(dircolors -b ${DOTSDIR}/config/dircolors)
export INPUTRC="${DOTSDIR}/inputrc"
export WGETRC="${DOTSDIR}/config/wgetrc"

export LESSHISTFILE="${XDG_CACHE_HOME}/lesshist"
export LESSHISTSIZE=2000

export GEMRC="${DOTSDIR}/gemrc"
export GEM_SPEC_CACHE="${HOME}/.local/cache/gem"
#export CABAL_CONFIG="${DOTSDIR}/config/cabal/config"

export BUNDLE_CONFIG="${DOTSDIR}/config/bundle/config"
export BUNDLE_PATH="${HOME}/.gem/ruby/2.3.0/"
export npm_config_prefix=$HOME/.local/node

export RACK_ENV=development
export RAILS_ENV=development

# Don't switch to VT100 mode for line drawing characters when in a UTF-8
# terminal. This fixes 'lqqk' showing up in PuTTY
export NCURSES_NO_UTF8_ACS=1

[ -d ~/go ] && export GOPATH=~/go

# Set an environment variable if we think we're running within Linux for
# Windows subsystem (aka Bash on windows)
[ -d /mnt/c/Windows ] && export LXSS=yep
