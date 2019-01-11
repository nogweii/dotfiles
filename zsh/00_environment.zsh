# Personal preferences. XDG uses these, among other applications
export PAGER="less"

# Find a flavor of vim to use as my editor.
#if [ -n "${commands[nvim-gtk]}" ]; then
#    export EDITOR="nvim-gtk"
if [ -n "${commands[nvim]}" ]; then
    export EDITOR="nvim"
elif [ -n "${commands[vim]}" ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi

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
export HISTSIZE="5000000" # Save a *lot* of history. Space is cheap
export SAVEHIST="${HISTSIZE}"

# Pretty colors! Used by zstyle & ls (and probably others)
eval $(dircolors -b ${DOTSDIR}/config/dircolors)

export LESSHISTSIZE=2000

export RACK_ENV=development
export RAILS_ENV=development

# Don't switch to VT100 mode for line drawing characters when in a UTF-8
# terminal. This fixes 'lqqk' showing up in PuTTY
export NCURSES_NO_UTF8_ACS=1

# Set an environment variable if we think we're running within Linux for
# Windows subsystem (aka Bash on windows)
[ -d /mnt/c/Windows ] && export LXSS=yep

# Sorry Ansible, I like the sentiment but that's too much scrolling for me.
export ANSIBLE_NOCOWS=1

# If the faster/simpler find (https://github.com/sharkdp/fd) is installed, use it for fzf
[ -n "${commands[fd]}" ] && export FZF_DEFAULT_COMMAND="fd --color always --no-ignore"
export FZF_DEFAULT_OPTS="--inline-info --ansi"

# By default, git doesn't have an email set. I use environment variables to
# control it. Override in ~/.zshrc.local.
export GIT_AUTHOR_EMAIL="colin@evaryont.me"
export GIT_COMMITTER_EMAIL="colin@evaryont.me"

# When logged into a GNOME desktop, have gem open work as I expect.
[ "${XDG_SESSION_DESKTOP}" = "gnome" ] && export GEM_EDITOR='gio open'

[ -n "${LXSS}" ] && export WINUSER="$(/mnt/c/Windows/System32/whoami.exe | sed 's/^.*\\//' | tr -d '\n\r')"
