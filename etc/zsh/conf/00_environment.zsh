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

if [ "${TERM_PROGRAM}" = "vscode" ]; then
    export EDITOR='code'
fi

if [ "${XDG_SESSION_DESKTOP:l}" = "kde" ]; then
    export BROWSER="kde-open5"
    export SSH_ASKPASS=/usr/bin/ksshaskpass
    export SSH_ASKPASS_REQUIRE=prefer
fi

# CLI default parameters
export GREP_DEFAULTS="-E -i -I --color=auto"
export LESS="-RSMwi"

# Application settings
export SUDO_PROMPT="[38;5;5m%p[00m's password:"
export GDK_USE_XFT=1
export QT_XFT=true

# Idle timeout value. Automatically closes the shell after this many seconds
export TMOUT=14400 # 4 hours

# MAILDIR & new mail alerts
# test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

export LOCALE="en_US.utf8"

# History location.
export HISTSIZE="5000000" # Save a *lot* of history. Space is cheap
export SAVEHIST="${HISTSIZE}"
export LESSHISTSIZE=2000

# Don't switch to VT100 mode for line drawing characters when in a UTF-8
# terminal. This fixes 'lqqk' showing up in PuTTY
export NCURSES_NO_UTF8_ACS=1

# Set an environment variable if we think we're running within Linux for
# Windows subsystem (aka Bash on windows)
[ -d /mnt/c/Windows ] && export LXSS=yep

# If the faster/simpler find (https://github.com/sharkdp/fd) is installed, use it for fzf
[ -n "${commands[fd]}" ] && export FZF_DEFAULT_COMMAND="fd --color always --no-ignore"
export FZF_DEFAULT_OPTS="--inline-info --ansi"

# By default, git doesn't have an email set. I use environment variables to
# control it. Override in ~/.zshrc.local.
export GIT_AUTHOR_EMAIL="me@nogweii.net"
export GIT_COMMITTER_EMAIL="me@nogweii.net"

[ -n "${LXSS}" ] && export WINUSER="$(/mnt/c/Windows/System32/whoami.exe | sed 's/^.*\\//' | tr -d '\n\r')"

export BAT_THEME="Monokai Extended Bright"
export BAT_STYLE="full"

PROMPT_EOL_MARK="%B%S␤%s%b" # customize zsh's injected newline indicator

export VIRTUAL_ENV_DISABLE_PROMPT=1
export ZK_NOTEBOOK_DIR="${HOME}/notes"

[ -n "${commands[pspg]}" ] && export PSQL_PAGER="pspg"

export BUNDLE_JOBS=4

# Configure the list of timezones `tz` will present
# See https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for a reference
export TZ_LIST="America/Phoenix,Phoenix;Etc/UTC,UTC;America/New_York,US East;America/Los_Angeles,US West;America/Denver,Mountains;America/Chicago,Central"

# A bunch of Hashicorp tools check in to https://checkpoint.hashicorp.com/ but I don't want that
export CHECKPOINT_DISABLE=1

# I manage krew through homebrew/pacman, so... don't phone home
export KREW_NO_UPGRADE_CHECK=1
