# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

#umask 077 # defaults => u=rwx,g=,o=
ZHOME="${HOME}/.zsh"
MISSING_FEATURES=()
source $ZHOME/env
source $ZHOME/style
source $ZHOME/alias
source $ZHOME/functions
source $ZHOME/keychain
if [ -e $ZHOME/named_dirs ] ; then
	source $ZHOME/named_dirs
fi

zle -N zle-keymap-select

# Allow comments even in interactive shells i.e.
# $ uname # This command prints system informations
# zsh: bad pattern: #
# $ setopt interactivecomments
# $ uname # This command prints system informations
# Linux
setopt interactivecomments

# Ctrl-Q will no longer freeze the terminal.
stty erase "^?"

setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -e

# Paste a URL? Now auto quoted, yay! :D
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

#cd ${HOME}
if [ -x "$(which fortune)" ] ; then
	fortune -s # "Short" apothegms only
else
	MISSING_FEATURES=($MISSING_FEATURES fortune)
fi
#set -o vi
# MAILDIR
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

# Create my git configuration unless it's already up-to-date.
ztmpl ~/.gitconfig

if [ ! "$MISSING_FEATURES" = "" ] ; then
	echo "Missing some features: ${MISSING_FEATURES}"
fi

autoload -U backward-kill-word-match
zle -N backward-kill-word-match
bindkey '^W' backward-kill-word-match
zstyle ':zle:*' word-style normal
zstyle ':zle:*' word-chars ''
