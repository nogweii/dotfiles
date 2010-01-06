# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

#umask 077 # defaults => u=rwx,g=,o=
ZHOME="${HOME}/.zsh"
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

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -x "$(which fortune 2>&1)" -a -z "$SSH_CONNECTION" ] ; then
	fortune -s # "Short" apothegms only
fi
#set -o vi
# MAILDIR
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

# Create my git configuration unless it's already up-to-date.
ztmpl ~/.gitconfig

autoload -U backward-kill-word-match
zle -N backward-kill-word-match
bindkey '^W' backward-kill-word-match
zstyle ':zle:*' word-style normal
zstyle ':zle:*' word-chars ''

function autobg() {
    jobs -s >| /tmp/j$$
    while read jnum jdesc
    do
        bg %${${jnum#\[}%\]}
    done < /tmp/j$$
    \rm -f /tmp/j$$
}

function precmd() {
    autobg
}
