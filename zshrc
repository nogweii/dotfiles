# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

ZHOME="${HOME}/.zsh"
. $ZHOME/env
. $ZHOME/style
. $ZHOME/alias
. $ZHOME/functions
. $ZHOME/keychain
if [ -e $ZHOME/named_dirs ] ; then
	. $ZHOME/named_dirs
fi

# Allow comments even in interactive shells i. e.
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
fortune -s # "Short" apothogems only
#set -o vi
# MAILDIR
test -e $HOME/Mail && export MAILDIR=$HOME/Mail && for i in $MAILDIR/*(.); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done
