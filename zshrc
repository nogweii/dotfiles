# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

ZSH="${HOME}/.config/zsh"
source $ZSH/environment.zsh
 source $ZSH/locale.zsh
 source $ZSH/xdg.zsh
source $ZSH/style
source $ZSH/aliases.zsh
 source $ZSH/pacman.zsh
 source $ZSH/aliases.ubuntu.zsh
source $ZSH/functions.zsh
#source $ZSH/keychain
if [ -e $ZSH/named_dirs ] ; then
    source $ZSH/named_dirs
fi

setopt interactivecomments
#setopt Share_History
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -v # Vim mode!

# Paste a URL? Now auto quoted, yay! :D
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -x "$(which fortune 2>&1)" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi
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

rationalise-dot() {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+=/
    else
        LBUFFER+=.
    fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
    RPS2=$RPS1

    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

keychain --quiet # Start ssh-agent, but don't add any keys (yet)
[ -f $HOME/.keychain/$HOSTNAME-sh ] && source $HOME/.keychain/$HOSTNAME-sh # Load ssh-agent environment variables
[ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] && source $HOME/.keychain/$HOSTNAME-sh-gpg # ditto, gpg-agent

# Ignore ^D when we're on an empty line, therefore, I have to type "exit" or
# "logout" to kill a zsh session
setopt IGNORE_EOF

export PATH="/usr/lib/cw:$PATH"
export NOCOLOR_PIPE=1
