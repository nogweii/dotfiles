# Create my git configuration unless it's already up-to-date.
ztmpl ~/.gitconfig

#function zle-keymap-select zle-line-init {
#    VIMODE="${${KEYMAP/vicmd/%#}/(main|viins)/&}"
#    zle reset-prompt
#}
#function zle-line-init {
#    VIMODE='%#'
#    #zle -K vicmd
#}
#zle -N zle-line-init
#zle -N zle-keymap-select

if [ -x /usr/bin/keychain ] ; then
    keychain --quiet # Start gpg-agent & ssh-agent, but don't add any keys (yet)
    [ -f $HOME/.keychain/${HOSTNAME}-sh ] && source $HOME/.keychain/${HOSTNAME}-sh # Load ssh-agent environment variables
    [ -f $HOME/.keychain/${HOSTNAME}-sh-gpg ] && source $HOME/.keychain/${HOSTNAME}-sh-gpg # ditto, gpg-agent
fi

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -x "$(which fortune 2>&1)" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi

source ~/.config/zsh/zsh-syntax-highlighting.zsh
