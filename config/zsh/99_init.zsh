# Create my git configuration unless it's already up-to-date.
ztmpl ~/.gitconfig

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
    RPS2=$RPS1

    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

keychain --quiet # Start ssh-agent, but don't add any keys (yet)
[ -f $HOME/.keychain/${HOSTNAME}-sh ] && source $HOME/.keychain/${HOSTNAME}-sh # Load ssh-agent environment variables
[ -f $HOME/.keychain/${HOSTNAME}-sh-gpg ] && source $HOME/.keychain/${HOSTNAME}-sh-gpg # ditto, gpg-agent

[[ ! -d ~/.data/zsh ]] && mkdir ~/.data/zsh

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -x "$(which fortune 2>&1)" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi
