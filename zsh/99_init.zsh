function zle-keymap-select zle-line-init {
    if [ "$KEYMAP" = 'vicmd' ] ; then
        VIMODE='%#' # command mode
    else
        VIMODE='&'  # insert mode
    fi
    zle reset-prompt
}
function zle-line-init {
    VIMODE='&' # zle defaults to insert mode
    # auto-fu-init
    #zle -K vicmd
}
zle -N zle-line-init
zle -N zle-keymap-select

if [ -x /usr/bin/keychain ] ; then
    keychain --quiet # Start gpg-agent & ssh-agent, but don't add any keys (yet)
    [ -f $HOME/.keychain/${HOSTNAME}-sh ] && source $HOME/.keychain/${HOSTNAME}-sh # Load ssh-agent environment variables
    [ -f $HOME/.keychain/${HOSTNAME}-sh-gpg ] && source $HOME/.keychain/${HOSTNAME}-sh-gpg # ditto, gpg-agent
fi

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -x "$(which fortune 2>&1)" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi

[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh

#if [ ! -z "$DISPLAY" ] ; then
#    source ${DOTSDIR}/config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#fi
#source ${XDG_CONFIG_HOME}/zsh/plugins/auto-fu.zsh
