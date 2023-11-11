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

if [ -S "${XDG_RUNTIME_DIR}/podman/podman.sock" ]; then
    export DOCKER_HOST="unix://${XDG_RUNTIME_DIR}/podman/podman.sock"
fi

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -n "${commands[fortune]}" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi

# For Linux installs
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
# For MacOS homebrew installs
[ -f /opt/homebrew/opt/fzf/shell/completion.zsh ] && source /opt/homebrew/opt/fzf/shell/completion.zsh
[ -f /opt/homebrew/opt/fzf/shell/key-bindings.zsh ] && source /opt/homebrew/opt/fzf/shell/key-bindings.zsh

# Enable syntax highlighting:
source ${DOTSDIR}/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
FAST_HIGHLIGHT_STYLES[path]=underline

# Enable fish-shell style history search:
source ${DOTSDIR}/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# histdb: Store my shell history in a sqlite database
typeset -g HISTDB_FILE="${HOME}/.local/cache/zsh/histdb.sqlite"

source ${DOTSDIR}/zsh/plugins/histdb/sqlite-history.zsh 

[ -n "${commands[python3]}" ] && AUTOSWITCH_DEFAULT_PYTHON=python3
source ${DOTSDIR}/zsh/plugins/autoswitch-virtualenv/autoswitch_virtualenv.plugin.zsh
AUTOSWITCH_SILENT='yes'
