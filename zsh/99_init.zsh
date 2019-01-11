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

if [ -S "${XDG_RUNTIME_DIR}/keyring/ssh" ]; then
    # As of GNOME 3.28, the keyring agent wraps the upstream OpenSSH agent process
    # and so I like it now. Set the AUTH_SOCK variable when it's around
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/keyring/ssh"
elif [ -n "${LXSS}" ]; then
    local ssh_agent_wsl="/mnt/c/Users/${WINUSER}/Local Applications/ssh-agent-wsl/ssh-agent-wsl"
    local ssh_agent_service="$(/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe '(Get-Service "ssh-agent").Status;' | tr -d '\r')"
    if [ -x "${ssh_agent_wsl}" ]; then
        # ssh-agent-wsl installed to my home directory allows me to use Windows 10's ssh-agent in WSL
        eval $("${ssh_agent_wsl}" -q -r)
    else
        echo "LXSS/WSL environment detected, but missing ssh-agent-wsl. Can't share SSH keys between environments."
    fi
elif [ -n "${commands[keychain]}" ]; then
    # Use keychain to launch a shared SSH agent across terminals when there is
    # no gnome-keyring
    eval $(keychain --eval --quiet --timeout 120 --noask --agents ssh --absolute --dir $XDG_CACHE_HOME/keychain --host localhost)
fi

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -n "${commands[fortune]}" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi

[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh

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

source ${DOTSDIR}/zsh/plugins/histdb/sqlite-history.zsh 
source ${DOTSDIR}/zsh/plugins/histdb/history-timer.zsh
local HISTDB_FILE="${HOME}/.local/cache/zsh/histdb.sqlite"
add-zsh-hook preexec _start_timer
add-zsh-hook precmd  _stop_timer
