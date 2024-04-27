# if there is an SSH connection, use OSC 52 to copy text
if [[ -n "${SSH_TTY}" ]]; then
    ZSH_SYSTEM_CLIPBOARD_METHOD=ssh

    function zsh-system-clipboard-set-ssh() {
        # The maximum length of an OSC 52 escape sequence is 100,000 bytes,
        # including the 7-byte header "\033]52;c;", the 1-byte footer "\007",
        # and then the base64 encoded contents. Since base64 consumes 4 bytes
        # for every 3 bytes of input, we have a limit of 74,994 bytes for the
        # unencoded data.
        # See also: https://sunaku.github.io/tmux-yank-osc52.html
        limit=74994

        IFS='' read -t 1 -d '' -r content
        encoded=$(echo $content | head -c ${limit} | base64 -w 0 -)
        echo -ne "\e]52;c;${encoded}\x07" > ${SSH_TTY}
    }

    # no easy way to paste, in part because I'm lazy. just paste via the
    # terminal client itself
    function zsh-system-clipboard-get-ssh() { ; }
fi

# Don't go through and set all of the default keybindings in the plugin
ZSH_SYSTEM_CLIPBOARD_DISABLE_DEFAULT_MAPS=false
source ${ZDOTDIR}/plugins/system-clipboard/zsh-system-clipboard.zsh

# Instead, define a subset of the keybindings that affect yanking & pasting but
# not deleting/changing text.

bindkey -M vicmd "P" zsh-system-clipboard-vicmd-vi-put-before
bindkey -M vicmd "Y" zsh-system-clipboard-vicmd-vi-yank-eol
bindkey -M vicmd "p" zsh-system-clipboard-vicmd-vi-put-after
bindkey -M vicmd "y" zsh-system-clipboard-vicmd-vi-yank
bindkey -M visual "p" zsh-system-clipboard-visual-put-replace-selection
