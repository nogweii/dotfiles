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
