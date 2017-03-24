bindkey -v # Vim mode!

# autoload -U zkdb ; zkdb
file="${XDG_CONFIG_HOME:-"$HOME/.config"}/zsh/zkdb-$TERM-${DISPLAY:-$VENDOR-$OSTYPE}"
if [ -e $file ] ; then
    source $file
    [[ -n ${key[Left]}      ]] && bindkey "${key[Left]}"      backward-char
    [[ -n ${key[Right]}     ]] && bindkey "${key[Right]}"     forward-char
    [[ -n ${key[Up]}        ]] && bindkey "${key[Up]}"        up-line-or-search
    [[ -n ${key[Down]}      ]] && bindkey "${key[Down]}"      down-line-or-search
    [[ -n ${key[Home]}      ]] && bindkey "${key[Home]}"      beginning-of-line
    [[ -n ${key[End]}       ]] && bindkey "${key[End]}"       end-of-line
    [[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
    [[ -n ${key[Delete]}    ]] && bindkey "${key[Delete]}"    delete-char
fi

bindkey '^W' backward-kill-word-match
bindkey . rationalise-dot
# without this, typing a . aborts incremental history search
bindkey -M isearch . self-insert

# Support smarter/safer pasting in terminals with bracketed paste support
# See also: https://cirw.in/blog/bracketed-paste
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste-magic bracketed-paste

# A little bit of emacs in my vim...
bindkey -M viins '^A' beginning-of-line
bindkey -M viins '^E' end-of-line

# A keybind (<C-x>f) to force completing files
zle -C complete-file menu-expand-or-complete _generic
zstyle ':completion:complete-file:*' completer _files
bindkey -M viins '^Xf' complete-file
bindkey -M viins '^X^F' complete-file
