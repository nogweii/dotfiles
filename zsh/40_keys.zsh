bindkey -v # Vim mode!

# autoload -U zkdb ; zkdb
local zkdb_file="${XDG_CONFIG_HOME:-"$HOME/.config"}/zsh/zkdb-$TERM-${DISPLAY:-$VENDOR-$OSTYPE}"
if [ -e $zkdb_file ] ; then
    source $zkdb_file
    [[ -n ${key[Left]}      ]] && bindkey "${key[Left]}"      backward-char
    [[ -n ${key[Right]}     ]] && bindkey "${key[Right]}"     forward-char
    [[ -n ${key[Up]}        ]] && bindkey "${key[Up]}"        up-line-or-search
    [[ -n ${key[Down]}      ]] && bindkey "${key[Down]}"      down-line-or-search
    [[ -n ${key[Home]}      ]] && bindkey "${key[Home]}"      beginning-of-line
    [[ -n ${key[End]}       ]] && bindkey "${key[End]}"       end-of-line
    [[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
    [[ -n ${key[Delete]}    ]] && bindkey "${key[Delete]}"    delete-char
fi

# Map variants of Home to '^[[H', and variants of End to '^[[F'
for mode in vicmd viins viopp; do
    bindkey -M $mode -s '^[[1~' '^[[H' '^[[7~' '^[[H' '^[OH' '^[[H' \
                        '^[[2~' '^[[F' '^[[8~' '^[[F' '^[OF' '^[[F'
    bindkey -M $mode '^[[H' vi-beginning-of-line '^[[F' vi-end-of-line
done

bindkey '^W' backward-kill-word-match
#bindkey . rationalise-dot
# without this, typing a . aborts incremental history search
#bindkey -M isearch . self-insert

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

# Quickly search through my snippet collection
bindkey -M viins '^X^S' pet-select

# Press Alt-S to rapidly prepend sudo to the beginning of the line
function insert-sudo() {
    zle beginning-of-line
    zle -U "sudo "
}
zle -N insert-sudo
bindkey "^[s" insert-sudo

bindkey -M vicmd 'ZE' fuzzy-edit
bindkey -M viins '^O' fuzzy-edit

# Load some optional but handy zle widgets that ship with Zsh itself
autoload -Uz select-quoted select-bracketed split-shell-arguments surround

zle -N select-quoted
zle -N select-bracketed
for mode in vicmd viopp; do
	for seq in {a,i}{\',\",\`}; do
		bindkey -M "$mode" "$seq" select-quoted
	done
	for seq in {a,i}${(s..)^:-'()[]{}<>bB'}; do
		bindkey -M "$mode" "$seq" select-bracketed
	done
done

zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround

bindkey -a cs change-surround
bindkey -a ds delete-surround
bindkey -a ys add-surround

source ${DOTSDIR}/zsh/plugins/vi-more-increment/vi-increment.zsh
source ${DOTSDIR}/zsh/plugins/vi-more-quote/vi-quote.zsh
