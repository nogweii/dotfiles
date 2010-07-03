setopt interactivecomments
#setopt Share_History
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -v # Vim mode!

# Paste a URL? Now auto quoted, yay! :D
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

autoload -U backward-kill-word-match
zle -N backward-kill-word-match
bindkey '^W' backward-kill-word-match
zstyle ':zle:*' word-style normal
zstyle ':zle:*' word-chars ''

# Ignore ^D when we're on an empty line, therefore, I have to type "exit" or
# "logout" to kill a zsh session
setopt IGNORE_EOF

zmodload zsh/terminfo zsh/zselect zsh/system
