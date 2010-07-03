setopt   interactive_comments # Allow comments in interactive mode
setopt   share_history        # Share the history, live, between instances
setopt   append_history       # Append, don't overwrite, the history file
setopt   notify               # Tell me when a job exits
setopt   ignore_eof           # Ignore ^D to exit zsh
setopt   auto_cd              # Just type in the directory, no 'cd' needed!
setopt   extended_glob        # Advanced shell globs
unsetopt beep                 # I hate beeps

# Paste a URL? Now auto quoted, yay! :D
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Smarter version of ^W, stopping at punctuation
autoload -U backward-kill-word-match
zle -N backward-kill-word-match
bindkey '^W' backward-kill-word-match
zstyle ':zle:*' word-style normal
zstyle ':zle:*' word-chars ''

# Load a bunch of modules
zmodload zsh/terminfo zsh/zselect zsh/system
bindkey -v # Vim mode!
