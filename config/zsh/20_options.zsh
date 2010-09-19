setopt   interactive_comments # Allow comments in interactive mode
setopt   append_history       # Append, don't overwrite, the history file
setopt   ignore_eof           # Ignore ^D to exit zsh
setopt   auto_cd              # Just type in the directory, no 'cd' needed!
setopt   extended_glob        # Advanced shell globs
unsetopt beep                 # I hate beeps
setopt   hist_verify          # Expand history expansions instead of executing
setopt   hist_ignore_dups     # Don't record duplicate (contiguous) commands
setopt   hist_ignore_space    # Don't record commands that begin with a space
setopt   hist_reduce_blanks   # Remove superfluous blanks before recording
setopt   hist_no_store        # Don't store invocations of `history`
setopt   extended_history     # Save time related information to history
#setopt   auto_pushd           # cd pushes the directory to the stack
setopt   pushd_ignore_dups    # ignore duplicate directories
setopt   pushd_silent         # don't output the directory

# Paste a URL? Now auto quoted, yay! :D
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Smarter version of ^W, stopping at punctuation
autoload -U backward-kill-word-match
zle -N backward-kill-word-match
zstyle ':zle:*' word-style normal
zstyle ':zle:*' word-chars ''

# Load a bunch of modules
zmodload zsh/terminfo zsh/zselect zsh/system
