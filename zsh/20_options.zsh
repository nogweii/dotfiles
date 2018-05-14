setopt   always_to_end          # move to the end of the word after a completion
setopt   append_history         # Append, don't overwrite, the history file
setopt   auto_cd                # Just type in the directory, no 'cd' needed!
setopt   auto_name_dirs         # variables that point to a dir are used for %~
setopt   auto_pushd             # cd pushes the new directory to the stack
setopt   auto_resume            # attempt fg a job before creating a new process
unsetopt beep                   # I hate beeps
setopt   bg_nice                # run jobs (foo &) at a lower priority
setopt   brace_ccl              # failed {} expansions are expanded into words
unsetopt case_glob              # globs are not case-sensitive
setopt   cdable_vars            # try expanding '~foo' when 'cd foo' fails
setopt   chase_links            # (implies chase_dots), resolve symlinks on cd
setopt   check_jobs             # warn me when jobs are running on exit
setopt   correct                # if I typo a command name, offer suggestions
setopt   extended_glob          # Advanced shell globs
setopt   extended_history       # Save time related information to history
unsetopt flow_control           # Disable flow control, opening C-s and C-q keys
setopt   hash_cmds              # faster command execution, save a $PATH search
setopt   hist_expire_dups_first # when trimming history, start with duplicates
setopt   hist_find_no_dups      # when doing history searches, ignore duplicates
setopt   hist_ignore_dups       # Don't record duplicate (contiguous) commands
setopt   hist_ignore_space      # Don't record commands that begin with a space
setopt   hist_no_store          # Don't store invocations of `history`
setopt   hist_reduce_blanks     # Remove superfluous blanks before recording
setopt   hist_verify            # Expand history expansions instead of executing
unsetopt ignore_eof             # Ignore ^D to exit zsh
setopt   inc_append_history     # immediately write lines to the history
setopt   interactive_comments   # Allow comments in interactive mode
setopt   pushd_ignore_dups      # ignore duplicate directories
setopt   pushd_silent           # don't output the directory
setopt   rcquotes               # Lazier/smarter quoting rules

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
