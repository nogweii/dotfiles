zstyle ':compinstall' filename "${XDG_CONFIG_HOME}/zsh/50_style.zsh"
zmodload zsh/complist
autoload -Uz compinit && compinit

# Use a cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME}/zsh/compcache"
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:::::' completer _force_rehash _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
# now, try to complete something and see that a menu appears. Nice, eh?
zstyle ':completion:*' verbose yes

compdef _rake rake
compdef _cheat cheat


# cygwin only: commands that auto-complete with and without .exe or .dll suffixes are annoying.
# thanks Thorsten Kampe & Bart Schaefer (and 'Atom Smasher' for his zshrc)
# http://www.zsh.org/mla/users/2009/threads.html#00391
[[ ${OSTYPE} == cygwin ]] && zstyle ':completion:*:-command-:*' ignored-patterns '(#i)*.exe' '(#i)*.dll'

## if there's a `manpath` command, use it
[[ -x $(whence -p manpath) ]] && export MANPATH=$(manpath 2> /dev/null)

compdef '_files -g "*.tgz *.gz *.tbz2 *.bz2 *.tar *.rar *.zip *.Z *.7z *.xz *.lzma *.lha *.rpm *.deb"' extract_archive
compdef _sudo smart_sudo
