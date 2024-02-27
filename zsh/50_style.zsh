zstyle ':compinstall' filename "${XDG_CONFIG_HOME}/zsh/50_style.zsh"
zmodload zsh/complist
autoload -Uz compinit

compsupercache ()
{
  local completion_dump="${XDG_CACHE_HOME}/zsh/compdump"
  # use a separate file to determine when to regenerate, as compinit doesn't
  # always need to modify the compdump
  local last_dump="${completion_dump}.last"

  if [[ -e "${last_dump}" && -f "${last_dump}"(#qN.md-1) ]]; then
    compinit -C -d "${completion_dump}"
  else
    compinit -d "${completion_dump}"
    touch "${last_dump}"
  fi

  # if zcompdump exists (and is non-zero), and is older than the .zwc file, then regenerate
  if [[ -s "${completion_dump}" && (! -s "${completion_dump}.zwc" || "${completion_dump}" -nt "${completion_dump}.zwc") ]]; then
    # since file is mem-mapped, it might be loaded right now (currently running shells), so rename it then make a new one
    [[ -e "${completion_dump}.zwc" ]] && mv -f "${completion_dump}.zwc" "${completion_dump}.zwc.prior"
    # compile it mapped, so multiple shells can share it (total mem reduction), but do so in the background
    zcompile -M "${completion_dump}" &!
  fi
}

compsupercache

# Use a cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME}/zsh/compcache"
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:::::' completer _force_rehash _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX:t+$#SUFFIX:h)/3 )) )'
zstyle ':completion:*' accept-exact-dirs 'yes'
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

compdef g=git

# cygwin only: commands that auto-complete with and without .exe or .dll suffixes are annoying.
# thanks Thorsten Kampe & Bart Schaefer (and 'Atom Smasher' for his zshrc)
# http://www.zsh.org/mla/users/2009/threads.html#00391
[[ ${OSTYPE} == cygwin ]] && zstyle ':completion:*:-command-:*' ignored-patterns '(#i)*.exe' '(#i)*.dll'

compdef '_files -g "*.tgz *.gz *.tbz2 *.bz2 *.tar *.rar *.zip *.Z *.7z *.xz *.lzma *.lha *.rpm *.deb"' extract_archive
compdef '_files -g "*.tgz *.gz *.tbz2 *.bz2 *.tar *.rar *.zip *.Z *.7z *.xz *.lzma *.lha *.rpm *.deb"' xf # A shorter alias for extract_archive
compdef _sudo smart_sudo
compdef '_values "projects" ~/code/*(/:t)' prj

zstyle -e ':completion:*:(ssh|scp|sftp|ssh-copy-id):*' hosts 'reply=(${(s: :)${${${(M)${(f)"$(<~/.ssh/config)"}:#Host*}#Host }:#*\**}} ${${${${(f)"$(<~/.ssh/known_hosts)"}:#[|0-9]*}%%\ *}%%,*} )'

compdef g=git

zstyle ':completion::*:ssh-master-exit:*:*' file-patterns '~/.ssh/master-*(=):all-files'

zstyle ':completion:*:*:task:*' verbose yes
zstyle ':completion:*:*:task:*:descriptions' format '%U%B%d%b%u'

zstyle ':completion:*:*:task:*' group-name ''

alias t=task
compdef _task t=task

# travis completion integration via the gem
[ -f ${TRAVIS_CONFIG_PATH}/travis.sh ] && source ${TRAVIS_CONFIG_PATH}/travis.sh
