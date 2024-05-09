zstyle ':compinstall' filename "${XDG_CONFIG_HOME}/zsh/50_style.zsh"
autoload -Uz compinit

# Make it possible to use completion specifications and functions written for bash.
autoload -Uz bashcompinit
bashcompinit

# Use a cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME}/zsh/compcache"
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
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

# When completing commands that edit/view files, don't complete zwc files
local -aU viewfile_cmds=(
  vi vim nvim emacs nano gedit code kak kate mcedit joe $EDITOR $VISUAL
  bat cat less more $PAGER)
zstyle ':completion:*:*:('${(j:|:)viewfile_cmds}'):*:*' ignored-patterns '*.zwc'

compdef _rake rake
compdef _cheat cheat

compdef g=git

# cygwin only: commands that auto-complete with and without .exe or .dll suffixes are annoying.
# thanks Thorsten Kampe & Bart Schaefer (and 'Atom Smasher' for his zshrc)
# http://www.zsh.org/mla/users/2009/threads.html#00391
[[ ${OSTYPE} == cygwin ]] && zstyle ':completion:*:-command-:*' ignored-patterns '(#i)*.exe' '(#i)*.dll'

compdef '_files -g "*.tgz *.gz *.tbz2 *.bz2 *.tar *.rar *.zip *.Z *.7z *.xz *.lzma *.lha *.rpm *.deb"' extract_archive
compdef '_files -g "*.tgz *.gz *.tbz2 *.bz2 *.tar *.rar *.zip *.Z *.7z *.xz *.lzma *.lha *.rpm *.deb"' xf # A shorter alias for extract_archive
compdef _sudo smart-sudo
compdef '_values "projects" ~/code/*(/:t)' prj

compdef g=git

zstyle ':completion::*:ssh-master-exit:*:*' file-patterns '~/.ssh/master-*(=):all-files'

zstyle ':completion:*:*:task:*' verbose yes
zstyle ':completion:*:*:task:*:descriptions' format '%U%B%d%b%u'

zstyle ':completion:*:*:task:*' group-name ''

alias t=task
compdef _task t=task

# Don't complete uninteresting users...
zstyle ':completion:*:users' ignored-patterns \
  '_*' adm amanda apache avahi beaglidx bin cacti canna clamav daemon dbus \
  distcache dovecot fax ftp games gdm gkrellmd gopher hacluster haldaemon halt \
  hsqldb ident junkbust ldap lp mail mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody 'nixbld*' nobody nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd rpc rpcuser \
  rpm shutdown squid sshd sync 'systemd-*' uucp vcsa xfs

# ...unless I really want to see one of them.
zstyle '*' single-ignored show
