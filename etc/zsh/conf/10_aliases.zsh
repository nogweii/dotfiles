########################
# Global aliases
alias -g   H='|head'
alias -g   L='|less'
alias -g NUL='&>/dev/null'
alias -g   T='|tail'
alias -g   G='|grep'
alias -g   N='&>/dev/null'
alias -g   S='|sort'
alias -g  TO='2>&1 3>&1' # '&>&1' returns a parse error
alias -g   U='|uniq'
alias -g  JL='| jq -C . | less'
alias -g  RE='| ruby -ne'

########################
# Listing aliases
if [ -n "${commands[lsd]}" ]; then
    alias ls="${commands[lsd]} --icon=auto --color=auto -F --git --hyperlink=auto --header"
elif [ -n "${commands[eza]}" ]; then
    alias ls="${commands[eza]} --classify=auto --icons=auto --color=auto --hyperlink"
elif [ -n "${commands[gls]}" ]; then
    alias ls="${commands[gls]} --color=auto -F"
else
    alias ls="ls --color=auto -F"
fi
alias    lsa='ls -A'                      # only show dot-files
alias    lsd='ls -d *(/)'                 # only show directories
alias    lsx='ls *(*)'                    # only executables
alias     ll='ls -lAh'
alias    lld='ll -d *(/)'                 # only show directories

########################
# Default aliases
#
# Some coreutils I use every day just seem to have silly defaults. Like mkdir,
# example. Why not support 'this/does/not/exist' and automatically make the
# parent directories?
alias mkdir='mkdir -p'
alias cp='cp -Ri'
alias rm=safedel
alias mv='mv -i'
alias grep="grep $GREP_DEFAULTS" # Only use my preferences in the parent process

alias nrm='yes | builtin command rm -rvf &>/dev/null'

alias makepkg="makepkg -s"

alias addme="s gpasswd -a ${USER}"

alias edit="${EDITOR}"
alias s=`whence -p sudo`

alias git-branch-name="git branch | sed '/^\*/!d; s/^\* //'"

alias vim2html='vim -c "let html_number_lines = 1" -c "let use_xhtml =1 " -c "let html_use_css = 1" -c "TOhtml" -c "set nu" -c "syntax on" -c "wq" -c "q" &>/dev/null'

alias mailcount="find $MAILDIR | grep -P new\/.? | sed 's@.*mail/\(.*\)/new.*@\1@' | awk '{a[\$1]++}END{for(i in a){print a[i] \" \" i}}' | sort -rn"
alias wtf="wtf -f $HOME/.config/acronyms.wtf"

# e will open the editor, whichever that one is
alias e='$EDITOR'

########################
# 'special' character aliases.
alias --  -="cd - &>/dev/null; ls"  # Go to the last directory
alias     .='cd'
alias qcd='_quiet_cd=1 cd'

alias maxup="echo \$(uprecords -a | head -n3 | tail -n1 | sed s/\-\>// | awk '{ print \$2, \$3, \$4 }')" # requires uptimed

if [ -n "${commands[ss]}" ]; then
    alias portstat="command sudo ss -lntup"
elif [ -n "${commands[netstat]}" ]; then
    alias portstat="sudo netstat -tunelp"
else
    alias portstat="echo missing ss and netstat"
fi

alias s=smart-sudo
alias wc='wc -l'
alias xf=extract_archive
alias list-dbus="qdbus | sed '/^:/d;s/ //' | sort"

# A quick utility that generates a nice, long password
alias gen-sha-pwd='head -c64 /dev/random | sha256sum'

# Lazy open command
if [ "${XDG_SESSION_DESKTOP:l}" = "kde" ]; then
    alias open='kde-open5 2>/dev/null'
fi

if [ -x "${commands[systemctl]}" ]; then
    alias reboot='sudo systemctl reboot'
    alias shutdown='sudo systemctl poweroff'
    alias suspend='sudo systemctl suspend'
fi

alias be='bundle exec '
alias ber='bundle exec rake'

alias pwgen='pwgen -cny 36 1'

alias aursearch='cower -cauto -s'
alias aurinfo='cower -cauto -i'

if [ -n "${commands[rg]}" ]; then
    alias gr=rg
elif [ -n "${commands[ag]}" ]; then
    alias gr=ag
elif [ -n "${commands[ack]}" ]; then
    alias gr=ack
else
    alias gr='grep -r'
fi

# npm's search is absolutely atrocious, it loads the entire index of ~300k
# packages into memory
alias npm8gb='node --max-old-space-size=8192 /usr/bin/npm'

alias nvim-startup-benchmark='nvim "+StartupTime"'

if [ -n "${commands[nvim]}" ]; then
    alias vi=nvim
    alias vim=nvim
fi

alias qrencode-text='qrencode -t ANSIUTF8 -m2 -lm'

if [ -n "${commands[ctop]}" -a "${TERM}" = "alacritty" ]; then
    alias ctop="TERM=xterm-256color ctop"
fi

# If plocate is installed and there isn't anything providing regular locate, alias it
if [ -n "${commands[plocate]}" -a -z "${commands[locate]}" ]; then
    alias locate=plocate
fi

alias ytdl.ogg="yt-dlp -x --audio-format=vorbis --audio-quality=0 --embed-thumbnail --xattrs --embed-metadata"

# I do a lot of kubernetes stuff, make executing kubectl easier
if [[ -v commands[kubecolor] ]]; then
    alias k=kubecolor
    alias knodeshell="${commands[kubectl]} node-shell -n kube-system -x"
else
    alias k=kubectl
    alias knodeshell="kubectl node-shell -n kube-system -x"
fi

# Helpful aliases to running the latest version of renovate bot on my laptop
alias renovate-config-validator="npx --package=renovate@latest --yes renovate-config-validator --strict"
alias renovate-local="LOG_LEVEL=debug FORCE_COLOR=3 npx --package=renovate@latest --yes renovate --platform=local 2>&1 | less"
alias renovate-extract='podman run -it --pull always --rm --volume "${PWD}:/usr/src/app" renovate/renovate:latest bash -c "git config --global --add safe.directory /usr/src/app && renovate --platform=local--onboarding=false --dry-run=extract"'

alias compcachereset="command rm --interactive=never ${XDG_CACHE_HOME}/zsh/compdump*; compsupercache"

alias dc=docker-compose

alias adb='HOME="$XDG_DATA_HOME"/android adb'
alias wget="wget --hsts-file='$XDG_DATA_HOME/wget-hsts'"

alias zshrc-local='$EDITOR $ZDOTDIR/local.zsh'
alias nvimrc='$EDITOR $XDG_CONFIG_HOME/nvim/lua/me/'

[[ -r /etc/grc.zsh ]] && source /etc/grc.zsh

alias glab-vars-to-env='glab variable export -F env'
