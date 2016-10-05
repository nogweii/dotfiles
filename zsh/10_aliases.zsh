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

########################
# Listing aliases
alias      ls='ls --color=auto -F'
alias     lsa='ls -A'                      # only show dot-files
alias     lsw='ls -d *(R,W,X.^ND/)'        # world-{readable,writable,executable} files
alias     lsd='ls -d *(/)'                 # only show directories
alias     lse='ls -d *(/^F)'               # only show empty directories
alias     lss='ls *(s,S,t)'                # only files with setgid/setuid/sticky
alias     lsl='ls *(@)'                    # only symlinks
alias     lsx='ls *(*)'                    # only executables
alias   lsnew="ls -rl *(D.om[1,10])"       # display the newest files
alias   lsold="ls -rtlh *(D.om[1,10])"     # display the oldest files
alias   lsbig="ls -flh *(.OL[1,10])"       # display the biggest files
alias lssmall="ls -Srl *(.oL[1,10])"       # display the smallest files
# `ll' series of listing aliases: More detailed, and more verbose.
alias      ll='ls -lAh'
alias     llw='ll -d *(R,W,X.^ND/)'        # world-{readable,writable,executable} files
alias     lld='ll -d *(/)'                 # only show directories
alias     lle='ll -d *(/^F)'               # only show empty directories
alias     lls='ll *(s,S,t)'                # only files with setgid/setuid/sticky
alias     lll='ll *(@)'                    # only symlinks
alias     llx='ll *(*)'                    # only executables


########################
# Default aliases
#
# Some coreutils I use every day just seem to have silly defaults. Like mkdir,
# example. Why not support 'this/does/not/exist' and automatically make the
# parent directories?
alias mkdir='mkdir -p'
alias cp='cp -Ri'
alias rm='rm -rv --one-file-system --no-preserve-root'
alias mv='mv -i'
alias grep="grep $GREP_DEFAULTS" # Only use my preferences in the parent process

########################
# Convenience aliases
alias nhclean='rm ./**/nohup.out'
alias swpclean='rm $(find G "\/\..*\.swp$")'
alias yrm='yes | rm -rvf'
alias nrm='yrm &>/dev/null'
alias startxl="startx &>/tmp/startx.log"

alias PS="ps wwaux"
alias psg="PS G -v grep G"

alias makepkg="makepkg -s"

alias addme='s gpasswd -a colin'

alias edit="${EDITOR}"
alias s=`whence -p sudo`

alias git-branch-name="git branch | sed '/^\*/!d; s/^\* //'"

alias vim2html='vim -c "let html_number_lines = 1" -c "let use_xhtml =1 " -c "let html_use_css = 1" -c "TOhtml" -c "set nu" -c "syntax on" -c "wq" -c "q" &>/dev/null'

alias mailcount="find $MAILDIR | grep -P new\/.? | sed 's@.*mail/\(.*\)/new.*@\1@' | awk '{a[\$1]++}END{for(i in a){print a[i] \" \" i}}' | sort -rn"
alias wtf="wtf -f $HOME/.config/acronyms.wtf"

########################
# 'special' character aliases.
alias --  -="cd - &>/dev/null; ls"  # Go to the last directory
alias     .='cd'

alias maxup="echo \$(uprecords -a | head -n3 | tail -n1 | sed s/\-\>// | awk '{ print \$2, \$3, \$4 }')" # requires uptimed

alias portstat="s netstat -tunelp"

alias gh-pages='git symbolic-ref HEAD refs/heads/gh-pages && rm .git/index && git clean -fdx'
if [ -n "${commands[hub]}" ]; then
    alias git=hub
fi

alias s=smart_sudo
alias extr=extract_archive
# sudo the previous command
alias ss="s \$(fc -l \$[ \$(print -P '%\!') - 1 ] | cut -d' ' -f3-)"

alias which='alias | /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'
alias wc='wc -l'
alias xf=extract_archive
alias list-dbus="qdbus | sed '/^:/d;s/ //' | sort"
if [ -n "${commands[mvn]}" ]; then
    alias maven="mvn"
    alias maven_notest="mvn -Dmaven.test.skip=true"
fi

# A quick utility that generates a nice, long password. Pulls from /dev/random,
# not urandom, so make sure there's plenty of entropy!
alias gen-sha-pwd='head -c512 /dev/random | sha512sum'

# Lazy open command
if [ "${XDG_SESSION_DESKTOP}" = "gnome" ]; then
    alias open=gvfs-open
elif [ "${XDG_SESSION_DESKTOP}" = "kde" ]; then
    alias open=kde-open
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

if [ -n "${commands[ag]}" ]; then
    alias gr=ag
elif [ -n "${commands[ack]}" ]; then
    alias gr=ack
else
    alias gr='grep -r'
fi

# npm's search is absolutely atrocious, it loads the entire index of ~300k
# packages into memory
alias npm8gb='node --max-old-space-size=8192 /usr/bin/npm'
