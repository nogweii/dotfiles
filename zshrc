# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

ZSH="${HOME}/.config/zsh"
source $ZSH/environment.zsh
 source $ZSH/locale.zsh
 source $ZSH/xdg.zsh
source $ZSH/style
source $ZSH/aliases.zsh
 source $ZSH/pacman.zsh
source $ZSH/functions.zsh
#source $ZSH/keychain
if [ -e $ZSH/named_dirs ] ; then
    source $ZSH/named_dirs
fi

setopt interactivecomments
#setopt Share_History
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -v # Vim mode!

# Paste a URL? Now auto quoted, yay! :D
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Run fortune only if it's installed and we aren't connected to the machine via ssh
if [ -x "$(which fortune 2>&1)" -a -z "$SSH_CONNECTION" ] ; then
    fortune -s # "Short" apothegms only
fi
# MAILDIR
test -e $HOME/mail && export MAILDIR=$HOME/mail && for i in $(echo $MAILDIR/**/cur(:h)); do mailpath[$#mailpath+1]="${i}?You have new mail in ${i:t}."; done

# Create my git configuration unless it's already up-to-date.
ztmpl ~/.gitconfig

autoload -U backward-kill-word-match
zle -N backward-kill-word-match
bindkey '^W' backward-kill-word-match
zstyle ':zle:*' word-style normal
zstyle ':zle:*' word-chars ''

#function autobg() {
#    jobs -s >| /tmp/j$$
#    while read jnum jdesc
#    do
#        bg %${${jnum#\[}%\]}
#    done < /tmp/j$$
#    \rm -f /tmp/j$$
#}
#
#function precmd() {
#    autobg
#}

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
    RPS2=$RPS1

    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

keychain --quiet # Start ssh-agent, but don't add any keys (yet)
[ -f $HOME/.keychain/${HOSTNAME}-sh ] && source $HOME/.keychain/${HOSTNAME}-sh # Load ssh-agent environment variables
[ -f $HOME/.keychain/${HOSTNAME}-sh-gpg ] && source $HOME/.keychain/${HOSTNAME}-sh-gpg # ditto, gpg-agent

# Ignore ^D when we're on an empty line, therefore, I have to type "exit" or
# "logout" to kill a zsh session
setopt IGNORE_EOF

export PATH="/usr/lib/cw:$PATH"
export NOCOLOR_PIPE=1

zmodload zsh/terminfo zsh/zselect zsh/system
# colorize stderr
color_err () {
    ## sysread & syswrite are part of zsh/system
    while sysread 'std_err'
    do
      syswrite -o 2 -- "${fg_bold[red]}${std_err}${terminfo[sgr0]}"
    done
}
exec 2> >( color_err )

[[ ! -d ~/.data/zsh ]] && mkdir ~/.data/zsh
fpath=(~/.data/zsh $fpath)

function smart_cd () {
  if [[ -f $1 ]] ; then
    [[ ! -e ${1:h} ]] && return 1
    builtin cd ${1:h}
  else
    builtin cd ${1}
  fi
}

function cd () {
  setopt localoptions
  setopt extendedglob
  local approx1 ; approx1=()
  local approx2 ; approx2=()
  if (( ${#*} == 0 )) || [[ ${1} = [+-]* ]] ; then
    builtin cd "$@"
  elif (( ${#*} == 1 )) ; then
    approx1=( (#a1)${1}(N) )
    approx2=( (#a2)${1}(N) )
    if [[ -e ${1} ]] ; then
      smart_cd ${1}
    elif [[ ${#approx1} -eq 1 ]] ; then
      smart_cd ${approx1[1]}
    elif [[ ${#approx2} -eq 1 ]] ; then
      smart_cd ${approx2[1]}
    else
      print couldn\'t correct ${1}
    fi
  elif (( ${#*} == 2 )) ; then
    builtin cd $1 $2
  else
    print cd: too many arguments
  fi
}

extract_archive () {
    local lower full_path target_dir
    lower=${(L)1} # Used for matching
    full_path=$(readlink -f $1) # The real path, expanded & absolute
    target_dir=$(echo $1 | sed 's/(\.tar)?\.[^.]*$//') # new directory name
    md $target_dir # mkdir && cd combo
    case "$lower" in
        *.tar.gz) tar xzf "$full_path" ;;
        *.tgz) tar xzf "$full_path" ;;
        *.gz) gunzip "$full_path" ;;
        *.tar.bz2) tar xjf "$full_path" ;;
        *.tbz2) tar xjf "$full_path" ;;
        *.bz2) bunzip2 "$full_path" ;;
        *.tar) tar xf "$full_path" ;;
        *.rar) unrar e "$full_path" ;;
        *.zip) unzip "$full_path" ;;
        *.z) uncompress "$full_path" ;;
        *.7z) 7z x "$full_path" ;;
        *.xz) xz -d "$full_path" ;;
        *.lzma) unlzma -vk "$full_path" ;;
        *.lha) lha e "$full_path" ;;
        *.rpm) rpm2cpio "$full_path" | tar xf - ;;
        *.deb) ar p "$full_path" data.tar.gz | tar zx ;;
        *) print "Unknown archive type: $1" ; return 1 ;;
    esac
    # Change in to the newly created directory, and
    # list the directory contents, if there is one.
    current_dirs=( *(N/) )
    if [[ ${#current_dirs} = 1 ]]; then
        cd $current_dirs[1]
        ls
        break
    fi
}

alias extr=extract_archive
compdef '_files -g "*.tgz *.gz *.tbz2 *.bz2 *.tar *.rar *.zip *.Z *.7z *.xz *.lzma *.lha *.rpm *.deb"' extract_archive

TRAPINT() {
        zle && [[ $HISTNO -eq $HISTCMD ]] && print -sr -- "$PREBUFFER$BUFFER"
        return $1
}

# Give us a root shell, or run the command with sudo.
# Expands command aliases first (cool!)
smart_sudo () {
    if [[ -n $1 ]]; then
        #test if the first parameter is a alias
        if [[ -n $aliases[$1] ]]; then
            #if so, substitute the real command
            sudo ${=aliases[$1]} $argv[2,-1]
        else
            #else just run sudo as is
            sudo $argv
        fi
    else
        #if no parameters were given, then assume we want a root shell
        sudo -s
    fi
}

alias s=smart_sudo
compdef _sudo smart_sudo
