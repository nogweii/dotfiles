#!/bin/zsh
# rake autocompletion from:
# http://weblog.rubyonrails.org/2006/3/9/fast-rake-task-completion-for-zsh
_rake_does_task_list_need_generating () {
  if [ ! -f .rake_tasks ]; then return 0;
  else
    accurate=$(stat -c%Y .rake_tasks)
    changed=$(stat -c%Y Rakefile)
    return $(expr $accurate '>=' $changed)
  fi
}

_rake () {
  if [ -f Rakefile ]; then
    if _rake_does_task_list_need_generating; then
      rake --silent --tasks | cut -d' ' -f2 2>/dev/null >.rake_tasks
    fi
    compadd `cat .rake_tasks`
  fi
}

# A simple zsh-based bookmarks system (kinda)
name-dir() {
  eval "hash -d ${1}=$(pwd)"
  echo "hash -d ${1}=$(pwd)" >> ${ZHOME}/named_dirs
  echo "~${1}"
}

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1 # Because we didn't really complete anything
}

# Based off http://gist.github.com/172292. Idea by @defunkt
# gist it! http://gist.github.com/172323 (zsh fork)
function ruby_or_irb() {
    if [[ "$1" == "" ]]; then
        command irb -f -I$XDG_CONFIG_DIR/irb -r irb_conf
    else
        command ruby $@
    fi
}
alias ruby=ruby_or_irb

function detach() {
    # Create the directory where we store the sockets, if it doesn't already
    # exist
    if [[ ! -d "$XDG_CACHE_HOME/dtach/" ]] ; then
        mkdir -p "$XDG_CACHE_HOME/dtach/"
    fi

    # From `dtach --help`:
    #     -A: Attach to the specified socket, or create it if it
    #         does not exist, running the specified command.
    dtach -A "$XDG_CACHE_HOME/dtach/$1.socket" -z -e "" $@
}

function smart_cd () {
  if [[ -f $1 ]] ; then
    [[ ! -e ${1:h} ]] && return 1
    builtin cd ${1:h}
  else
    builtin cd ${1}
  fi
}

function cd () {
  local approx1 ; approx1=()
  local approx2 ; approx2=()
  # No parameters, or the first one begins with a '+' or '-'
  if (( ${#*} == 0 )) || [[ ${1} = [+-]* ]] ; then
    # Are we in a VCS dir? (git only, relies on vcs_info) + no parameters?
    if [[ -n $vcs_info_msg_0_ ]] && (( ${#*} == 0 )); then
      builtin cd $(git rev-parse --show-toplevel)
    else
      builtin cd "$@"
    fi
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
    target_dir=$(echo $1 | sed 's/(\.tar|\.zip)?\.[^.]*$//') # new directory name
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
    fi
}

TRAPINT() {
        zle && [[ $HISTNO -eq $HISTCMD ]] && print -sr -- "$PREBUFFER$BUFFER"
        return $1
}

# Give us a root shell, or run the command with sudo.
# Expands command aliases first (cool!)
smart_sudo () {
    sudo_opts=""
    if [ ! -z "$SUDO_ASKPASS" ]; then
        sudo_opts="-A"
    fi
    if [[ -n $1 ]]; then
        #test if the first parameter is a alias
        if [[ -n $aliases[$1] ]]; then
            #if so, substitute the real command
            sudo $sudo_opts ${=aliases[$1]} $argv[2,-1]
        elif [[ "$1" = "vim" ]] ; then
            # sudo vim -> sudoedit
            sudoedit $sudo_opts $argv[2,-1]
        else
            #else just run sudo as is
            sudo $sudo_opts $argv
        fi
    else
        #if no parameters were given, then assume we want a root shell
        sudo $sudo_opts -s
    fi
}

# colorize stderr
color_err () {
    ## sysread & syswrite are part of zsh/system
    while sysread 'std_err'
    do
      syswrite -o 2 -- "${fg_bold[red]}${std_err}${terminfo[sgr0]}"
    done
}
# Automatically red-ify all stderr output
#exec 2> >( color_err )

function nicename() {
  for i in "$@" ; do
    j=$(echo "$i" | tr [[:upper:]] [[:lower:]] | tr ' ' _) && mv "$i" "$j"
  done
}

function errors() {
  # this monster is an awesome regular expression, yeah? Finds lot of errors in
  # a bunch of log formats!
  regexp='.*(missing|error|fail|\s(not|no .+) found|(no |not |in)valid|fatal|conflict|problem|critical|corrupt|warning|wrong|illegal|segfault|\sfault|caused|\sunable|\(EE\)|\(WW\))'

  # Default case: No parameters passed, so search all of /var/log
  log_path="/var/log"
  if [ -n "$1" ] ; then
    # If the parameter is a file, search only that one
    if [ -f "$1" ] ; then
      log_path="$1"
      smart_sudo find $log_path -type f -regex '[^0-9]+$' -exec grep -Eni $regexp {} \+ | $PAGER
    else
      echo $regexp
      return 0;
    fi
  fi
}

function cawk {
    first="awk '{print "
    last=" \"\"}'"
    cmd="${first}$(echo "$@" | sed "s:[0-9]*:\$&,:g")${last}"
    eval $cmd
}

function title {
  case $TERM in
      *xterm*|rxvt|rxvt-unicode|rxvt-256color|(dt|k|E)term)
        print -Pn "\e]0;$1\a"
      ;;
      screen)
        print -Pn "\ek$1\e\\"
      ;;
  esac
}

for i in ${dot_path}/share/zsh/*(*) ; do
  alias "${i:t}"="unalias '${i:t}'; autoload -U '${i:t}'; ${i:t}"
done

autoload colors zsh/terminfo

# source from, or add to ~/.zshrc. Go opens zsh at the current location, and on
# exit, cd into ranger's last location.
ranger_open () {
     ranger "$(pwd)" <$TTY
     print -n "\033[A"
     zle && zle -I
     cd "$(grep \^\' ~/.config/ranger/bookmarks | cut -b3-)"
     # Redo the prompt, to fit any new data from the new directory
     setprompt
     precmd
     _zsh_highlight-zle-buffer
}
zle -N ranger_open
bindkey -a "go" ranger_open

dh(){ `fc -lnr 0 | perl -nwe 's/\s*\d+\s+//; print unless eof' | dmenu -b` }

df() {
  # Is dfc installed & did I not pass any arguments to df?
  if [ -n "${commands[dfc]}" -a $# -eq 0 ]; then
    # Add 64 so that sed won't delete the ANSI color codes. Why 64? Just guessed
    # 50-60, then kept incrementing until the lines got too long. 64 works
    # perfectly.
    command dfc -a -T -w -W -o -i -f -c always | sed "s/^\(.\{,$(($COLUMNS+64))\}\).*/\\1/"
  else
    # Just run df like normal, passing along any parameters
    command df $@
  fi
}

# just type '...' to get '../..'
rationalise-dot() {
  local MATCH
  if [[ $LBUFFER =~ '(^|/| |	|'$'\n''|\||;|&)\.\.$' ]]; then
    LBUFFER+=/
    zle self-insert
    zle self-insert
  else
    zle self-insert
  fi
}
zle -N rationalise-dot
