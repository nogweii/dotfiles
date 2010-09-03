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
      rake --silent --tasks | cut -d " " -f 2 > .rake_tasks
    fi
    compadd `cat .rake_tasks`
  fi
}

_cheat () {
  compadd `ls -1 ~/.cheat/ | sed s/\.yml$//`
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

function ztmpl {
    if [[ $1.tmpl -nt $1 || (! -f $1 && -f $1.tmpl) ]] ; then
        local foo="$(print -bP "$(cat $1.tmpl)")"
        echo ${(e)foo} > $1
    fi
}

# Based off http://gist.github.com/172292. Idea by @defunkt
# gist it! http://gist.github.com/172323 (zsh fork)
function ruby_or_irb() {
    if [[ "$1" == "" ]]; then
        irb -f -I ~/.config/irb -r irb_conf
    else
        ruby $@
    fi
}
alias ruby=ruby_or_irb

function detach() {
    # Create the directory where we store the sockets, if it doesn't already exist
    if [[ ! -d "$XDG_CACHE_HOME/dtach/" ]] ; then
        mkdir -p "$XDG_CACHE_HOME/dtach/"
    fi

    # From `dtach --help`:
    #     -A: Attach to the specified socket, or create it if it
    #         does not exist, running the specified command.
    dtach -A "$XDG_CACHE_HOME/dtach/$1.socket" -z -e "" $@
}

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

# This variable dictates weather we are going to do the vcs prompt update
# before printing the next prompt.  On some setups this saves 10s of work.
PR_VCS_UPDATE=1

# called before command execution
# here we decide if we should update the prompt next time
function zsh_vcs_prompt_preexec {
        case "$(history $HISTCMD)" in 
            *vcs*)
                PR_VCS_UPDATE=1
                ;;
        esac
}
preexec_functions+='zsh_vcs_prompt_preexec'

# called after directory change
# we just assume that we have to update vcs prompt
function zsh_vcs_prompt_chpwd {
        PR_VCS_UPDATE=1
}
chpwd_functions+='zsh_vcs_prompt_chpwd'

# called before prompt generation
# if needed, we will update the prompt info
function zsh_vcs_prompt_precmd {
       if [[ -n "$PR_VCS_UPDATE" ]] ; then
               vcs_info 'prompt'
               PR_VCS_UPDATE=
       fi
}
precmd_functions+='zsh_vcs_prompt_precmd'


function nicename() {
  for i in "$@" ; do
    j=$(echo "$i" | tr [[:upper:]] [[:lower:]] | tr ' ' _) && mv "$i" "$j" 
  done
}

function error_log() {
  regexp='.*(missing|error|fail|\s(not|no .+) found|(no |not |in)valid|fatal|conflict|problem|critical|corrupt|warning|wrong|illegal|segfault|\sfault|caused|\sunable|\(EE\)|\(WW\))'
  if [ "x$1" != "x" ] ; then
    echo $regexp
    return 0;
  fi
  s find /var/log/* -type f -regex '[^0-9]+$' -exec grep -Eni $regexp {} \+ | $PAGER
}
