#!/bin/zsh
# cd && ls
cl()  {
    cd $1 && lsa
}

# mkdir && cd
mcd() {
    mkdir -p "$1"
    cd "$1"
}

# Display path in titlebar of terms.
# Automagical function called every time there is a path change (that I called, not within 
# another script)
chpwd() {
    [[ -t 1 ]] || return 0
    case $TERM in
        *xterm*|*rxvt*|(dt|k|E)term)
            print -Pn "\e]2;%~@$(hostname)\a"
        ;;
    esac
}

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
function ruby() { [[ "$1" == "" ]] && `whence -p irb` || `whence -p ruby` $@ }

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

function d() {
    mkdir -p $1 ; cd $1
}
