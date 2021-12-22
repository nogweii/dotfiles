#!/bin/zsh

autoload -Uz add-zsh-hook
autoload colors zsh/terminfo

# A simple zsh-based bookmarks system (kinda)
name-dir() {
  eval "hash -d ${1}=$(pwd)"
  echo "hash -d ${1}=$(pwd)" >> ${ZHOME}/named_dirs
  echo "~${1}"
}

alias ruby=_ruby_or_irb
alias irb=_ruby_or_irb

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
  regexp='(missing|error|fail|\s(not|no .+) (found|present)|(not? |in)valid|fatal|conflict|problem|critical|corrupt|warning|wrong|illegal|segfault|\sfault|caused|\sunable|\((E|W){2}\)|BUG|panic|exception|negative|eek!|den(y|ied))'

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

local function_file
for function_file in ${DOTSDIR}/zsh/functions/* ; do
  autoload ${function_file:t}
done

function df() {
  # Is dfc installed & did I not pass any arguments to df? 
  if [ -n "${commands[dfc]}" -a $# -eq 0 ]; then
    # '-TbW' means to show the type not the graph, and don't truncate
    command dfc -TbW -q name
  elif [ $# -eq 0 ]; then
    # dfc isn't installed, but I did not pass any parameters
    command df -hT
  else
    # Just run df like normal, passing along any parameters I specified
    command df $@
  fi
}

# A smarter(ish) g alias: By default, 'g' alone will show the status. 'g blah'
# still works.
function g {
  if [[ $# > 0 ]]; then
    if [[ "$@" = "st" ]]; then
      echo "\e[33mstop doing that! just use 'g'"
    else
      git "$@"
    fi
  else
    git status --short --branch
  fi
}

# Get the environment variables for a particular process ID.
function pidenv() {
  local env_path="/proc/${1:-self}/environ"
  if [[ ! -f $env_path ]] ; then
    echo "pidenv: No such process id ${1:-self}"
    return 2
  fi

  # I don't think this qualifies for the "Useless Use of Cat" award since I'm
  # trying to read a file that the current user may or may not have permission
  # to read.
  local cat_cmd
  if [[ -r $env_path ]] ; then
    cat_cmd="cat"
  else
    cat_cmd="sudo cat"
  fi

  eval "${cat_cmd} ${env_path} | xargs -n 1 -0 | sort"
}

# Get the time the process was started. (This works by investigating the
# timestamp for the per-process directory in /proc.)
function pidstarted() {
  local pid_path="/proc/${1:-self}"
  if [[ ! -d $pid_path ]] ; then
    echo "pidstarted: No such process id ${1:-self}"
    return 2
  fi

  date --date="@$(stat -c '%Z' ${pid_path})" +'%d %b %Y, %H:%M'
}

function vg-ssh() {
  # List all vagrant boxes available in the system including its status, and
  # try to access the selected one via ssh
  local target_machine=($(jq -rS '.machines[] | {name, vagrantfile_path, state} | .name + " " + .state + " " +.vagrantfile_path' ~/.vagrant.d/data/machine-index/index | column -t | fzf))
  [ $? -gt 0 ] && return 130
  _quiet_cd=1; cd ${target_machine[3]}
  vagrant ssh ${target_machine[1]}
}

# A project-aware vim command. If we're within a project context (initiated by
# prj) then use the main editor. Otherwise, vim as normal.
function vim() {
  if [ -n "${PRJ_PROJECT}" ]; then
    command vim --servername "${PRJ_PROJECT}" --remote $@
    tmux select-window -t editor # focus vim
  else
    command vim $@
  fi
}

function psg {
  ps wwup $(pgrep -f $1) 2>&-
}

function ssh-agent-kill {
  ssh-agent -D
  kill $SSH_AGENT_PID
  unset SSH_AGENT_PID SSH_AUTH_SOCK
}

function ssh-control-kill {
  # don't complain about empty glob matches
  setopt null_glob
  unsetopt nomatch

  for c in ~/.ssh/control-*(=) $ANSIBLE_SSH_CONTROL_PATH_DIR/*(=); do
    echo "Killing $c"
    ssh -oControlPath=$c -O exit localhost
    sleep 0.5 # give a chance for the background ssh process to exit
    [ -e $c ] && rm -v $c
  done
}

function pet-prev() {
  echo "Saving previous command as a snippet"
  PREV=$(fc -lrn | head -n 1)
  sh -c "pet new `printf %q "$PREV"`"
}

function pet-select() {
  BUFFER=$(pet search --query "$LBUFFER")
  CURSOR=$#BUFFER
  zle redisplay
}
zle -N pet-select

function tls-info() {
   echo | openssl s_client -servername "${1}" -connect "${1}":"${2:-443}" 2>/dev/null | openssl x509 -text -noout
}

# a (slightly) smarter cat command, that uses bat when available
function cat() {
  if [ -n "${commands[bat]}" ]; then
    if [[ $# == 1 && -t 0 ]]; then
      # only use bat in the simplest case: a single file passed in 
      # an interactive session
      bat --paging=never --wrap=never --color=always --italic-text=always --decorations=always --terminal-width=$COLUMNS $1 | less
    else
      command cat $@
    fi
  else
    command cat $@
  fi
}

# a (slightly) smarter less command, that uses bat when available
function less() {
  if [ -n "${commands[bat]}" ]; then
    if [[ $# == 1 && -t 0 ]]; then
      # only use bat in the simplest case: a single file passed in 
      # an interactive session
      bat --paging=never --wrap=never --color=always --italic-text=always --decorations=always --terminal-width=$COLUMNS $1 | less
    else
      command less $@
    fi
  else
    command less $@
  fi
}

function nvim-packs() {
  # Check if packer.nvim has been installed yet
  local packer_directory="${XDG_DATA_HOME}/nvim/site/pack/packer/start/packer.nvim"

  if [ ! -d ${packer_directory} ]; then
    echo 'Cloning Neovim package manager packer.nvim...'
    git clone -q "https://github.com/wbthomason/packer.nvim" "${packer_directory}"
  fi

  # Install the plugins configured
  nvim -u NONE \
    +'runtime plugin/rplugin.vim' \
    +'set termguicolors' \
    +'lua require("me.plugins")' \
    +'lua require("packer").sync()'
}
