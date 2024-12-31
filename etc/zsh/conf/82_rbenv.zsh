# Configure rbenv. This is similar to the output of rbenv init, though a bit
# more neckbeard-y so that I can move the root directory. (Avoiding yet another
# dotfile in $HOME)

# Early return to skip sourcing the rest of the file if rbenv isn't installed
if [[ -z $commands[rbenv] ]]; then
   return
fi

export RBENV_SHELL=zsh

# Get the directory rbenv is in (e.g. installed to /usr/local/rbenv/? The full
# path to the rbenv command is /usr/local/rbenv/bin/rbenv and I want just just
# '/usr/local/rbenv')
local rbenv_parent_path=${${:-$commands[rbenv]:A/../../}:A}

# If the parent path is writable by me, use that. (For when rbenv is installed
# in my home directory)
if [[ -w $rbenv_parent_path ]]; then
  export RBENV_ROOT="${rbenv_parent_path}"
else
  # Failing that, use $XDG_DATA_HOME
  export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"
fi

path=("${RBENV_ROOT}/shims" $path)

rbenv() {
  local command
  command="${1:-}"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  rehash|shell)
    eval "$(rbenv "sh-$command" "$@")";;
  *)
    command rbenv "$command" "$@";;
  esac
}

# The completions are shipped in the same parent directory as the command
source "${rbenv_parent_path}/completions/_rbenv"
compdef _rbenv rbenv

# Don't need this variable any more
unset rbenv_parent_path
